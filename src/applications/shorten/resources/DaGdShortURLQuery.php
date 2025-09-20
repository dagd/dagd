<?php

/**
 * Queries about shorturls, for reading and writing them.
 *
 * This is all kinds of queries, including DB queries, blacklist queries,
 * routemap queries, etc. There is probably a better abstraction to use here.
 */
final class DaGdShortURLQuery {
  private $controller;

  public function __construct($controller) {
    $this->controller = $controller;
  }

  /**
   * @return DaGdShortURL or null
   */
  public function fromShort($short_url, $include_disabled = false) {
    $id = null;
    $long_url = null;
    $owner_ip = null;
    $enabled = null;
    $query_str = 'SELECT id, longurl, owner_ip, enabled FROM shorturls WHERE ';
    $query_str .= 'shorturl=?';

    if (!$include_disabled) {
      $query_str .= ' AND enabled=1';
    }

    $query = $this
      ->controller
      ->getReadDB()
      ->prepare($query_str);
    $query->bind_param('s', $short_url);
    $start = microtime(true);
    $query->execute();
    $end = microtime(true);
    statsd_time('query_time_getLongURL', ($end - $start) * 1000);
    $query->bind_result($id, $long_url, $owner_ip, $enabled);
    $query->fetch();
    $query->close();

    if (!empty($id) && !empty($long_url) && !empty($owner_ip)) {
      return new DaGdShortURL($id, $short_url, $long_url, $owner_ip, $enabled);
    }

    return null;
  }

  public function fromShortCached($short_url, $include_disabled = false) {
    $ttl = DaGdConfig::get('shorten.cache_ttl');
    if ($ttl <= 0) {
      return $this->fromShort($short_url, $include_disabled);
    }

    $cache = $this->controller->cache();
    if (!$cache) {
      return $this->fromShort($short_url, $include_disabled);
    }

    $key = 'shorturl:'.$short_url.':'.(int)$include_disabled;
    $callback = new DaGdShortURLCacheMiss(
      $this,
      $short_url,
      $include_disabled);
    return $cache->getOrStore($key, $callback, $ttl);
  }

  public function clearFromCache($shorturl) {
    $cache = $this->controller->cache();
    if ($cache) {
      $cache->delete('shorturl:'.$shorturl.':0');
      $cache->delete('shorturl:'.$shorturl.':1');
    }
  }

  /**
   * Hashes a given long URL and tries to match the hash to an existing row
   * in the shorturls table.
   *
   * @return DaGdShortURL or null
   */
  public function fromLongByHash($long_url) {
    $id = null;
    $short_url = null;
    $owner_ip = null;
    $longurl_hash = hash('sha256', $long_url);

    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT id, shorturl, owner_ip FROM shorturls WHERE longurl_hash=? '.
        'AND enabled=1 AND custom_shorturl=0 ORDER BY id DESC LIMIT 1');
    $query->bind_param('s', $longurl_hash);
    $start = microtime(true);
    $query->execute();
    $end = microtime(true);
    statsd_time('query_time_getNonCustomShortURL', ($end - $start) * 1000);
    $query->bind_result($id, $short_url, $owner_ip);
    $query->fetch();
    $query->close();

    if (!empty($id) && !empty($short_url) && !empty($owner_ip)) {
      return new DaGdShortURL($id, $short_url, $long_url, $owner_ip, true);
    }

    return null;
  }


  /**
   * @return boolean
   */
  public function isFreeShortURL($short_url) {
    $count = 0;
    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT COUNT(*) FROM shorturls WHERE shorturl=?');
    $query->bind_param('s', $short_url);
    $query->execute();
    $query->bind_result($count);
    $query->fetch();
    $query->close();

    // If we do *NOT* get one above (!), then it is free, and we return true.
    return !(bool)$count;
  }

  // TODO: Maybe pull this out to something a bit more general so other apps can
  // make use of it some day.
  /**
   * @return boolean
   */
  public function isBannedIP($ip) {
    $count = 0;
    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT COUNT(*) FROM blocked_ips WHERE '.
        'inet6_aton(?) between ip_start and ip_end');
    $query->bind_param('s', $ip);
    $query->execute();
    $query->bind_result($count);
    $query->fetch();
    $query->close();

    // Return true if the author is banned, false if not.
    return (bool)$count;
  }

  /**
   * Checks to see if the given shorturl conflicts with routemap entries.
   *
   * @return boolean
   */
  public function isRouteMapConflict($short_url) {
    $routes = DaGdConfig::get('general.routemap');
    foreach ($routes as $route => $metadata) {
      if (strpos($route, 'shorten') !== false) {
        // Ignore Shorten routes since they are effectively
        // wildcard/fallthrough.
        continue;
      }
      $route = substr($route, 1);
      if (preg_match('@^'.$route.'@', $short_url)) {
        return true;
      }
    }
    return false;
  }

  public function isValidShortUrl($short_url) {
    $valid_char_pattern = DaGdConfig::get('shorten.custom_url_regex');
    return preg_match($valid_char_pattern, $short_url);
  }

  /**
   * @return boolean
   */
  public function isBlacklisted($long_url, $is_create = false) {
    $request = $this->controller->getRequest();
    $referral = $request->getHeader('referer');

    return id(new Blacklist($long_url))
      ->setIsCreate($is_create)
      ->setReferral($referral)
      ->setCache($this->controller->cache())
      ->check();
  }

  /**
   * @return boolean
   */
  public function isWhitelisted($long_url) {
    $whitelist_strings = DaGdConfig::get('shorten.longurl_whitelist_strings');
    foreach ($whitelist_strings as $string) {
      if (strpos($long_url, $string) !== false) {
        statsd_bump('shorturl_whitelisted_string');
        statsd_bump('shorturl_whitelisted');
        return true;
      }
    }

    $whitelist_regexes = DaGdConfig::get('shorten.longurl_whitelist');
    foreach ($whitelist_regexes as $regex) {
      if (preg_match('#'.$regex.'#i', $this->getLongUrl())) {
        statsd_bump('shorturl_whitelisted_regex');
        statsd_bump('shorturl_whitelisted');
        return true;
      }
    }
    return false;
  }

  /**
   * Store the URL. Importantly: does NOT run any blacklist logic, rather leaves
   * that for the caller to do.
   *
   * @return DaGdShortURL
   * @throw DaGdShortenStoreException
   */
  public function store($owner_ip, $short_url, $long_url, $custom_shorturl) {
    $long_url_hash = hash('sha256', $long_url);

    $query = $this->controller->getWriteDB()->prepare(
      'INSERT INTO shorturls(shorturl, longurl, owner_ip, custom_shorturl, '.
      'longurl_hash) VALUES(?, ?, ?, ?, ?);');

    $query->bind_param(
      'sssis',
      $short_url,
      $long_url,
      $owner_ip,
      $custom_shorturl,
      $long_url_hash);

    $start = microtime(true);
    $res = $query->execute();
    $end = microtime(true);
    $id = $this->controller->getWriteDB()->insert_id;
    statsd_time('query_time_store_shorturl_insert', ($end - $start) * 1000);

    if ($res) {
      statsd_bump('shorturl_store');
      return new DaGdShortURL($id, $short_url, $long_url, $owner_ip, true);
    } else {
      statsd_bump('shorturl_store_fail');
      throw new DaGdShortenStoreException();
    }
  }

  /**
   * Enables a short URL, given the shorturl.
   *
   * Requires UPDATE privileges on the `shorturls` table.
   *
   * @return true if changed, false if not.
   */
  public function enable($shorturl) {
    $query = $this->controller->getWriteDB()->prepare(
      'UPDATE shorturls SET enabled=1 WHERE shorturl=?');
    $query->bind_param('s', $shorturl);
    $query->execute();
    $affected = $this->controller->getWriteDB()->affected_rows;
    if ($affected !== 0) {
      $this->clearFromCache($shorturl);
      return true;
    } else {
      return false;
    }
  }

  /**
   * Disables a short URL, given the shorturl.
   *
   * Requires UPDATE privileges on the `shorturls` table.
   *
   * @return true if changed, false if not.
   */
  public function disable($shorturl) {
    $query = $this->controller->getWriteDB()->prepare(
      'UPDATE shorturls SET enabled=0 WHERE shorturl=?');
    $query->bind_param('s', $shorturl);
    $query->execute();
    $affected = $this->controller->getWriteDB()->affected_rows;
    if ($affected !== 0) {
      $this->clearFromCache($shorturl);
      return true;
    } else {
      return false;
    }
  }

  /**
   * Disable all shorturls whose owner_ip is the same as the given shorturl.
   *
   * Requires UPDATE privileges on the `shorturls` table.
   *
   * @return The number of affected rows.
   */
  public function disableIp($shorturl) {
    $surl = $this->fromShort($shorturl, true);
    if (!$surl) {
      return null;
    }
    $owner_ip = $surl->getOwnerIp();
    $query = $this->controller->getWriteDB()->prepare(
      'update shorturls set enabled=0 where owner_ip=? and enabled=1');
    $query->bind_param('s', $owner_ip);
    $query->execute();
    $affected = $this->controller->getWriteDB()->affected_rows;
    return $affected;
  }

  /**
   * Prevent new shorturls if owned by the owner of the given shorturl.
   *
   * Requires INSERT privileges on the `blocked_ips` table.
   *
   * @return true if the insert was successful, false if not.
   */
  public function banIp($shorturl) {
    $surl = $this->fromShort($shorturl, true);
    if (!$surl) {
      return null;
    }
    $owner_ip = $surl->getOwnerIp();
    $query = $this->controller->getWriteDB()->prepare(
      'insert into blocked_ips(ip_start, ip_end) '.
      'values(inet6_aton(?), inet6_aton(?))');
    $query->bind_param('ss', $owner_ip, $owner_ip);
    return $query->execute();
  }

  /**
   * Allow new shorturls if owned by the owner of the given shorturl.
   *
   * Requires DELETE privileges on the `blocked_ips` table.
   *
   * @return true if the deletion was successful, false if not.
   */
  public function unbanIp($shorturl) {
    $surl = $this->fromShort($shorturl, true);
    if (!$surl) {
      return null;
    }
    $owner_ip = $surl->getOwnerIp();
    $query = $this->controller->getWriteDB()->prepare(
      'delete from blocked_ips where '.
      'ip_start=inet6_aton(?) and ip_end=inet6_aton(?)');
    $query->bind_param('ss', $owner_ip, $owner_ip);
    return $query->execute();
  }

  /**
   * Grab daily access counts for a given short URL. Returns an array where
   * keys are dates of the format yyyy-mm-dd, and values are the number of
   * accesses. The array is empty if the query failed or the short URL does
   * not exist.
   *
   * The column shorturl_access.shorturl_id is a FK to shorturls and
   * automatically indexed, so this query should be pretty fast.
   *
   * @return array described above
   */
  public function dailyAccess($shorturl, $days) {
    $out = array();
    // We subtract one day because today is included in the range.
    $start_date = new DateTime();
    $start_date->sub(new DateInterval('P'.($days - 1).'D'));
    $start_date->setTime(0, 0, 0);
    // The end date should be tomorrow to include today in the range.
    $end_date = new DateTime('tomorrow');

    $interval = new DateInterval('P1D');
    $daterange = new DatePeriod($start_date, $interval, $end_date);

    foreach ($daterange as $date) {
      $out[$date->getTimestamp()] = 0;
    }

    $date_ts = null;
    $count = 0;

    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT UNIX_TIMESTAMP(DATE(access_hour)), SUM(access_count) '.
        'FROM shorturl_access_buckets '.
        'JOIN shorturls ON shorturls.id = shorturl_access_buckets.shorturl_id '.
        'WHERE shorturls.shorturl = ? '.
        'AND access_hour >= DATE_SUB(NOW(), INTERVAL ? DAY) '.
        'GROUP BY DATE(access_hour)');
    $query->bind_param('si', $shorturl, $days);
    $query->execute();
    $query->bind_result($date_ts, $count);

    while ($query->fetch()) {
      if (array_key_exists($date_ts, $out)) {
        $out[$date_ts] = $count;
      }
    }

    $query->close();
    return $out;
  }
}
