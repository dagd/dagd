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
  public function fromShort($short_url) {
    $long_url = null;

    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT id, longurl FROM shorturls WHERE shorturl=? AND enabled=1');
    $query->bind_param('s', $short_url);
    $start = microtime(true);
    $query->execute();
    $end = microtime(true);
    statsd_time('query_time_getLongURL', ($end - $start) * 1000);
    $query->bind_result($id, $long_url);
    $query->fetch();
    $query->close();

    if (!empty($id) && !empty($long_url)) {
      return new DaGdShortURL($id, $short_url, $long_url);
    }

    return null;
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
    $longurl_hash = hash('sha256', $long_url);

    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT id, shorturl FROM shorturls WHERE longurl_hash=? AND '.
        'enabled=1 AND custom_shorturl=0 ORDER BY id DESC LIMIT 1');
    $query->bind_param('s', $longurl_hash);
    $start = microtime(true);
    $query->execute();
    $end = microtime(true);
    statsd_time('query_time_getNonCustomShortURL', ($end - $start) * 1000);
    $query->bind_result($id, $short_url);
    $query->fetch();
    $query->close();

    if (!empty($id) && !empty($short_url)) {
      return new DaGdShortURL($id, $short_url, $long_url);
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
  public function isBlacklisted($long_url) {
    return id(new Blacklist($long_url))
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
        return true;
      }
    }

    $whitelist_regexes = DaGdConfig::get('shorten.longurl_whitelist');
    foreach ($whitelist_regexes as $regex) {
      if (preg_match('#'.$regex.'#i', $this->getLongUrl())) {
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
      return new DaGdShortURL($id, $short_url, $long_url);
    } else {
      statsd_bump('shorturl_store_fail');
      throw new DaGdShortenStoreException();
    }
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

    $date = null;
    $count = 0;

    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'select access_dt, count(*) from shorturl_access where '.
        'shorturl_id=(select id from shorturls where shorturl=?) '.
        'and access_dt >= date_sub(now(), interval ? day) group by '.
        'year(access_dt), month(access_dt), day(access_dt) order by '.
        'access_dt asc');
    $query->bind_param('si', $shorturl, $days);
    $query->execute();
    $query->bind_result($date, $count);

    // Special-case the first fetch so we can use it to prepare $out.
    if (!$query->fetch()) {
      // There is no work to do, bail out early
      return array();
    }

    $now_date = date('Y-m-d');
    $iter_date = date('Y-m-d', strtotime($date));
    $first_date = $iter_date;
    // We want to start with the first date in the result.
    while ($iter_date != $now_date) {
      // Store the epoch for the start of the day.
      $key = strtotime($iter_date);
      $out[$key] = 0;
      $iter_date = date('Y-m-d', strtotime($iter_date.' + 1 day'));
    }

    // And fill in the first one that we got with the fetch() above so we don't
    // drop any data.
    $key = strtotime($first_date);
    $out[$key] = $count;

    // Now fetch the rest of the rows.
    while ($query->fetch()) {
      $date_epoch = strtotime(date('Y-m-d', strtotime($date)));
      $out[$date_epoch] = $count;
    }

    $query->close();
    return $out;
  }
}
