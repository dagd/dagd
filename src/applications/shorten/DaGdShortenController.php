<?php

require_once dirname(__FILE__).'/resources/random_string.php';
require_once dirname(__FILE__).'/resources/blacklist.php';

class DaGdShortenController extends DaGdBaseClass {
  public function getHelp() {
    return array(
      'title' => 'shorten',
      'summary' => 'Shorten your long URLs (/, /s, /shorten).',
      'path' => '',
      'examples' => array(
        array(
          'arguments' => null,
          'request' => array(
            'url' => 'http://some_long_url',
            'shorturl' => 'slug'),
          'summary' => 'Shorten a URL'),
        array(
          'arguments' => array('g'),
          'summary' => 'An example short URL with a custom suffix'),
      ));
  }

  public function configure() {
    $style = '#bar {
      background-color: #3c6eb4;
      color: #f8f8f8;
      padding: 15px;
      height: 30px;
      line-height: 30px;
    }
    .constraint { width: 700px; margin: 0 auto; }
    #bar a, #bar a:active, #bar a:visited { color: #ccc; }
    table { border-spacing: 30px; border-collapse: separate; }
    td { padding: 10px 0; }
    h2 { font-family: overpass, sans-serif !important; }
    body, h2 { margin: 0; padding: 0; }';

    $this
      ->setWrapHtml(true)
      ->setWrapPre(false)
      ->setStyle($style);
    return $this;
  }

  private $long_url;
  private $longurl_hash;
  private $short_url;
  private $stored_url_id;
  private $owner_ip;
  private $custom_url = false;
  private $store_url = true;

  private function isFreeShortURL() {
    $query = $this->getReadDB()->prepare(
      'SELECT COUNT(*) FROM shorturls WHERE shorturl=?');
    $query->bind_param('s', $this->short_url);
    $query->execute();
    $query->bind_result($count);
    $query->fetch();
    $query->close();

    // If we do *NOT* get one above (!), then it is free, and we return true.
    return !(bool)$count;
  }

  private function blacklisted($url) {
    return id(new Blacklist($url))->check();
  }

  private function isBannedAuthor() {
    $query = $this->getReadDB()->prepare(
      'SELECT COUNT(*) FROM blocked_ips WHERE '.
      'inet6_aton(?) between ip_start and ip_end');
    $query->bind_param('s', $this->owner_ip);
    $query->execute();
    $query->bind_result($count);
    $query->fetch();
    $query->close();

    // Return true if the author is banned, false if not.
    return (bool)$count;
  }

  private function whitelisted($url) {
    // Similar to above, but for whitelisting.
    $whitelist_strings = DaGdConfig::get('shorten.longurl_whitelist_strings');
    foreach ($whitelist_strings as $string) {
      if (strpos($url, $string) !== false) {
        statsd_bump('shorturl_whitelisted');
        return true;
      }
    }

    $whitelist_regexes = DaGdConfig::get('shorten.longurl_whitelist');
    foreach ($whitelist_regexes as $regex) {
      if (preg_match('#'.$regex.'#i', $url)) {
        statsd_bump('shorturl_whitelisted');
        return true;
      }
    }
    return false;
  }

  public function getLongURL($shorturl) {
    $query = $this->getReadDB()->prepare(
      'SELECT id,longurl FROM shorturls WHERE shorturl=? AND enabled=1');
    $query->bind_param('s', $shorturl);
    $start = microtime(true);
    $query->execute();
    $end = microtime(true);
    statsd_time('query_time_getLongURL', ($end - $start) * 1000);
    $query->bind_result($this->stored_url_id, $this->long_url);
    $query->fetch();
    $query->close();
    return $this->long_url;
  }

  public function getStatsForURL($shorturl) {
    // This function costs two queries, but they both hit already-existing
    // indexes and so they should be nearly trivial.
    $id = null;
    $creation_dt = null;
    $longurl = null;

    // Get some initial info
    $shorturls_query = $this->getReadDB()->prepare(
      'SELECT id,creation_dt,longurl FROM shorturls '.
      'WHERE shorturl=? AND enabled=1');
    $shorturls_query->bind_param('s', $shorturl);
    $start = microtime(true);
    $shorturls_query->execute();
    $end = microtime(true);
    statsd_time('query_time_stats_select', ($end - $start) * 1000);
    $shorturls_query->bind_result($id, $creation_dt, $longurl);
    $shorturls_query->fetch();
    $shorturls_query->close();

    // If $id never gets set, the shorturl doesn't exist. Bail out and return
    // null.
    if ($id === null) {
      return null;
    }

    $count_accesses = null;
    $count_distinct_accesses = null;

    $access_query = $this->getReadDB()->prepare(
      'SELECT count(ip),count(distinct ip) FROM shorturl_access '.
      'WHERE shorturl_id=?');
    $access_query->bind_param('i', $id);
    $start = microtime(true);
    $access_query->execute();
    $end = microtime(true);
    statsd_time('query_time_stats_access', ($end - $start) * 1000);
    $access_query->bind_result($count_accesses, $count_distinct_accesses);
    $access_query->fetch();
    $access_query->close();

    $res = array(
      'id' => $id,
      'creation_dt' => $creation_dt,
      'longurl' => $longurl,
      'accesses' => $count_accesses,
      'distinct_accesses' => $count_distinct_accesses,
    );
    return $res;
  }

  private function getNonCustomShortURL($longurl_hash) {
    $query = $this->getReadDB()->prepare(
      'SELECT id,shorturl FROM shorturls WHERE longurl_hash=? AND enabled=1 '.
      'AND custom_shorturl=0 ORDER BY id DESC LIMIT 1');
    $query->bind_param('s', $longurl_hash);
    $start = microtime(true);
    $query->execute();
    $end = microtime(true);
    statsd_time('query_time_getNonCustomShortURL', ($end - $start) * 1000);
    $query->bind_result($this->stored_url_id, $this->short_url);
    $query->fetch();
    $query->close();
    return;
  }

  private function logURLAccess() {
    $query = $this->getWriteDB()->prepare(
      'INSERT INTO shorturl_access(shorturl_id, ip, useragent) VALUES(?,?,?)');
    $stored_url_id = $this->stored_url_id;
    $client_ip = client_ip();
    $useragent = server_or_default('HTTP_USER_AGENT', '');
    $query->bind_param(
      'iss',
      $stored_url_id,
      $client_ip,
      $useragent);

    $start = microtime(true);
    $res = $query->execute();
    $end = microtime(true);
    statsd_time('query_time_LogURLAccess', ($end - $start) * 1000);

    if ($res) {
      return true;
    } else {
      return false;
    }
  }

  private function redirect_from_shorturl() {
    $this->getLongURL($this->route_matches[1]);
    if ($this->long_url) {

      // This check is best-effort to allow all older entries to continue
      // working even if they don't host-parse.
      // If it's whitelisted, don't even bother checking dnsbl
      if (!$this->whitelisted($this->long_url)) {
        if ($this->blacklisted($this->long_url)) {
          // If blacklisted for any reason, simply return a 404.
          error404();
          return false;
        }
      }

      $this->logURLAccess();
      statsd_bump('shorturl_access');
      header('X-Original-URL: '.$this->long_url);
      $qs = build_given_querystring();
      if ($this->route_matches[2]) {
        header('Location: '.$this->long_url.'/'.$this->route_matches[2].$qs);
      } else {
        header('Location: '.$this->long_url.$qs);
      }
      return true;
    } else {
      error404();
      return false;
    }
  }

  private function store_shorturl() {
    if (!$this->store_url) {
      return true;
    }
    $query = $this->getWriteDB()->prepare(
      'INSERT INTO shorturls(shorturl, longurl, owner_ip, custom_shorturl, '.
      'longurl_hash) VALUES(?, ?, ?, ?, ?);');
    $query->bind_param(
      'sssis',
      $this->short_url,
      $this->long_url,
      $this->owner_ip,
      $this->custom_url,
      $this->longurl_hash);

    $start = microtime(true);
    $res = $query->execute();
    $end = microtime(true);
    statsd_time('query_time_store_shorturl_insert', ($end - $start) * 1000);

    if ($res) {
      statsd_bump('shorturl_store');
      return true;
    } else {
      error500('Something has gone wrong! :( ... Try again? Please?');
      statsd_bump('shorturl_store_fail');
      return false;
    }
  }

  private function set_shorturl_or_400() {
    if ($short_url = request_or_default('shorturl')) {
      $this->custom_url = true;
      $valid_char_pattern = DaGdConfig::get('shorten.custom_url_regex');
      if (!preg_match($valid_char_pattern, $short_url)) {
        error400('Invalid short URL entered. Alphanumeric only, please.');
        return false;
      } else {
        $routes = DaGdConfig::get('general.routemap');
        foreach ($routes as $route => $metadata) {
          if ($metadata['controller'] == 'DaGdShortenController') {
            continue;
          }
          $route = substr($route, 1);
          if (preg_match('@^'.$route.'@', $short_url)) {
            error400(
              'That URL conflicts with other da.gd URLs. Please use another '.
              'URL.');
            return false;
          }
        }
        $this->short_url = substr($short_url, 0, 10);
        if (!$this->isFreeShortURL()) {
          error400('That custom URL was already taken, go back and try again!');
          return false;
        }
      }
      statsd_bump('shorturl_new_custom');
    } else {
      $this->longurl_hash = hash('sha256', $this->long_url);
      $this->getNonCustomShortURL($this->longurl_hash);
      if ($this->short_url) {
        // The idea here is that we query against the hash which has an index
        // that we hit with getNonCustomShortURL.
        // If we get a result, then it's stored in $this->short_url, and we don't
        // want to re-insert it, so we set store_url false.
        //
        // See 1d3974d741eab5765cf9e20ca5e7277be1747699 for some reasoning, but
        // ultimately it comes down to maximum InnoDB index key length and this
        // being a workaround to make it quicker to add random longurls. We want
        // to re-use random shorturls when shortening the same longurl and
        // without being able to add longurl into an index, our lookup times
        // were crazy on random url inserts.
        $this->store_url = false;
      } else {
        $min = DaGdConfig::get('shorten.random_min_length');
        $max = DaGdConfig::get('shorten.random_max_length');
        $this->short_url = randstr(rand($min, $max));
        while (!$this->isFreeShortURL()) {
          debug('Hash collision', 'Calling randstr again');
          statsd_bump('shorturl_random_hash_collision');
          $this->short_url = randstr(rand($min, $max));
        }
      }
      statsd_bump('shorturl_new_random');
    }

    $this->short_url = htmlspecialchars(urlencode($this->short_url));
    return true;
  }

  public function set_longurl_or_400() {
    if ($_REQUEST['url'] == '') {
      // If url was there but is an empty string, say so.
      error400('Error: Cannot create something out of nothing.');
      return false;
    }

    if ($long_url = request_or_default('url')) {
      // Something has at least been submitted. Is it valid?
      // Good enough for now...probably needs some better checks.
      if (preg_match('@^https?://@', $long_url)) {

        if ($this->isBannedAuthor()) {
          statsd_bump('shorturl_author_ip_banned');
          error403();
          return false;
        }

        // If it's not whitelisted, and it IS blacklisted, then bail out.
        if (!$this->whitelisted($long_url)) {
          if ($this->blacklisted($long_url)) {
            error400('Blacklisted original URL.');
            return false;
          }
        }

        $this->long_url = $long_url;
        return true;
      } else {
        error400('http and https protocols only, please.');
        return false;
      }
    } else {
      return false;
    }
  }

  public function render() {
    if (array_key_exists('url', $_REQUEST)) {
      $this->owner_ip = client_ip();
      if ($this->set_longurl_or_400() && $this->set_shorturl_or_400()) {
        if ($this->store_shorturl()) {
          header('X-Short-URL: '.$this->short_url);
          $this->escape = false;
          $new_link = DaGdConfig::get('general.baseurl').'/'.$this->short_url;
          return '<a href="'.$new_link.'">'.$new_link.'</a>';
        }
      }
      return;
    }

    // No 'url' was passed, so we are not creating a new short-url.
    if ($this->route_matches[1]) {
      // Attempt to access a stored URL
      $this->redirect_from_shorturl();
      return;
    } else {
      // We are not attempting to access a stored URL, but we also don't have
      // a 'url' - Show the form so that we can create a new short-url.
      if (!is_html_useragent()) {
        // No use in showing a form for text UAs. Rather, show help text.
        // TODO: Move this to renderCLI()
        return help('DaGdShortenController');
      }

      $longurl_tr = tag(
        'tr',
        array(
          tag('td', 'Long URL: '),
          tag(
            'td',
            tag(
              'input',
              null,
              array(
                'type' => 'text',
                'name' => 'url',
                'id' => 'url',
                'size' => '35',
                'placeholder' => 'https://google.com/',
                'autofocus' => TAG_ATTR_BARE,
              )
            )
          ),
        )
      );

      $shorturl_tr = tag(
        'tr',
        array(
          tag(
            'td',
            'Custom short URL: ',
            array(
              'style' => 'white-space: nowrap;',
            )
          ),
          tag(
            'td',
            array(
              tag(
                'input',
                null,
                array(
                  'type' => 'text',
                  'maxlength' => '10',
                  'name' => 'shorturl',
                  'size' => '10',
                  'placeholder' => 'g',
                )
              ),
              tag('br'),
              tag('small', '(blank for random, max 10)'),
            )
          ),
        )
      );

      $submit_tr = tag(
        'tr',
        tag(
          'td',
          tag(
            'input',
            null,
            array(
              'type' => 'submit',
              'value' => 'Shorten URL',
              'style' => 'width: 100%',
            )
          ),
          array(
            'colspan' => '2',
          )
        )
      );

      $abuse_tr = tag(
        'tr',
        tag(
          'td',
          tag(
            'small',
            'report abuse to abuse@da.gd'
          )
        )
      );

      $form = tag(
        'form',
        tag(
          'table',
          array(
            $longurl_tr,
            $shorturl_tr,
            $submit_tr,
            $abuse_tr,
          )
        ),
        array(
          'method' => 'POST',
          'action' => '/',
        )
      );

      $darkmode_link = tag(
        'a',
        $this->getDarkmode() ? 'light mode' : 'dark mode',
        array(
          'href' => $this->getDarkmode() ? '/?darkmode=0' : '/?darkmode=1',
        )
      );

      $links = array(
        tag('a', 'help', array('href' => '/help')),
        ' | ',
        tag(
          'a',
          'open source',
          array('href' => 'https://github.com/relrod/dagd')
        ),
        ' | ',
        $darkmode_link,
        ' | ',
        tag('a', 'donate', array('href' => 'https://www.patreon.com/relrod')),
      );

      $links_div = tag(
        'div',
        array(
          tag('h2', 'da.gd', array('style' => 'float: left; display: inline;')),
          tag(
            'div',
            $links,
            array(
              'style' => 'float: right; display: inline;'
            )
          ),
        ),
        array(
          'class' => 'constraint',
        )
      );

      $navbar = tag(
        'div',
        $links_div,
        array(
          'id' => 'bar',
          'style' => 'overflow: auto;',
        )
      );

      return array(
        $navbar,
        tag(
          'div',
          $form,
          array(
            'id' => 'main',
            'class' => 'constraint',
          )
        ),
      );
    }
  }
}
