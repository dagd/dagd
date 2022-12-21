<?php

require_once dirname(__FILE__).'/safe_browsing.php';

// A class for grouping together blacklist logic.
// check* will update $blacklisted or bail out early if it's already set.
// They all return $this, and so can be chained.
// Blacklist->check() will run all checks in default order.
class Blacklist {
  protected $url;
  protected $blacklisted = false;
  protected $blacklist_source = '';
  protected $blacklist_reason = '';
  protected $cache;

  /**
   * Is the query for a short URL that is being created?
   *
   * We use this because some services (cough, GSB) rate-limit us, but we can
   * bypass the rate-limit (without violating any rules) by using a different
   * project for a different purpose. So we use a different project for creating
   * versus accessing short URLs.
   */
  protected $is_create = false;

  public function __construct($url) {
    $this->url = $url;
  }

  private function setBlacklisted($blacklisted) {
    $this->blacklisted = $blacklisted;
    return $this;
  }

  public function getBlacklisted() {
    return $this->blacklisted;
  }

  public function setBlacklistSource($blacklist_source) {
    $this->blacklist_source = $blacklist_source;
    return $this;
  }

  public function getBlacklistSource() {
    return $this->blacklist_source;
  }

  public function setBlacklistReason($blacklist_reason) {
    $this->blacklist_reason = $blacklist_reason;
    return $this;
  }

  public function getBlacklistReason() {
    return $this->blacklist_reason;
  }

  public function setCache($cache) {
    $this->cache = $cache;
    return $this;
  }

  public function getCache() {
    return $this->cache;
  }

  public function setIsCreate($is_create) {
    $this->is_create = $is_create;
    return $this;
  }

  public function getIsCreate() {
    return $this->is_create;
  }

  public function checkString() {
    if ($this->getBlacklisted()) {
      return $this;
    }

    // Check the array of strings and do direct substring searches
    // because they are significantly faster than regexes.
    $blacklist_strings = DaGdConfig::get('shorten.longurl_blacklist_strings');
    foreach ($blacklist_strings as $string) {
      if (strpos($this->url, $string) !== false) {
        statsd_bump('shorturl_blacklisted_string');
        statsd_bump('shorturl_blacklisted');
        $this->setBlacklisted(true);
        $this->setBlacklistSource('shorten.longurl_blacklist_strings');
        $this->setBlacklistReason($string);
        return $this;
      }
    }

    return $this;
  }

  public function checkRegex() {
    if ($this->getBlacklisted()) {
      return $this;
    }

    $blacklist_regexes = DaGdConfig::get('shorten.longurl_blacklist');
    foreach ($blacklist_regexes as $regex) {
      if (preg_match('#'.$regex.'#i', $this->url)) {
        statsd_bump('shorturl_blacklisted_regex');
        statsd_bump('shorturl_blacklisted');
        $this->setBlacklisted(true);
        $this->setBlacklistSource('shorten.longurl_blacklist');
        $this->setBlacklistReason('#'.$regex.'#i');
        return $this;
      }
    }

    return $this;
  }

  public function checkDNSBL() {
    if ($this->getBlacklisted()) {
      return $this;
    }

    // Check if there are any dnsbl server suffixes defined.
    $dnsbl_servers = DaGdConfig::get('shorten.dnsbl');

    if (count($dnsbl_servers) !== 0) {
      $host = parse_url($this->url, PHP_URL_HOST);

      if ($host !== false) {
        $dnsbl = new DaGdDNSBLLookup($host);
        $safe_url = null;
        $want_cache = DaGdConfig::get('shorten.dnsbl_cache');

        if ($want_cache && $cache = $this->getCache()) {
          $key = 'dnsbl_'.hash('sha256', $this->url);
          $seconds = DaGdConfig::get('shorten.dnsbl_cache_expiry');
          $safe_url = $cache->getOrStore($key, $dnsbl, $seconds);
        } else {
          $safe_url = $dnsbl->run();
        }

        if (idx($safe_url, 'safe') === false) {
          statsd_bump('shorturl_blacklisted_dnsbl');
          statsd_bump('shorturl_blacklisted');
          $this->setBlacklisted(true);
          $this->setBlacklistSource('shorten.dnsbl');
          $this->setBlacklistReason(idx($safe_url, 'source'));
          return $this;
        }
      }
    }

    return $this;
  }

  public function checkSafeBrowsing() {
    if ($this->getBlacklisted()) {
      return $this;
    }

    $safe_browsing_enabled = DaGdConfig::get('shorten.safe_browsing');

    if (!$safe_browsing_enabled) {
      return $this;
    }

    $safe_url = null;

    $want_cache = DaGdConfig::get('shorten.safe_browsing_cache');
    // Allows this function to be used even if setCache() is never called.
    if ($this->getCache() && $want_cache) {
      $gsb = new DaGdGoogleSafeBrowsing($this->url, $this->getIsCreate());
      $key = 'gsb_'.hash('sha256', $this->url);
      $seconds = DaGdConfig::get('shorten.safe_browsing_cache_expiry');
      $safe_url = $this->getCache()->getOrStore($key, $gsb, $seconds);
    } else {
      $safe_url = query_safe_browsing($this->url, $this->getIsCreate());
    }

    if ($safe_url === false) {
      statsd_bump('shorturl_blacklisted_safebrowsing');
      statsd_bump('shorturl_blacklisted');
      $this->setBlacklisted(true);
      $this->setBlacklistSource('shorten.safe_browsing');
      $this->setBlacklistReason('Google Safe Browsing');
    }

    return $this;
  }

  /**
   * Run all checks and return $this.
   */
  public function checkAll() {
    return $this
      ->checkString()
      ->checkRegex()
      ->checkDNSBL()
      ->checkSafeBrowsing();
  }

  public function check() {
    try {
      return $this
        ->checkAll()
        ->getBlacklisted();
    } catch (Exception $ex) {
      return $this->getBlacklisted();
    }
  }
}
