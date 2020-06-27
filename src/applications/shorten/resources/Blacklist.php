<?php

require_once dirname(__FILE__).'/dnsbl.php';
require_once dirname(__FILE__).'/safe_browsing.php';

// A class for grouping together blacklist logic.
// check* will update $blacklisted or bail out early if it's already set.
// They all return $this, and so can be chained.
// Blacklist->check() will run all checks in default order.
class Blacklist {
  protected $url;
  protected $blacklisted = false;
  protected $blacklist_source = '';

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
      if ($host !== false && !query_dnsbl($host)) {
        statsd_bump('shorturl_blacklisted_dnsbl');
        statsd_bump('shorturl_blacklisted');
        $this->setBlacklisted(true);
        $this->setBlacklistSource('shorten.dnsbl');
        return $this;
      }
    }

    return $this;
  }

  public function checkSafeBrowsing() {
    if ($this->getBlacklisted()) {
      return $this;
    }

    $safe_browsing_enabled = DaGdConfig::get('shorten.safe_browsing');

    if ($safe_browsing_enabled) {
      statsd_bump('shorturl_blacklist_query_safebrowsing');
      $safe_url = query_safe_browsing($this->url);
      if ($safe_url === false) {
        statsd_bump('shorturl_blacklisted_safebrowsing');
        statsd_bump('shorturl_blacklisted');
        $this->setBlacklisted(true);
        $this->setBlacklistSource('shorten.safe_browsing');
        return $this;
      }
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
      $this
        ->checkAll()
        ->getBlacklisted();
    } catch (Exception $ex) {
      return $this->getBlacklisted();
    }
  }
}
