<?php

final class DaGdShortURL {
  private $id;
  private $long_url;

  public function __construct($controller, $id, $short_url, $long_url) {
    $this->id = $id;
    $this->short_url = $short_url;
    $this->long_url = $long_url;
  }

  public function getId() {
    return $this->id;
  }

  public function getLongUrl() {
    return $this->long_url;
  }

  /**
   * @return boolean
   */
  public function isBlacklisted() {
    return id(new Blacklist($this->getLongUrl()))->check();
  }

  /**
   * @return boolean
   */
  public function isWhitelisted() {
    $whitelist_strings = DaGdConfig::get('shorten.longurl_whitelist_strings');
    foreach ($whitelist_strings as $string) {
      if (strpos($this->getLongUrl(), $string) !== false) {
        statsd_bump('shorturl_whitelisted');
        return true;
      }
    }

    $whitelist_regexes = DaGdConfig::get('shorten.longurl_whitelist');
    foreach ($whitelist_regexes as $regex) {
      if (preg_match('#'.$regex.'#i', $this->getLongUrl())) {
        statsd_bump('shorturl_whitelisted');
        return true;
      }
    }
    return false;
  }
}
