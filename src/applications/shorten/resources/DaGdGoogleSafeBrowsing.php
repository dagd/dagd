<?php

// TODO: Move query_safe_browsing into here.
final class DaGdGoogleSafeBrowsing implements DaGdCacheMissCallback {
  private $url;

  public function __construct($url, $is_create) {
    $this->url = $url;
    $this->is_create = $is_create;
  }

  public function run($key) {
    statsd_bump('shorturl_blacklist_query_safebrowsing');
    $start = microtime(true);
    $gsb = query_safe_browsing($this->url, $this->is_create);
    $end = microtime(true);
    statsd_time('gsb_query_time', ($end - $start) * 1000);
    return $gsb;
  }
}
