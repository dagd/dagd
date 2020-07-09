<?php

// TODO: Move query_safe_browsing into here.
final class DaGdGoogleSafeBrowsing implements DaGdCacheMissCallback {
  private $url;

  public function __construct($url) {
    $this->url = $url;
  }

  public function run($key) {
    statsd_bump('shorturl_blacklist_query_safebrowsing');
    return query_safe_browsing($this->url);
  }
}
