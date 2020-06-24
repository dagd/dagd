<?php

abstract class DaGdCacheableResponse extends DaGdResponse {
  private $cacheable = true;

  // This is 60 * 60 * 24 * 30 = one month.
  // We never change static content at a given route, so we can long-term cache.
  private $cache_duration = 2592000;

  public function setCacheable($cacheable) {
    $this->cacheable = $cacheable;
    return $this;
  }

  public function setCacheDuration($cache_duration) {
    $this->cache_duration = $cache_duration;
    return $this;
  }

  public function getCacheDuration() {
    return $this->cache_duration;
  }

  public function getHeaders() {
    $headers = array();

    if ($this->cacheable) {
      $headers['Cache-Control'] = implode(
        ', ',
        array(
          'public',
          'max-age='.$this->cache_duration,
          'immutable',
        )
      );
    } else {
      $headers['Cache-Control'] = 'no-cache';
      $headers['Expires'] = '-1';
    }

    // Explicitly do NOT merge headers with the superclass, because we want to
    // control our own caching.
    return $headers;
  }
}
