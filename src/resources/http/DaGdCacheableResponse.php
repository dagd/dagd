<?php

final class DaGdCacheableResponse extends DaGdResponse {
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
    if ($this->cacheable) {
      $this->addHeader(
        'Cache-Control',
        implode(
          ', ',
          array(
            'public',
            'max-age='.$this->cache_duration,
            'immutable',
          )
        ));
    } else {
      $this->addHeader('Cache-Control', 'no-cache');
    }

    return parent::getHeaders();
  }
}
