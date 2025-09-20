<?php

final class DaGdShortURLCacheMiss implements DaGdCacheMissCallback {
  private $query;
  private $short_url;
  private $include_disabled;

  public function __construct(
    DaGdShortURLQuery $query,
    $short_url,
    $include_disabled) {

    $this->query = $query;
    $this->short_url = $short_url;
    $this->include_disabled = $include_disabled;
  }

  public function run($key) {
    statsd_bump('shorturl_cache_miss');
    return $this->query->fromShort(
      $this->short_url,
      $this->include_disabled);
  }
}