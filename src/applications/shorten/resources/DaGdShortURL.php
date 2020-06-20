<?php

/**
 * A super simple read-only representation of a short URL.
 *
 * This is just a disguised 3-tuple.
 */
final class DaGdShortURL {
  private $id;
  private $short_url;
  private $long_url;

  public function __construct($id, $short_url, $long_url) {
    $this->id = $id;
    $this->short_url = $short_url;
    $this->long_url = $long_url;
  }

  public function getId() {
    return $this->id;
  }

  public function getShortUrl() {
    return $this->short_url;
  }

  public function getLongUrl() {
    return $this->long_url;
  }
}
