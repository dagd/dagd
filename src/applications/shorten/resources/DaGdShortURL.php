<?php

/**
 * A super simple read-only representation of a short URL.
 *
 * This is just a disguised tuple.
 */
final class DaGdShortURL {
  private $id;
  private $short_url;
  private $long_url;
  private $bare_argument;

  public function __construct($id, $short_url, $long_url, $bare_argument) {
    $this->id = $id;
    $this->short_url = $short_url;
    $this->long_url = $long_url;
    $this->bare_argument = $bare_argument;
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

  public function getBareArgument() {
    return $this->bare_argument;
  }
}
