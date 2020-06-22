<?php

final class DaGdFileResponse extends DaGdResponse {
  private $content_type;
  private $cacheable = true;

  // This is 60 * 60 * 24 * 30 = one month.
  // We never change static content at a given route, so we can long-term cache.
  private $cache_duration = 2592000;

  public function setContentType($content_type) {
    $this->content_type = $content_type;
    return $this;
  }

  public function getContentType() {
    return $this->content_type;
  }

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
    $headers = array(
      'Content-type' => $this->content_type,
      'X-Content-Type-Options' => 'nosniff',
    );

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

  /**
   * Allow an arbitrary file to be read.
   *
   * Callers MUST validate that the file path is safe to send before calling
   * this method.
   */
  public function setFile($filename) {
    $data = file_get_contents($filename);
    $this->setBody($data);
    return $this;
  }
}
