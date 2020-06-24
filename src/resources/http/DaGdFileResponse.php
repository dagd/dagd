<?php

final class DaGdFileResponse extends DaGdCacheableResponse {
  private $content_type;

  public function setContentType($content_type) {
    $this->content_type = $content_type;
    return $this;
  }

  public function getContentType() {
    return $this->content_type;
  }

  public function getHeaders() {
    $headers = array(
      'Content-type' => $this->content_type,
      'X-Content-Type-Options' => 'nosniff',
    );

    // Merge in the potentially-cacheable headers
    return array_merge(parent::getHeaders(), $headers);
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
