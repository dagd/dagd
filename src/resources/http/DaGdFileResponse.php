<?php

final class DaGdFileResponse extends DaGdResponse {
  private $content_type;

  public function setContentType($content_type) {
    $this->content_type = $content_type;
    return $this;
  }

  public function getHeaders() {
    $headers = array(
      'Content-type' => $this->content_type,
      'X-Content-Type-Options' => 'nosniff',
    );
    $headers = array_merge(parent::getHeaders(), $headers);
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
