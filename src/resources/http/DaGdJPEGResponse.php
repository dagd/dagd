<?php

final class DaGdJPEGResponse extends DaGdCacheableResponse {
  public function getHeaders() {
    $headers = array(
      'Content-type' => 'image/jpeg',
      'X-Content-Type-Options' => 'nosniff',
    );
    $headers = array_merge(parent::getHeaders(), $headers);
    return $headers;
  }
}
