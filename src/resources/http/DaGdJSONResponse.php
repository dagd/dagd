<?php

final class DaGdJSONResponse extends DaGdResponse {
  public function getHeaders() {
    $headers = array(
      'Content-type' => 'application/json',
      'X-Content-Type-Options' => 'nosniff',
    );
    $headers = array_merge(parent::getHeaders(), $headers);
    return $headers;
  }
}
