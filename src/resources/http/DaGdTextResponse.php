<?php

final class DaGdTextResponse extends DaGdResponse {
  public function getHeaders() {
    $headers = array(
      'Content-type' => 'text/plain; charset=utf-8',
      'X-Content-Type-Options' => 'nosniff',
    );
    $headers = array_merge(parent::getHeaders(), $headers);
    return $headers;
  }

  public function getBody() {
    $newline = $this->getTrailingNewline() ? "\n" : '';
    return parent::getBody().$newline;
  }
}
