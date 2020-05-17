<?php

final class DaGdTextResponse extends DaGdResponse {
  private $trailing_newline = false;

  public function setTrailingNewline($trailing_newline) {
    $this->trailing_newline = $trailing_newline;
    return $this;
  }

  public function getTrailingNewline() {
    return $this->trailing_newline;
  }

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
