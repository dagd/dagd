<?php

final class DaGdRedirectResponse extends DaGdResponse {
  private $to;

  public function setTo($to) {
    $this->to = $to;
    return $this;
  }

  public function getTo() {
    return $this->to;
  }

  public function getHeaders() {
    $headers = array(
      'Location' => $this->getTo(),
    );
    $headers = array_merge(parent::getHeaders(), $headers);
    return $headers;
  }

  public function getBody() {
    return null;
  }
}
