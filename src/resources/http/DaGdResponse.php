<?php

abstract class DaGdResponse {
  private $code = 200;
  private $message = 'OK';
  private $headers = array();
  private $body = '';

  // TODO: This is only important for text responses, so possibly should move
  // to DaGdTextResponse, but it feels weird to implement one-off controlling
  // methods in subclasses.
  private $trailing_newline = false;

  public function setCode($code) {
    $this->code = $code;
    return $this;
  }

  public function setMessage($message) {
    $this->message = $message;
    return $this;
  }

  public function setMimetype($mimetype) {
    $this->mimetype = $mimetype;
    return $this;
  }

  public function setHeaders($headers) {
    $this->headers = $headers;
    return $this;
  }

  public function setTrailingNewline($trailing_newline) {
    $this->trailing_newline = $trailing_newline;
    return $this;
  }

  public function addHeader($key, $value, $throw_on_existing = false) {
    if ($throw_on_existing && array_key_exists($key, $this->getHeaders())) {
      throw new Exception('Header "'.$key.'" would be redefined.');
    }
    $this->headers[$key] = $value;
    return $this;
  }

  public function setBody($body) {
    $this->body = $body;
    return $this;
  }

  public function getCode() {
    return $this->code;
  }

  public function getMessage() {
    return $this->message;
  }

  public function getMimetype() {
    return $this->mimetype;
  }

  public function getHeaders() {
    return $this->headers;
  }

  public function getBody() {
    return $this->body;
  }

  public function getTrailingNewline() {
    return $this->trailing_newline;
  }

  public function sendHeaders() {
    if ($this->getCode() != 200) {
      header('HTTP/1.1 '.$this->getCode().' '.$this->getMessage());
    }
    foreach ($this->getHeaders() as $key => $value) {
      header($key.': '.$value);
    }
  }

  public function render() {
    $this->sendHeaders();
    echo $this->getBody();
  }
}
