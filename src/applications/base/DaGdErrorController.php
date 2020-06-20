<?php

abstract class DaGdErrorController extends DaGdController {
  private $message;

  public function setMessage($message) {
    $this->message = $message;
    return $this;
  }

  public function getMessage($default = null) {
    return $this->message ? $this->message : $default;
  }
}
