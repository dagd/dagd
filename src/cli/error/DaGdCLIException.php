<?php

abstract class DaGdCLIException extends DaGdException {
  private $program;

  public function setProgram($program) {
    $this->program = $program;
    return $this;
  }

  public function getProgram() {
    return $this->program;
  }

  public function getCliMessage() {
    return null;
  }

  public function getPublicMessage() {
    return $this->getCliMessage();
  }

  public function getPrivateMessage() {
    return $this->getCliMessage();
  }

  public function toCli($colors = true) {
    $str = null;

    if ($this->getCliMessage()) {
      $str = $this->getCliMessage();
    } else {
      $str = $this->getMessage();
    }

    return $this->getProgram()->error($str, $colors);
  }
}
