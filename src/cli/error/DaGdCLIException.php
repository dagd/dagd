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

  public function toCli($colors = true) {
    $out = '[';

    if ($colors) {
      $out .= $this->getProgram()->red('ERROR');
    } else {
      $out .= 'ERROR';
    }

    $out .= '] '.$this->getMessage();
    $out .= "\n";

    return $out;
  }
}
