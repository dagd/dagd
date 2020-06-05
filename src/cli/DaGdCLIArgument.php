<?php

final class DaGdCLIArgument extends DaGdCLIParameter {
  private $value;

  public function getKind() {
    return 'argument';
  }

  public function setValue($value) {
    $this->value = $value;
    return $this;
  }

  public function getValue() {
    return $this->value;
  }
}
