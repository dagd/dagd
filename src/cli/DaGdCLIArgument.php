<?php

final class DaGdCLIArgument extends DaGdCLIParameter {
  private $value;
  private $default;

  public function getKind() {
    return 'argument';
  }

  public function setValue($value) {
    $this->value = $value;
    return $this;
  }

  public function getValue() {
    if ($this->value !== null) {
      return $this->value;
    }
    return $this->getDefault();
  }

  /**
   * Sets the default value *if the argument was not passed*.
   */
  public function setDefault($default) {
    $this->default = $default;
    return $this;
  }

  /**
   * Gets the default value *if the argument was not passed*.
   */
  public function getDefault() {
    return $this->default;
  }
}
