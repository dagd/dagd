<?php

final class DaGdCLIFreeformParameter extends DaGdCLIParameter {
  private $value;
  private $default;

  public function getKind() {
    return 'freeform_parameter';
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
   * Sets the default value *if the parameter was not passed*.
   */
  public function setDefault($default) {
    $this->default = $default;
    return $this;
  }

  /**
   * Gets the default value *if the parameter was not passed*.
   */
  public function getDefault() {
    return $this->default;
  }
}
