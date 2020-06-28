<?php

class DaGdFormNonemptyTextareaField extends DaGdFormTextareaField {
  public function isValid() {
    if (strlen($this->getValue()) > 0) {
      return true;
    }

    $this->addError('value must be non-empty');
  }
}
