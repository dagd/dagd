<?php

class DaGdFormNonemptyTextInputField extends DaGdFormTextInputField {
  public function isValid() {
    $valid = true;

    if (strlen($this->getValue()) <= 0) {
      $this->addError('value must be non-empty');
      $valid = false;
    }

    if ($this->getValue() == 'errplz') {
      $this->addError('I errored');
      $valid = false;
    }

    return $valid;
  }
}
