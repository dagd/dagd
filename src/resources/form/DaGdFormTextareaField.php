<?php

class DaGdFormTextareaField extends DaGdFormField {
  public function toTag() {
    return tag(
      'textarea',
      $this->getValue() ? $this->getValue() : '',
      $this->getAttributes());
  }
}
