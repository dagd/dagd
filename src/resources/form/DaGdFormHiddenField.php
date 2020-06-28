<?php

class DaGdFormHiddenField extends DaGdFormField {
  public function toTag() {
    return tag(
      'input',
      null,
      array_merge(
        $this->getAttributes(),
        array(
          'value' => $this->getValue(),
          'type' => 'hidden',
        )));
  }
}
