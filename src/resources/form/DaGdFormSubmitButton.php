<?php

class DaGdFormSubmitButton extends DaGdFormField {
  public function toTag() {
    return tag(
      'input',
      null,
      array_merge(
        $this->getAttributes(),
        array('type' => 'submit')));
  }
}
