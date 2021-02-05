<?php

class DaGdFormTextareaField extends DaGdFormField {
  public function toTag() {
    $error_lis = array();
    foreach ($this->getErrors() as $error) {
      $error_lis[] = tag('li', $error);
    }

    $error_ul = null;
    if (!empty($error_lis)) {
      $error_ul = tag('ul', $error_lis);
    }

    $field = tag(
      'textarea',
      $this->getValue() ? $this->getValue() : '',
      $this->getAttributes());

    $div = tag(
      'div',
      array(
        $field,
        $error_ul,
      )
    );

    return $div;
  }
}
