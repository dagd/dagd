<?php

class DaGdFormTextInputField extends DaGdFormField {
  public function getAttributes() {
    $attrs = array();
    if ($val = $this->getValue()) {
      $attrs['value'] = $val;
    }

    return array_merge(
      parent::getAttributes(),
      $attrs);
  }

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
      'input',
      null,
      array_merge(
        $this->getAttributes(),
        array(
          'type' => 'text',
        )
      ));

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
