<?php

// TODO: This almost feels like it should extend DaGdTag if it were non-final.
// It could feasibly be used as an argument/reason to make DaGdTag non-final.
abstract class DaGdFormField implements DaGdToTagInterface {
  private $name;
  private $attributes = array();
  private $errors = array();
  private $value;

  public function __construct($name) {
    $this->name = $name;
  }

  abstract public function toTag();

  public function isValid() {
    return true;
  }

  public function setName($name) {
    $this->name = $name;
    return $this;
  }

  public function getName() {
    return $this->name;
  }

  public function setAttributes($attributes) {
    $this->attributes = $attributes;
    return $this;
  }

  public function getAttributes() {
    $attrs = array(
      'name' => $this->getName(),
      'id' => $this->getName(),
    );

    return array_merge(
      $this->attributes,
      $attrs);
  }

  public function setErrors($errors) {
    $this->errors = $errors;
    return $this;
  }

  public function addError($error) {
    $this->errors[] = $error;
    return $this;
  }

  public function getErrors() {
    return $this->errors;
  }

  public function setValue($value) {
    $this->value = $value;
    return $this;
  }

  public function getValue() {
    return $this->value;
  }
}
