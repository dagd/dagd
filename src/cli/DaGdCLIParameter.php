<?php

abstract class DaGdCLIParameter {
  private $name;
  private $shortname;
  private $description;
  private $required = false;
  private $given = false;

  public function getKind() {
    throw new Exception('Unimplemented method: getKind()');
  }

  public function setName($name) {
    $this->name = $name;
    return $this;
  }

  public function getName() {
    return $this->name;
  }

  public function setShortname($shortname) {
    if (strlen($shortname) != 2 || $shortname[0] != '-') {
      throw new Exception('shortname must be one character and follow a -');
    }
    $this->shortname = $shortname;
    return $this;
  }

  public function getShortname() {
    return $this->shortname;
  }

  public function setDescription($description) {
    $this->description = $description;
    return $this;
  }

  public function getDescription() {
    return $this->description;
  }

  public function setRequired($required) {
    if (gettype($required) !== 'boolean') {
      throw new Exception('"required" field must be a boolean');
    }
    $this->required = $required;
    return $this;
  }

  public function getRequired() {
    return $this->required;
  }

  public function setGiven($given) {
    $this->given = $given;
    return $this;
  }

  public function getGiven() {
    return $this->given;
  }
}
