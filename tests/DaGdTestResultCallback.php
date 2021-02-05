<?php

abstract class DaGdTestResultCallback {
  private $test;

  public function setTest($test) {
    $this->test = $test;
    return $this;
  }

  public function getTest() {
    return $this->test;
  }

  public function fail($text, $output = '') {
    throw new Exception('fail() not implemented');
  }

  public function pass($text) {
    throw new Exception('pass() not implemented.');
  }
}
