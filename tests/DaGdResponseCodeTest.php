<?php

final class DaGdResponseCodeTest extends DaGdTest {
  private $expected_code;

  public function __construct($test_path, $expected_code) {
    $this->path = $test_path;
    $this->expected_code = $expected_code;
  }

  public function run() {
    $this->retrieve();
    $match = preg_match('@'.$this->expected_code.'@', $this->response);
    return $this->test(
      $match,
      'give correct HTTP Response code ('.$this->expected_code.' ?= '.
        $this->response.')',
      $this->response);
  }
}
