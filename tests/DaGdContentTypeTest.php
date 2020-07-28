<?php

final class DaGdContentTypeTest extends DaGdTest {
  private $expected_mimetype;

  public function __construct($test_path, $expected_mimetype) {
    $this->path = $test_path;
    $this->expected_mimetype = $expected_mimetype;
  }

  public function run() {
    $this->retrieve();
    $headers = $this->getHeaders();
    $correctness = strstr($headers['Content-Type'], $this->expected_mimetype);
    return $this->test(
      $correctness,
      'return correct mimetype ('.$this->expected_mimetype.')',
      $headers['Content-Type']);
  }
}
