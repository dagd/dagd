<?php

final class DaGdHeaderRegexTest extends DaGdTest {
  private $regex;
  private $invert;

  public function __construct(
    $test_path,
    $header_key,
    $pattern,
    $invert = false) {

    $this->path = $test_path;
    $this->header_key = $header_key;
    $this->regex = $pattern;
    $this->invert = $invert;
  }

  public function run() {
    $content = $this->retrieve(true);
    $headers = $this->getHeaders();

    if (!isset($headers[$this->header_key])) {
      return $this->test(false, 'header '.$this->header_key.' must exist');
    }

    $value = $headers[$this->header_key];

    if ($this->invert) {
      $match = !preg_match($this->regex, $value);
      return $this->test(
        $match,
        'header '.$this->header_key.' must NOT match: '.$this->regex,
        $value);
    } else {
      $match = preg_match($this->regex, $value);
      return $this->test(
        $match,
        'header '.$this->header_key.' must match: '.$this->regex,
        $value);
    }
  }
}
