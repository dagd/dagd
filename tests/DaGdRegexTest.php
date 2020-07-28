<?php

final class DaGdRegexTest extends DaGdTest {
  private $regex;
  private $invert;

  public function __construct($test_path, $pattern, $invert = false) {
    $this->path = $test_path;
    $this->regex = $pattern;
    $this->invert = $invert;
  }

  public function run() {
    $content = $this->retrieve(true);
    if ($this->invert) {
      $match = !preg_match($this->regex, $content);
      return $this->test(
        $match,
        'must NOT match pattern: '.$this->regex,
        $content);
    } else {
      $match = preg_match($this->regex, $content);
      return $this->test(
        $match,
        'must match pattern: '.$this->regex,
        $content);
    }
  }
}
