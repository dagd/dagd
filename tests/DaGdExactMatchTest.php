<?php

final class DaGdExactMatchTest extends DaGdTest {
  private $match;
  private $invert;

  public function __construct($test_path, $match, $invert = false) {
    $this->path = $test_path;
    $this->match = $match;
    $this->invert = $invert;
  }

  public function run() {
    $content = $this->retrieve(true);
    if ($this->invert) {
      return $this->test(
        $content !== $this->match,
        'must NOT be exactly: '.$this->match,
        $content);
    } else {
      return $this->test(
        $content === $this->match,
        'must match exactly be: '.$this->match,
        $content);
    }
  }
}
