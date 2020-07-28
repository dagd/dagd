<?php

final class DaGdResponseRegexTest extends DaGdTest {
  private $regex;
  private $invert;

  public function __construct(
    $test_path,
    $pattern,
    $invert = false) {

    $this->path = $test_path;
    $this->regex = $pattern;
    $this->invert = $invert;
  }

  public function run() {
    $content = $this->retrieve(true);
    $response = $this->getResponse();

    $message = 'response header must match: '.$this->regex;
    $match = preg_match($this->regex, $response);

    if ($this->invert) {
      $match = !$match;
      $message = 'response header must NOT match: '.$this->regex;
    }

    return $this->test(
      $match,
      $message,
      $response);
  }
}
