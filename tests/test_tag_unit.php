<?php

final class DaGdTagUnitTest extends DaGdTest {
  private $tag;
  private $expected_output;

  public function __construct($tag, $expected_output) {
    $this->tag = $tag;
    $this->expected_output = $expected_output;

    // This is really just a label
    $this->path = 'Tag unit test';
  }

  public function run() {
    $real_output = $this->tag->renderSafe();
    return $this->test(
      $real_output === $this->expected_output,
      'return generate HTML correctly ('.$this->expected_output.')',
      $real_output);
  }
}
