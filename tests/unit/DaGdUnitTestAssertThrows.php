<?php

final class DaGdUnitTestAssertThrows_basic extends DaGdUnitTestCallback {
  public function run() {
    throw new Exception('foo');
  }
}

final class DaGdUnitTestAssertThrows extends DaGdUnitTest {
  public function run() {
    $this->path = 'DaGdUnitTestAssertThrows';
    $this->assertThrows('Exception', new DaGdUnitTestAssertThrows_basic());
  }
}
