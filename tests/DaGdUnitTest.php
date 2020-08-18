<?php

abstract class DaGdUnitTest extends DaGdTest {
  private $tests = array();

  public function __construct() {
    $this->path = 'unit';
  }

  abstract public function runUnits();

  // This runs the tests in runUnits() and returns the worst result.
  // This allows unit tests to have multiple assertions, at the cost of test
  // result counters being "inaccurate" as each unit test class is considered by
  // the runner to be one test.
  public function run() {
    // runUnits() side effects in two ways: it will print the status line, but
    // also update $tests as it goes.
    $this->runUnits();
    return $this->getWorstResult($this->tests);
  }

  protected function retrieve($ignore_errors = false) {
    throw new Exception('Unit tests should not call retrieve()');
  }

  protected function assertTrue($condition, $summary, $debug = '') {
    $this->tests[] = $this->test($condition, $summary, $debug);
  }

  protected function assertFalse($condition, $summary, $debug = '') {
    $this->tests[] = $this->test(!$condition, $summary, $debug);
  }

  protected function assertThrows($ex_name, DaGdUnitTestCallback $cb) {
    $ret = false;
    $msg = 'No exception thrown';
    try {
      $cb->run();
    } catch (Exception $ex) {
      $cls = get_class($ex);
      $msg = 'Threw exception `'.$cls.'`';
      if ($cls == $ex_name) {
        $ret = true;
      }
    }
    $this->tests[] = $this->test($ret, 'Expected exception `'.$ex_name.'`', $msg);
  }
}
