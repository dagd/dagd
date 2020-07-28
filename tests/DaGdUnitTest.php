<?php

require_once dirname(__FILE__).'/base.php';

abstract class DaGdUnitTest extends DaGdTest {
  public function __construct() {
    $this->path = 'unit';
  }

  abstract public function run();

  protected function retrieve($ignore_errors = false) {
    throw new Exception('Unit tests should not call retrieve()');
  }

  protected function assertTrue($condition, $summary, $debug = '') {
    return $this->test($condition, $summary, $debug);
  }

  protected function assertFalse($condition, $summary, $debug = '') {
    return $this->test(!$condition, $summary, $debug);
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
    return $this->test($ret, 'Expected exception `'.$ex_name.'`', $msg);
  }
}
