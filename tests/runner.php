<?php

require_once dirname(__FILE__).'/constants.php';

class DaGdTestRunner {
  private $concurrency = 5;
  private $pids = array();
  private $tests = array();
  private $last_started = -1;
  private $return_code = 0;
  private $base_url = '';

  private $passes = 0;
  private $tolerated_failures = 0;
  private $failures = 0;

  public function getPasses() {
    return $this->passes;
  }

  public function getToleratedFailures() {
    return $this->tolerated_failures;
  }

  public function getFailures() {
    return $this->failures;
  }

  public function getReturnCode() {
    return $this->return_code;
  }

  public function arm($test) {
    $this->tests[] = $test;
    return $this;
  }

  public function setConcurrency($int) {
    $this->concurrency = $int;
    return $this;
  }

  public function setBaseUrl($url) {
    $this->base_url = $url;
    return $this;
  }

  public function getBaseUrl() {
    return $this->base_url;
  }

  private function handleRC($rc) {
    switch ($rc) {
    case SUCCESS:
      $this->passes++;
      break;
    case TOLERATED_FAILURE:
      $this->tolerated_failures++;
      break;
    default:
      $this->return_code = 1;
      $this->failures++;
    }
  }

  public function run($start_test = 0) {
    // Run preparatory tests first. These have to be done in a particular
    // order, so we can't just throw them to the dogs...er, child processes.
    $remaining_tests = array();
    foreach ($this->tests as $test) {
      // While we're here, also tell the test who is running it.
      // This is mainly so we can access test-global configuration such as the
      // base URL.
      $test->setRunner($this);
      if ($test->getPreparatory()) {
        $rc = $test->run();
        $this->handleRC($rc);
      } else {
        $remaining_tests[] = $test;
      }
    }

    while (true) {
      while (count($this->pids) < $this->concurrency) {
        $this->last_started++;
        if ($this->last_started == count($remaining_tests)) {
          // We can't return here because we still need to wait for the last
          // of the threads to clean up. But we can break out, do that final
          // cleanup, and then return.
          break;
        }
        $pid = pcntl_fork();
        if ($pid === -1) {
          echo 'Could not fork child thread.';
          exit(1);
        }
        if ($pid === 0) {
          $exit = SUCCESS;
          $rc = $remaining_tests[$this->last_started]->run();
          if ($rc != SUCCESS) {
            $exit = $rc;
          }
          exit($exit);
        } else {
          $this->pids[] = $pid;
        }
      }

      foreach ($this->pids as $idx => $pid) {
        $status = null;
        pcntl_waitpid($pid, $status);
        $rc = pcntl_wexitstatus($status);
        $this->handleRC($rc);
        unset($this->pids[$idx]);
      }

      if ($this->last_started == count($remaining_tests)) {
        return $this->return_code;
      }
    }
  }
}
