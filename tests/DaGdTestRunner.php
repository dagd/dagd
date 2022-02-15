<?php

require_once dirname(__FILE__).'/constants.php';

class DaGdTestRunner {
  private $concurrency = 5;
  private $pids = array();
  private $tests = array();
  private $last_started = -1;
  private $return_code = 0;
  private $base_url = '';
  private $groups_filter = array();
  private $groups_exclude_filter = array();
  private $run_preparatory = true;
  private $results_callback;

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

  public function setGroupsFilter(array $groups) {
    $this->groups_filter = $groups;
    return $this;
  }

  public function addGroupFilter($group) {
    $this->groups_filter[] = $group;
    return $this;
  }

  public function setGroupsExcludeFilter(array $groups) {
    $this->groups_exclude_filter = $groups;
    return $this;
  }

  public function addGroupExcludeFilter($group) {
    $this->groups_exclude_filter[] = $group;
    return $this;
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

  /**
   * If true, skip preparatory tests/tasks.
   *
   * This might cause certain tests, but can speed up test groups if you know
   * that they aren't dependent on them.
   */
  public function setRunPreparatory($run_preparatory) {
    $this->run_preparatory = $run_preparatory;
    return $this;
  }

  public function getRunPreparatory() {
    return $this->run_preparatory;
  }

  public function setResultsCallback($results_callback) {
    $this->results_callback = $results_callback;
    return $this;
  }

  public function getResultsCallback() {
    return $this->results_callback;
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

  public function run() {
    // Run preparatory tests first. These have to be done in a particular
    // order, so we can't just throw them to the dogs...er, child processes.
    // We run the preparatory tests even when filters are active. This is to
    // avoid having to implement a notion of 'test group dependencies.'
    $remaining_tests = array();
    foreach ($this->tests as $test) {
      // While we're here, also tell the test who is running it.
      // This is mainly so we can access test-global configuration such as the
      // base URL.
      $test->setRunner($this);

      // Also set the test's callback, so it knows how to output results.
      $test->setResultsCallback($this->getResultsCallback());

      if ($test->getPreparatory() && $this->getRunPreparatory()) {
        $rc = $test->run();
        $this->handleRC($rc);
      } else {
        if (empty($this->groups_filter) &&
            empty($this->groups_exclude_filter)) {
          $remaining_tests[] = $test;
        } else {
          $test_groups = $test->getGroups();

          // First, see if we have an excluded group
          foreach ($test_groups as $group) {
            if (in_array($group, $this->groups_exclude_filter)) {
              continue;
            }
          }

          // Now, iterate again and mark the test as wanted if it is.
          foreach ($test_groups as $group) {
            if (in_array($group, $this->groups_filter)) {
              $remaining_tests[] = $test;
              break;
            }
          }
        }
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
          $rc = $remaining_tests[$this->last_started]->run();
          exit($rc);
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
