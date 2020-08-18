<?php

require_once dirname(__FILE__).'/constants.php';

abstract class DaGdTest {
  protected $path;
  protected $response;
  protected $headers;
  protected $tolerate_failure;
  protected $accept = '*/*';
  protected $method = 'GET';
  private $original_user_agent;
  private $preparatory = false;
  private $groups = array('default');

  abstract public function run();

  public function setTolerateFailure($tolerate_failure) {
    $this->tolerate_failure = $tolerate_failure;
    return $this;
  }

  public function setPreparatory($is_preparatory) {
    $this->preparatory = $is_preparatory;
    return $this;
  }

  public function getPreparatory() {
    return $this->preparatory;
  }

  public function setGroups(array $groups) {
    $this->groups = $groups;
    return $this;
  }

  public function getGroups() {
    return $this->groups;
  }

  public function addGroup($group) {
    $this->groups[] = $group;
    return $this;
  }

  public function setUserAgent($user_agent) {
    $this->original_user_agent = ini_get('user_agent');
    ini_set('user_agent', $user_agent);
    return $this;
  }

  public function setAccept($type) {
    $this->accept = $type;
    return $this;
  }

  public function setMethod($method) {
    $this->method = $method;
    return $this;
  }

  public function getMethod() {
    return $this->method;
  }

  public function setRunner($runner) {
    $this->runner = $runner;
    return $this;
  }

  public function getRunner() {
    return $this->runner;
  }

  protected function fail($text, $output = '') {
    if ($this->tolerate_failure) {
      echo chr(27)."[1;33m".'*** (tolerable failure) '.$text.' ***'.chr(27)."[0m"."\n";
    } else {
      echo chr(27)."[1;31m".'*** '.$text.' ***'.chr(27)."[0m"."\n";
    }
    if ($output != '') {
      echo '    Output was: '.chr(27)."[1;35m".$output.chr(27)."[0m"."\n";
    }
  }

  protected function pass($text) {
    echo chr(27)."[1;32m".'*** '.$text.' ***'.chr(27)."[0m"."\n";
  }

  protected function retrieve($ignore_errors = false) {
    $context = stream_context_create(
      array(
        'http' => array(
          'method' => $this->method,
          'ignore_errors' => $ignore_errors,
          'follow_location' => false,
          'header' => array(
            'Accept: '.$this->accept."\r\n",
          ),
        ),
      )
    );

    $base_url = $this->getRunner()->getBaseUrl();
    $obtain = @file_get_contents($base_url.$this->path, false, $context);

    $this->response = $http_response_header[0];

    foreach (array_slice($http_response_header, 1) as $header) {
      $header_split = explode(': ', $header, 2);
      if (count($header_split) > 1) {
        list($name, $value) = $header_split;
        $this->headers[$name] = $value;
      }
    }

    if ($this->original_user_agent) {
      ini_set('user_agent', $this->original_user_agent);
    }
    return $obtain;
  }

  protected function getHeaders() {
    if (!$this->headers) {
      throw new Exception(
        'Call retrieve() before calling getHeaders()!');
    }
    return $this->headers;
  }

  protected function getResponse() {
    if (!$this->response) {
      throw new Exception(
        'Call retrieve() before calling getResponse()!');
    }
    return $this->response;
  }

  protected function test($condition, $summary, $output = '') {
    if ($condition) {
      $this->pass('Test PASSED ['.$this->path.']: '.$summary);
      return SUCCESS;
    } else {
      $this->fail('Test FAILED ['.$this->path.']: '.$summary, $output);
      if ($this->tolerate_failure) {
        return TOLERATED_FAILURE;
      } else {
        return FAILURE;
      }
    }
  }

  public static function getWorstResult(array $results) {
    if (empty($results)) {
      throw new Exception('results must not be empty');
    }

    $worst = $results[0];

    foreach ($results as $result) {
      // If we are at a FAILURE or we have seen a FAILURE (in the initial value
      // above), then return FAILURE outright.
      if ($worst === FAILURE || $result === FAILURE) {
        return FAILURE;
      }

      // Otherwise, if the worst we've seen is a SUCCESS, then update to the
      // current result (either SUCCESS or TOLERATED_FAILURE).
      //
      // $worst will never be FAILURE here because of the above if, and if it is
      // TOLERATED_FAILURE, we want to keep that.
      if ($worst === SUCCESS) {
        $worst = $result;
      }
    }
    return $worst;
  }
}
