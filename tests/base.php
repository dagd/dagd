<?php

require_once dirname(__FILE__).'/constants.php';

abstract class DaGdTest {
  protected $path;
  protected $response;
  protected $headers;
  protected $tolerate_failure;
  protected $accept = '*/*';
  private $original_user_agent;
  private $preparatory = false;

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

  public function setUserAgent($user_agent) {
    $this->original_user_agent = ini_get('user_agent');
    ini_set('user_agent', $user_agent);
    return $this;
  }

  public function setAccept($type) {
    $this->accept = $type;
    return $this;
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
}
