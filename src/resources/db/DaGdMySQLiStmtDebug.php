<?php

/**
 * This is a drop-in debug replacement for mysqli_stmt. It automatically times
 * execute() and stores the resulting query times.
 */
class DaGdMySQLiStmtDebug extends mysqli_stmt {
  private $query;
  private $milliseconds;

  public function __construct(mysqli $mysqli, $query) {
    $this->query = $query;
    parent::__construct($mysqli, $query);
  }

  public function getQuery() {
    return $this->query;
  }

  public function getMilliseconds() {
    return $this->milliseconds;
  }

  public function execute() {
    $start = microtime(true);
    $res = parent::execute();
    $end = microtime(true);
    $this->milliseconds = ($end - $start) * 1000;
    $this->store_result();
    return $res;
  }
}
