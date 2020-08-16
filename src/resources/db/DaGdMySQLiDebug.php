<?php

/**
 * This is a subclass of mysqli with some added debug features. It is a direct
 * drop-in replacement (and used as such in our entrypoint). It tracks each
 * each query that was run and uses DaGdMySQLiStmtDebug to automatically time
 * its execution.
 *
 * Due to the use of prepared statements, we do not have (easy) access to the
 * bound prepared query. We instead track the unbounded form. This has an added
 * benefit of not leaking data if the debugger ever somehow gets enabled in
 * production environments.
 *
 * NOTE: We currently only override prepare() here. Any application which uses
 * something like mysqli::real_query() or mysqli::query() will not have its
 * queries logged here.
 */
class DaGdMySQLiDebug extends mysqli {
  private $queries = array();

  public function getQueries() {
    return $this->queries;
  }

  public function prepare($query) {
    $stmt = new DaGdMySQLiStmtDebug($this, $query);
    $this->queries[] = $stmt;
    return $stmt;
  }
}
