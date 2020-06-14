<?php

final class DaGdShortURLAccessLogger {
  private $controller;
  private $surl;

  public function __construct(DagdController $controller, DaGdShortURL $surl) {
    $this->controller = $controller;
    $this->surl = $surl;
  }

  /**
   * @return boolean - true if successful, false if not.
   */
  public function log($useragent) {
    $id = $this->surl->getId();
    $query = $this
      ->controller
      ->getWriteDB()
      ->prepare(
        'INSERT INTO shorturl_access(shorturl_id, useragent) VALUES(?,?)');
    $query->bind_param(
      'is',
      $id,
      $useragent);

    $start = microtime(true);
    $res = $query->execute();
    $end = microtime(true);
    statsd_time('query_time_LogURLAccess', ($end - $start) * 1000);

    if ($res) {
      return true;
    } else {
      return false;
    }
  }
}
