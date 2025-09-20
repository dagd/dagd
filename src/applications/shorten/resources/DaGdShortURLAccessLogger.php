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
  public function log() {
    $id = $this->surl->getId();
    $query = $this
      ->controller
      ->getWriteDB()
      ->prepare(
        'INSERT INTO shorturl_access_buckets '.
        '(shorturl_id, access_hour, access_count) '.
        'VALUES (?, DATE_FORMAT(NOW(), \'%Y-%m-%d %H:00:00\'), 1) '.
        'ON DUPLICATE KEY UPDATE access_count = access_count + 1');
    $query->bind_param(
      'i',
      $id);

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
