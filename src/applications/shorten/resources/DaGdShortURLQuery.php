<?php

/**
 * Various  kinds of read-only queries about shorturls.
 */
final class DaGdShortURLQuery {
  private $controller;

  public function __construct($controller) {
    $this->controller = $controller;
  }

  /**
   * @return DaGdShortURL or null
   */
  public function fromShort($short_url) {
    $id = null;
    $long_url = null;

    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT id, longurl FROM shorturls WHERE shorturl=? AND enabled=1');
    $query->bind_param('s', $short_url);
    $start = microtime(true);
    $query->execute();
    $end = microtime(true);
    statsd_time('query_time_getLongURL', ($end - $start) * 1000);
    $query->bind_result($id, $long_url);
    $query->fetch();
    $query->close();

    if (!empty($id) && !empty($long_url)) {
      return id(
        new DaGdShortURL($this->controller, $id, $short_url, $long_url));
    }

    return null;
  }

  /**
   * @return DaGdShortURL or null
   */
  private function fromHash($longurl_hash) {
    $id = null;
    $long_url = null;
    $short_url = null;

    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT id, longurl, shorturl FROM shorturls WHERE longurl_hash=? AND '.
        'enabled=1 AND custom_shorturl=0 ORDER BY id DESC LIMIT 1');
    $query->bind_param('s', $longurl_hash);
    $start = microtime(true);
    $query->execute();
    $end = microtime(true);
    statsd_time('query_time_getNonCustomShortURL', ($end - $start) * 1000);
    $query->bind_result($id, $long_url, $short_url);
    $query->fetch();
    $query->close();

    if (!empty($id) && !empty($long_url) && !empty($short_url)) {
      return id(
        new DaGdShortURL($this->controller, $id, $short_url, $long_url));
    }

    return null;
  }


  /**
   * @return boolean
   */
  private function isFreeShortURL($short_url) {
    $count = 0;
    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT COUNT(*) FROM shorturls WHERE shorturl=?');
    $query->bind_param('s', $short_url);
    $query->execute();
    $query->bind_result($count);
    $query->fetch();
    $query->close();

    // If we do *NOT* get one above (!), then it is free, and we return true.
    return !(bool)$count;
  }

  // TODO: Maybe pull this out to something a bit more general so other apps can
  // make use of it some day.
  /**
   * @return boolean
   */
  private function isBannedIP($ip) {
    $count = 0;
    $query = $this
      ->controller
      ->getReadDB()
      ->prepare(
        'SELECT COUNT(*) FROM blocked_ips WHERE '.
        'inet6_aton(?) between ip_start and ip_end');
    $query->bind_param('s', $ip);
    $query->execute();
    $query->bind_result($count);
    $query->fetch();
    $query->close();

    // Return true if the author is banned, false if not.
    return (bool)$count;
  }

}
