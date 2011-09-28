<?php
require_once dirname(__FILE__).'/resources/dagd_whois.php';

final class DaGdWhoisController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Whois a given domain or IP address.',
    'path' => 'w',
    'examples' => array(
      array(
        'arguments' => array('google.com'),
        'summary' => null),
      array(
        'arguments' => array('127.0.0.1'),
        'summary' => null),
    ));
   
  public function render() {
    $query = $this->route_matches[1];
    $whois_client = new DaGdWhois($query);
    return $whois_client->performQuery();
  }
}
