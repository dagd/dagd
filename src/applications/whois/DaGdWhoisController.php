<?php
require_once dirname(__FILE__).'/resources/dagd_whois.php';

final class DaGdWhoisController extends DaGdBaseClass {
  public function getHelp() {
    return array(
      'title' => 'whois',
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
  }

  public function configure() {
    return $this
      ->setWrapHtml(true);
  }

  public function render() {
    $query = $this->route_matches[1];
    $trace = request_or_default('trace', false, true, true);
    $whois_client = new DaGdWhois($query, $trace);
    return $whois_client->performQuery();
  }
}
