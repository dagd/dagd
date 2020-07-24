<?php

final class DaGdWhoisController extends DaGdController {
  public static function getHelp() {
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

  public function execute(DaGdResponse $response) {
    $request = $this->getRequest();
    $query = $request->getRouteComponent(1);
    $trace = $request->param('trace', false, true, true);
    $whois_client = new DaGdWhois($query, $trace);
    return $whois_client->performQuery();
  }
}
