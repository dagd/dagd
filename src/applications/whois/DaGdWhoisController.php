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
    $query = $request->getRouteComponent('query');
    if ($query === null) {
      $query = $request->getClientIP();
    }
    $whois_client = new DaGdWhois($query);
    $result = $whois_client->performQuery();
    $this->addDebugCard(
      id(new DaGdCard())
        ->setTitle('whois referral trace')
        ->setBody(tag('pre', $result['trace'])));
    return $result['data'];
  }
}
