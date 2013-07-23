<?php
require_once dirname(__FILE__).'/../whois/resources/dagd_whois.php';

final class DaGdISPController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Return the name of your ISP, or that of the given IP.',
    'path' => 'isp',
    'examples' => array(
      array(
        'summary' => 'Return the name of your ISP'),
      array(
        'arguments' => array('69.171.234.21'),
        'summary' => 'Return the name of the ISP of the given address.'),
    ));

  public function render() {
    if (count($this->route_matches) > 1) {
      $query = $this->route_matches[1];
    } else {
      $query = client_ip();
    }

    $whois_client = new DaGdWhois($query);
    $response = $whois_client->performQuery();
    if (preg_match('/Org\-?Name: ?(.+)/', $response, $org_matches)) {
      return trim($org_matches[1]);
    }
    return 'ISP could not be found.';
  }
}
