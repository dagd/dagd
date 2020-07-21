<?php

final class DaGdISPController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'isp',
      'summary' => 'Return the name of your ISP, or that of the given IP.',
      'path' => 'isp',
      'examples' => array(
        array(
          'summary' => 'Return the name of your ISP'),
        array(
          'arguments' => array('69.171.234.21'),
          'summary' => 'Return the name of the ISP of the given address.'),
      ));
  }

  public function execute(DaGdResponse $response) {
    $query = $this->getRequest()->getRouteComponent(1, client_ip());
    $whois_client = new DaGdWhois($query);
    $response = $whois_client->performQuery();
    if (preg_match(
      // NOTE: Later matches will win
      '/(?:CustName|descr|Org\-?Name|OrgTechName|Organization|contact:Name|owner)(?:;I|): ?(.+)/i',
      $response,
      $org_matches)) {
      return trim($org_matches[1]);
    }
    return 'ISP could not be found.';
  }
}
