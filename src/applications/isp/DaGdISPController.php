<?php

final class DaGdISPController extends DaGdController {
  public static function getHelp() {
    return id(new DaGdHelp())
      ->setTitle('isp')
      ->setDescription('Returns the name of your ISP, or that of the given IP.')
      ->addPath(
        id(new DaGdHelpPath())
          ->setPath('isp')
          ->setMethods(array('GET'))
          ->addExample(
            id(new DaGdHelpExample())
              ->setCommentary(
                'Return the name of your ISP.')))
      ->addPath(
        id(new DaGdHelpPath())
          ->setPath('isp')
          ->addPathArg('ip')
          ->setMethods(array('GET'))
          ->addExample(
            id(new DaGdHelpExample())
              ->addPathArg('69.171.234.21')
              ->setCommentary(
                'Return the name of the ISP of the given address.')));
  }

  public function execute(DaGdResponse $response) {
    $query = $this->getRequest()->getRouteComponent('ip', $this->getRequest()->getClientIP());
    $whois_client = new DaGdWhois($query);
    $response = $whois_client->performQuery();
    if (preg_match(
      // NOTE: Later matches will win
      '/(?:CustName|descr|Org\-?Name|OrgTechName|Organization|contact:Name|owner)(?:;I|): ?(.+)/i',
      $response['data'],
      $org_matches)) {
      return trim($org_matches[1]);
    }
    return 'ISP could not be found.';
  }
}
