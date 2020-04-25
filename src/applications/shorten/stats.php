<?php

final class DaGdStatsController extends DaGdBaseClass {
  public function getHelp() {
    return array(
      'title' => 'stats',
      'summary' => 'Display basic stats for a short url.',
      'path' => 'stats',
      'examples' => array(
        array(
          'arguments' => array('g'),
          'summary' => 'An example short URL with a custom suffix'),
      ));
  }

  protected $wrap_html = true;
  protected $wrap_pre = true;

  public function render() {
    $ctrl = id(new DaGdShortenController())
      ->setRouteMatches($this->route_matches)
      ->setReadDB($this->getReadDB())
      ->setWriteDB($this->getWriteDB());
    $stats = $ctrl->getStatsForURL($this->route_matches[1]);
    if ($stats === null) {
      return error404();
    }
    $resp = '';
    foreach ($stats as $k => $v) {
      $resp .= $k.': '.$v."\n";
    }
    return $resp;
  }
}
