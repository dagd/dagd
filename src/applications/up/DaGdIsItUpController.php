<?php

require_once dirname(__FILE__).'/resources/IsItUpQuery.php';

final class DaGdIsItUpController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'up',
      'summary' => 'Determine whether or not a site is up.',
      'path' => 'up',
      'examples' => array(
        array(
          'arguments' => array('google.com'),
          'summary' => 'Get the HTTP response code for the given site'),
        array(
          'arguments' => array('elrod.me'),
          'summary' => 'Get the HTTP response code, not following redirects',
          'request' => array(
            'redirects' => '0',
          )
        ),
        array(
          'arguments' => array('google.com'),
          'summary' => 'Get the HTTP response code, verifying SSL peers',
          'request' => array(
            'sslverify' => '1',
          )
        ),
      ),
    );
  }

  public function execute(DaGdResponse $response) {
    $url = $this->getRequest()->getRouteMatches()[1];
    $verbose = $this->getRequest()->param('verbose', false, true, true);
    $sslverify = $this->getRequest()->param('sslverify', false, true, true);
    $redirects = $this->getRequest()->param('redirect', true, true, true);
    if ($sslverify && !preg_match('#^https?://#i', $url)) {
      $url = 'https://'.$url;
    }
    $up = new IsItUpQuery($url);
    return $up->query($verbose, $sslverify, $redirects);
  }
}
