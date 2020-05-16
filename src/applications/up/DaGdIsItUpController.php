<?php

require_once dirname(__FILE__).'/resources/IsItUpQuery.php';

final class DaGdIsItUpController extends DaGdBaseClass {
  public function getHelp() {
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

  public function configure() {
    return $this
      ->setWrapHtml(true);
  }

  public function render() {
    $url = $this->route_matches[1];
    $verbose = request_or_default('verbose', false, true, true);
    $sslverify = request_or_default('sslverify', false, true, true);
    $redirects = request_or_default('redirect', true, true, true);
    if ($sslverify && !preg_match('#^https?://#i', $url)) {
      $url = 'https://'.$url;
    }
    $up = new IsItUpQuery($url);
    return $up->query($verbose, $sslverify, $redirects);
  }
}
