<?php

require_once dirname(__FILE__).'/resources/IsItUpQuery.php';

final class DaGdIsItUpController extends DaGdBaseClass {
  public $__help__ = array(
    'title' => 'up',
    'summary' => 'Determine whether or not a site is up.',
    'path' => 'up',
    'examples' => array(
      array(
        'arguments' => array('google.com'),
        'summary' => 'Get the HTTP response code for the given site'),
    ));

  protected $wrap_html = true;

  public function render() {
    $up = new IsItUpQuery($this->route_matches[1]);
    $verbose = request_or_default('verbose', false, true, true);
    return $up->query($verbose);
  }
}