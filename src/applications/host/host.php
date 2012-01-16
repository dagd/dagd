<?php
final class DaGdHostController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Returns the IP of the given hostname, separated by ", ".',
    'path' => 'host',
    'examples' => array(
      array(
        'arguments' => array('google.com'),
        'summary' => 'Get a comma-separated list of IPs for google.com'),
    ));
  
  public function render() {
    $ips = gethostbynamel($this->route_matches[1]);
    if (!empty($ips)) {
      echo implode(', ', $ips);
    } else {
      echo 'No IPs found for given hostname.';
    }
  }
}
