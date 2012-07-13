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
    if (array_key_exists('noipv6', $_REQUEST)) {
      $records = dns_get_record($this->route_matches[1], DNS_A);
    } else {
      $records = dns_get_record($this->route_matches[1], DNS_A + DNS_AAAA);
    }

    $ips = array();

    foreach ($records as $record) {
      if (array_key_exists('ip', $record)) {
        $ips[] = $record['ip'];
      } elseif (array_key_exists('ipv6', $record)) {
        $ips[] = $record['ipv6'];
      }
    }

    if (!empty($ips)) {
      echo implode(', ', $ips);
    } else {
      echo 'No IPs found for given hostname.';
    }
  }
}
