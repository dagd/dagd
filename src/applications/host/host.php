<?php
final class DaGdHostController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Return the IP or hostname of the given value.',
    'path' => 'host',
    'examples' => array(
      array(
        'arguments' => array('google.com'),
        'summary' => 'Get a comma-separated list of IPs for google.com'),
      array(
        'arguments' => array('127.0.0.1'),
        'summary' => 'Get the hostname for the PTR of 127.0.0.1')
    ));
  
  public function render() {
    if (filter_var($this->route_matches[1], FILTER_VALIDATE_IP)) {
      // Treat it like an IP.
      return gethostbyaddr($this->route_matches[1]);
    } else {
      // Treat it like a hostname.
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
        return implode(', ', $ips);
      } else {
        return 'No IPs found for given hostname.';
      }
    }
  }
}
