<?php
final class DaGdHostController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'host',
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
  }

  public function execute(DaGdResponse $response) {
    $query = $this->getRequest()->getRouteMatches()[1];
    if (filter_var($query, FILTER_VALIDATE_IP)) {
      // Treat it like an IP.
      return gethostbyaddr($query);
    } else {
      // Treat it like a hostname.
      $noipv6 = $this
        ->getRequest()
        ->getParamOrDefault('noipv6', false, true, true);
      if ($noipv6) {
        $records = dns_get_record($query, DNS_A);
      } else {
        $records = dns_get_record($query, DNS_A + DNS_AAAA);
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
