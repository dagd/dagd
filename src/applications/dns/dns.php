<?php

final class DaGdDNSController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Fetch and return all DNS records for a given hostname.',
    'path' => 'dns',
    'examples' => array(
      array(
        'summary' => 'Get DNS records for google.com',
        'arguments' => array(
          'google.com',
        ),
      ),
    ));

  private $dns;

  private function append_if_exists($string, $array, $key) {
    if (idx($array, $key)) {
      return $string.$array[$key].' ';
    } else {
      return $string;
    }
  }

  public function render() {
    $hostname = $this->route_matches[1];
    $this->dns = dns_get_record($hostname);

    $records = '';

    // record ttl class type value
    foreach ($this->dns as $record) {
      if ($record['type'] == 'SOA') {
        continue;
      }

      $records .= $record['host'].' ';
      $records .= $record['ttl'].' ';
      $records .= $record['class'].' ';
      $records .= $record['type'].' ';

      $records = $this->append_if_exists($records, $record, 'pri');
      $records = $this->append_if_exists($records, $record, 'weight');
      $records = $this->append_if_exists($records, $record, 'target');
      $records = $this->append_if_exists($records, $record, 'port');
      $records = $this->append_if_exists($records, $record, 'ip');
      $records = $this->append_if_exists($records, $record, 'ipv6');
      $records = $this->append_if_exists($records, $record, 'txt');
      $records .= "\n";
    }
    return $records;
  }
}
