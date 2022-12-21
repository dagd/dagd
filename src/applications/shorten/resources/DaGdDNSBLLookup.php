<?php

require_once 'Net/DNS2.php';

final class DaGdDNSBLLookup implements DaGdCacheMissCallback {
  private $domain;

  public function __construct($domain) {
    $this->domain = $domain;
  }

  public function run($key = null) {
    statsd_bump('shorturl_blacklist_query_dnsbl');
    return $this->query($this->domain);
  }

  // Queries all dnsbl servers in the config. Returns true if the domain is
  // acceptable, false otherwise.
  private function query($domain) {
    $dnsbl_servers = DaGdConfig::get('shorten.dnsbl');
    $dnsbl_query_via = DaGdConfig::get('shorten.blacklist_via');

    foreach ($dnsbl_servers as $suffix) {
      $resolver = new Net_DNS2_Resolver(
        array(
          'nameservers' => $dnsbl_query_via,
        )
      );

      try {
        $response = $resolver->query($domain.$suffix);
        // If an IP is being shortened, we ignore the bad response from
        // the dnsbl.
        if ($response->answer[0]->address != '127.0.1.255') {
          // The response is NOT that we are being called out for checking an
          // IP. That means it's a legitimate response, and $domain is BAD.
          // Return false meaning that the domain should NOT be trusted.
          return array(
            'safe' => false,
            'source' => $suffix,
          );
        }
      } catch (Net_DNS2_Exception $e) {
        if ($e->getCode() == Net_DNS2_Lookups::RCODE_NXDOMAIN) {
          // This is where we would handle a "success"
        }
      }
    }
    return array(
      'safe' => true,
    );
  }
}
