<?php

require_once dirname(__FILE__).'/../../../resources/global_resources.php';

class DaGdWhois {
  private $domain;
  private $query = '';
  private $whois_server;
  private $whois_port = 43;
  private $skip_detail = false;

  public function __construct($domain) {
    $this->domain = $domain;
  }

  /*
   * Given a domain (passed to the constructor), we need to use the tld of it
   * to connect to <tld>.whois-servers.net to get the real whois server.
   * Using this, we end up making two whois queries, but it's the cleanest
   * way to do this.
   *
   * @returns <string> the tld of the domain passed to the constructor.
   */
  private function tld() {
    $tld = explode('.', $this->domain);
    $tld = end($tld);
    return $tld;
  }

  /*
   * Look up the proper whois server for a domain, given its tld.
   * Do this by connecting to <tld>.whois-servers.net, and asking them,
   * and store the result in $this->whois_server.
   *
   * @returns <bool> true if successful, false if not.
   */
  public function fetchWhoisServer() {
    $hardcoded_tld_map = DaGdConfig::get('whois.hardcode_map');
    if (array_key_exists($this->tld(), $hardcoded_tld_map)) {
      $custom_tld = $hardcoded_tld_map[$this->tld()];

      // We can have a custom query without having a custom server...
      if (array_key_exists('query', $custom_tld)) {
        $this->query = $custom_tld['query'].' ';
      }

      // But if we specify our own server, there's no point in looking for
      // a different one.
      if (array_key_exists('server', $custom_tld)) {
        $this->whois_server = $custom_tld['server'];
        return true;
      }
    }

    $transient_sock = null;
    if (filter_var($this->domain, FILTER_VALIDATE_IP)) {
      $default = DaGdConfig::get('whois.transient_server');

      $default_server = $default['server'];
      $default_port = 43;

      if (strpos($default_server, ':') !== false) {
        list($default_server, $default_port) = explode(':', $default_server, 2);
      }

      $transient_sock = fsockopen($default_server, $default_port);
      if (!$transient_sock) {
        return false;
      }
      fwrite($transient_sock, $default['query'].' '.$this->domain."\r\n");
    } else {
      $transient_sock = fsockopen($this->tld().'.whois-servers.net', 43);
      if (!$transient_sock) {
        return false;
      }
      fwrite($transient_sock, 'domain '.$this->domain."\r\n");
    }

    $whois_server = null;
    $whois_info = '';
    while (!feof($transient_sock)) {
      $line = fgets($transient_sock);
      $referral = preg_match(
        '#(?:Whois Server|ReferralServer): (.*)#i',
        $line,
        $whois_server);

      // This can't be easy because there's an edge case where the referral
      // server doesn't exist, so after parsing we get a simple "\r" back.
      $referral_server_name = null;
      if (!empty($whois_server) && count($whois_server) > 0) {
        $referral_server_name = trim($whois_server[1]);
      }
      if (!empty($referral) &&
          !empty($whois_server) &&
          !empty($referral_server_name)) {
        break;
      }
      $whois_info .= $line;
    }
    fclose($transient_sock);
    if ($whois_server && !empty($referral_server_name)) {
      $whois_server = $whois_server[1];
      $whois_server = preg_replace('#r?whois://#', '', $whois_server);
      if (strpos($whois_server, ':') !== false) {
        list($this->whois_server, $this->whois_port) = 
          explode(':', $whois_server, 2);
      } else {
        $this->whois_server = $whois_server;
      }

      $blacklisted_referrals = DaGdConfig::get('whois.referral_blacklist');
      if (!in_array($this->whois_server, $blacklisted_referrals)) {
        return true;
      }
    }

    $this->skip_detail = true;
    return $whois_info;
  }

  /*
   * Now that we know which server to query, we can query them directly
   * and this should be the final step.
   *
   * @returns <string> the result from the real whois server.
   * @returns <bool> false if non successful.
   */
  private function fetchWhoisDetails() {
    $sock = fsockopen($this->whois_server, (int)$this->whois_port);
    if (!$sock) {
      return false;
    }
    fwrite($sock, $this->query.$this->domain."\r\n");
    $response = '';
    while (!feof($sock)) {
      $response .= fgets($sock);
    }
    fclose($sock);
    if (strlen($response)) {
      return $response;
    } else {
      return false;
    }
  }

  /*
   * Use the above functions to give actual whois info.
   *
   * @returns <string> whois info!
   */
  public function performQuery() {
    $whois_server = $this->fetchWhoisServer();
    if ($this->skip_detail) {
      return $whois_server;
    } else {
      return $this->fetchWhoisDetails();
    }
  }
}