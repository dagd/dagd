<?php

class DaGdWhois {
  private $query;
  private $whois_server;
  
  public function __construct($query) {
    $this->query = $query;
  }

  /*
   * Given a domain (passed to the constructor), we need to use the tld of it
   * to connect to <tld>.whois-servers.net to get the real whois server.
   * Using tihs, we end up making two whois queries, but it's the cleanest
   * way to do this.
   *
   * @returns <string> the tld of the domain passed to the constructor.
   */
  private function tld() {
    $tld = explode('.', $this->query);
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
    $sock = fsockopen($this->tld().'.whois-servers.net', 43);
    if (!$sock) {
      return false;
    }
    fwrite($sock, 'domain '.$this->query."\r\n");
    $whois_server = null;
    while ($line = fgets($sock)) {
      if (preg_match('#Whois Server: (.*)#i', $line, $whois_server)) {
        break;
      }
    }
    fclose($sock);
    if ($whois_server) {
      $this->whois_server = $whois_server[1];
      return true;
    } else {
      return false;
    }
  }

  /*
   * Now that we know which server to query, we can query them directly
   * and this should be the final step.
   *
   * @returns <string> the result from the real whois server.
   * @returns <bool> false if non successful.
   */
  private function fetchWhoisDetails() {
    $sock = fsockopen($this->whois_server, 43);
    if (!$sock) {
      return false;
    }
    fwrite($sock, $this->query."\r\n");
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
    $this->fetchWhoisServer();
    return $this->fetchWhoisDetails();
  }
}