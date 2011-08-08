<?php
require_once dirname(__FILE__).'/resources/dagd_whois.php';

class DaGdWhoisController extends DaGdBaseClass {
    
  public function render() {
    $query = $this->route_matches[1];
    $whois_client = new DaGdWhois($query);
    return $whois_client->performQuery();
  }
}