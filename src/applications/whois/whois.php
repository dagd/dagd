<?php
require_once dirname(__FILE__).'/resources/dagd_whois.php';

class DaGdWhoisController extends DaGdBaseClass {
  private $query;
  
  public function __construct($path, $query) {
    $this->query = $query;
  }
    
  public function render() {
    $whois_client = new DaGdWhois($this->query);
    return $whois_client->performQuery();
  }
}