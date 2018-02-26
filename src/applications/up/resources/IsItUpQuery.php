<?php

require_once dirname(__FILE__).'/../../../resources/global_resources.php';

class IsItUpQuery {
  private $site;
  private $response;

  public function __construct($site) {
    $this->site = $site;
  }

  public function query(
    $verbose = false,
    $sslverify = false,
    $redirects = true) {
    $query = $this->site;
    if ($query == 'localhost' or $query == '127.0.0.1') {
      return 'LOL';
    } else {
      $curl = curl_init();

      // Configurable options (via config file)
      $agent = DaGdConfig::get('general.useragent');
      curl_setopt($curl, CURLOPT_URL, $query);

      $timeout = DaGdConfig::get('isitup.timeout');
      curl_setopt($curl, CURLOPT_TIMEOUT, $timeout);

      $max_redirects = DaGdConfig::get('isitup.max_redirects');
      if (!$redirects) {
        $max_redirects = 0;
      }
      curl_setopt($curl, CURLOPT_MAXREDIRS, $max_redirects);

      curl_setopt($curl, CURLOPT_USERAGENT, $agent);
      curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
      curl_setopt($curl, CURLOPT_HEADER, true);
      curl_setopt($curl, CURLOPT_NOBODY, true);
      curl_setopt($curl, CURLOPT_SSL_VERIFYPEER, $sslverify);
      curl_setopt($curl, CURLOPT_FOLLOWLOCATION, $redirects);
      $this->response = curl_exec($curl);
      $http_response = curl_getinfo($curl, CURLINFO_HTTP_CODE);
      curl_close($curl);

      if ($verbose) {
        if ($http_response == 0) {
          return 'The server appears to be offline.';
        } else {
          return 'The server responded with HTTP status code '.
            $http_response.'.';
        }
      }

      return $http_response;
    }
  }
}