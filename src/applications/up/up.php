<?php
final class DaGdIsItUpController extends DaGdBaseClass {
  public $__help__ = array(
    'title' => 'up',
    'summary' => 'Determine whether or not a site is up.',
    'path' => 'up',
    'examples' => array(
      array(
        'arguments' => array('google.com'),
        'summary' => 'Get the HTTP response code for the given site'),
    ));

  protected $wrap_html = true;

  public function render() {
    $query = $this->route_matches[1];
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
      curl_setopt($curl, CURLOPT_MAXREDIRS, $max_redirects);

      curl_setopt($curl, CURLOPT_USERAGENT, $agent);
      curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
      curl_setopt($curl, CURLOPT_HEADER, true);
      curl_setopt($curl, CURLOPT_NOBODY, true);
      curl_setopt($curl, CURLOPT_SSL_VERIFYPEER, false);
      curl_setopt($curl, CURLOPT_SSL_VERIFYHOST, false);
      curl_setopt($curl, CURLOPT_SSLVERSION, 3);
      curl_setopt($curl, CURLOPT_FOLLOWLOCATION, true);
      curl_exec($curl);
      $http_response = curl_getinfo($curl, CURLINFO_HTTP_CODE);
      return $http_response;
    }
  }
}