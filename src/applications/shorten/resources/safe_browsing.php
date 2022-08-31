<?php

function query_safe_browsing($urls) {
  $client_id = DaGdConfig::get('shorten.safe_browsing_client_id');
  $client_version = DaGdConfig::get('shorten.safe_browsing_client_version');
  $threat_types = DaGdConfig::get('shorten.safe_browsing_threat_types');
  $platform_types = DaGdConfig::get('shorten.safe_browsing_platform_types');
  $api_key = DaGdConfig::get('shorten.safe_browsing_api_key');
  $agent = DaGdConfig::get('general.useragent');
  $timeout = DaGdConfig::get('shorten.safe_browsing_timeout');
  $default = DaGdConfig::get('shorten.safe_browsing_default_accept');

  $payload = array();
  $payload['client'] = array();
  $payload['client']['clientId'] = $client_id;
  $payload['client']['clientVersion'] = $client_version;

  $payload['threatInfo'] = array();
  $payload['threatInfo']['threatTypes'] = $threat_types;
  $payload['threatInfo']['platformTypes'] = $platform_types;
  $payload['threatInfo']['threatEntryTypes'] = array('URL');
  $payload['threatInfo']['threatEntries'] = array();

  if (is_array($urls)) {
    foreach ($urls as $url) {
      $payload['threatInfo']['threatEntries'][] = array(
        'url' => $url,
      );
    }
  } else {
    $payload['threatInfo']['threatEntries'][] = array(
      'url' => $urls,
    );
  }

  $url = 'https://safebrowsing.googleapis.com/v4/threatMatches:find?key='.
    $api_key;
  $payload_str = json_encode($payload);

  $curl = curl_init();
  curl_setopt($curl, CURLOPT_URL, $url);
  curl_setopt($curl, CURLOPT_TIMEOUT, $timeout);
  curl_setopt($curl, CURLOPT_USERAGENT, $agent);
  curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl, CURLOPT_HEADER, false);
  curl_setopt($curl, CURLOPT_NOBODY, false);
  curl_setopt($curl, CURLOPT_SSL_VERIFYPEER, true);
  curl_setopt($curl, CURLOPT_CUSTOMREQUEST, 'POST');
  curl_setopt($curl, CURLOPT_POSTFIELDS, $payload_str);
  curl_setopt(
    $curl,
    CURLOPT_HTTPHEADER,
    array(
      'Content-Type: application/json',
      'Content-Length: '.strlen($payload_str),
    ));

  $response = curl_exec($curl);
  $http_response = curl_getinfo($curl, CURLINFO_HTTP_CODE);
  curl_close($curl);

  if (strlen($response) == 0 || $http_response != 200) {
    return $default;
  }

  $json_response = json_decode($response, true);

  // Per Google:
  // Note: If there are no matches (that is, if none of the URLs specified in
  // the request are found on any of the lists specified in a request), the
  // HTTP POST response simply returns an empty object in the response body.
  return empty($json_response);
}
