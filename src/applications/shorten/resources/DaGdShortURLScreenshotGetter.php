<?php

/**
 * Use Google's "PageSpeed Insights" API to fetch a screenshot of a short URL's
 * long URL.
 *
 * We could also abstract this out and potentially set up some kind of AWS
 * Lambda endpoint or similar to do screenshots.
 */
final class DaGdShortURLScreenshotGetter implements DaGdCacheMissCallback {
  private $long_url;

  public function __construct($long_url) {
    $this->long_url = $long_url;
  }

  /**
   * Returns a base64 string directly from Google's API.
   */
  public function getScreenshot() {
    $long_url = urlencode($this->long_url);

    $agent = DaGdConfig::get('general.useragent');
    $timeout = DaGdConfig::get('shorten.safe_browsing_timeout');
    $key = DaGdConfig::get('shorten.google_pagespeed_insights_key');

    if (empty($key)) {
      throw new Exception(
        'required config option shorten.google_pagespeed_insights_key is '.
        'empty');
    }

    $url = 'https://www.googleapis.com/pagespeedonline/v5/runPagespeed?';
    $url .= 'locale=en_US&key='.$key.'&url='.$long_url;

    $curl = curl_init();
    curl_setopt($curl, CURLOPT_URL, $url);
    curl_setopt($curl, CURLOPT_TIMEOUT, 30);
    curl_setopt($curl, CURLOPT_USERAGENT, $agent);
    curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($curl, CURLOPT_HEADER, false);
    curl_setopt($curl, CURLOPT_NOBODY, false);
    curl_setopt($curl, CURLOPT_SSL_VERIFYPEER, true);
    $response = curl_exec($curl);
    $http_response = curl_getinfo($curl, CURLINFO_HTTP_CODE);
    curl_close($curl);

    if (strlen($response) == 0 || $http_response != 200) {
      return null;
    }

    $json_response = json_decode($response, true);
    $audits = $json_response['lighthouseResult']['audits'];
    $ss = $audits['final-screenshot']['details']['data'];
    return $ss;
  }

  public function run($key) {
    $ss = $this->getScreenshot();
    return explode(',', $ss, 2)[1];
  }
}
