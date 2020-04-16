<?php

class DaGdHeaderGetter {
  private $site = '';
  private $connect_timeout = 3;
  private $request_timeout = 3;
  private $follow_redirects = true;
  private $max_redirects = 5;
  private $curl;

  public function setSite($site) {
    $this->site = $site;
    return $this;
  }

  public function getSite() {
    return $this->site;
  }

  public function setConnectTimeout($connect_timeout) {
    $this->connect_timeout = $connect_timeout;
    return $this;
  }

  public function getConnectTimeout() {
    return $this->connect_timeout;
  }

  public function setRequestTimeout($request_timeout) {
    $this->request_timeout = $request_timeout;
    return $this;
  }

  public function getRequestTimeout() {
    return $this->request_timeout;
  }

  public function setFollowRedirects($follow_redirects) {
    $this->follow_redirects = $follow_redirects;
    return $this;
  }

  public function getFollowRedirects() {
    return $this->follow_redirects;
  }

  public function setMaxRedirects($max_redirects) {
    $this->max_redirects = $max_redirects;
    return $this;
  }

  public function getMaxRedirects() {
    return $this->max_redirects;
  }

  protected function createClient($site = null) {
    $this->curl = curl_init();
    if (empty($site)) {
      $site = $this->site;
    }
    curl_setopt($this->curl, CURLOPT_URL, $site);
    curl_setopt($this->curl, CURLOPT_NOBODY, true);
    curl_setopt($this->curl, CURLOPT_CONNECTTIMEOUT, $this->connect_timeout);
    curl_setopt($this->curl, CURLOPT_TIMEOUT, $this->request_timeout);
    curl_setopt($this->curl, CURLOPT_VERBOSE, true);
    curl_setopt($this->curl, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($this->curl, CURLOPT_HEADER, true);
    curl_setopt($this->curl, CURLOPT_FOLLOWLOCATION, false);
    return $this;
  }

  protected function cleanup() {
    if ($this->curl) {
      curl_close($this->curl);
    }
    $this->curl = null;
  }

  // We can't just use CURLOPT_FOLLOWLOCATION because if it lands on a site
  // that breaks (e.g. a redirect to a site that doesn't DNS resolve), it won't
  // return anything at all. We want to capture intermediate results.
  // Unfortunately this means we also need to parse headers and pull out
  // "Location".
  public function requestHeaders() {
    $results = array();
    $redirects = 1;
    $redirect_to = null;
    if ($this->follow_redirects) {
      $redirects = $this->max_redirects;
    }
    for ($i = 0; $i < $redirects; $i++) {
      $this->createClient($redirect_to);
      $out = curl_exec($this->curl);
      if (empty($out)) {
        // Nothing to parse, nothing to append, just stop here.
        break;
      }
      $results[] = $out;
      $code = curl_getinfo($this->curl, CURLINFO_HTTP_CODE);
      $this->cleanup();
      if (!in_array($code, range(300, 308))) {
        break;
      }
      $redirect_to = $this->parseHeaders($out, 'location');
      if (empty($redirect_to)) {
        break;
      }
    }
    return $results;
  }

  // This function is uncached because we are only requesting the value of one
  // header. If it's ever used anywhere else, it should be rewritten to cache
  // the results.
  protected function parseHeaders($data, $key) {
    $header_lines = explode("\n", $data);
    foreach ($header_lines as $header) {
      $header_split = explode(':', $header, 2);
      if (count($header_split) < 2) {
        continue;
      }
      $header_key = trim(strtolower($header_split[0]));
      if ($header_key === $key) {
        return trim($header_split[1]);
      }
    }
    return null;
  }

}
