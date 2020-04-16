<?php

class DaGdHeaderGetter {
  private $site = '';
  private $connect_timeout = 3;
  private $request_timeout = 3;
  private $follow_redirects = true;
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

  protected function createClient() {
    $this->curl = curl_init();
    curl_setopt($this->curl, CURLOPT_URL, $this->site);
    curl_setopt($this->curl, CURLOPT_NOBODY, true);
    curl_setopt($this->curl, CURLOPT_CONNECTTIMEOUT, $this->connect_timeout);
    curl_setopt($this->curl, CURLOPT_TIMEOUT, $this->request_timeout);
    curl_setopt($this->curl, CURLOPT_VERBOSE, true);
    curl_setopt($this->curl, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($this->curl, CURLOPT_HEADER, true);
    curl_setopt($this->curl, CURLOPT_FOLLOWLOCATION, $this->follow_redirects);
    return $this;
  }

  protected function cleanup() {
    if ($this->curl) {
      curl_close($this->curl);
    }
    $this->curl = null;
  }

  public function requestHeaders() {
    $this->createClient();
    $out = curl_exec($this->curl);
    $this->cleanup();
    return $out;
  }
}
