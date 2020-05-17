<?php

final class DaGdRequest {
  private $cookies = array();
  private $request = array();
  private $server = array();
  private $route_matches = array();

  public function setCookies($cookies) {
    $this->cookies = $cookies;
    return $this;
  }

  public function getCookies() {
    return $this->cookies;
  }

  public function setRequest($request) {
    $this->request = $request;
    return $this;
  }

  public function getRequest() {
    return $this->request;
  }

  public function setServer($server) {
    $this->server = $server;
    return $this;
  }

  public function getServer() {
    return $this->server;
  }

  public function setRouteMatches($route_matches) {
    $this->route_matches = $route_matches;
    return $this;
  }

  public function getRouteMatches() {
    return $this->route_matches;
  }

  public function getParamOrDefault(
    $key,
    $default = null,
    $allow_empty = false,
    $empty_default = null) {

    $raw_request = $this->getRequest();
    if ($allow_empty &&
        array_key_exists($key, $raw_request) &&
        strlen($raw_request[$key]) == 0) {
      return $empty_default;
    }
    return idx($raw_request, $key, $default);
  }

  public function wantsCow() {
    return $this->getParamOrDefault('cow', false, true, true);
  }

  public function wantsText() {
    return $this->getParamOrDefault('text', false, true, true);
  }

  public function wantsJson() {
    return $this->getParamOrDefault('json', false, true, true);
  }

  public function getCookie($cookie, $default = null) {
    return idx($this->cookies, $cookie, $default);
  }

  public function getClientIP() {
    if (idx($this->server, 'HTTP_X_DAGD_PROXY') &&
        $ip = idx($this->server, 'HTTP_X_FORWARDED_FOR')) {
      return $ip;
    }
    return idx($this->server, 'REMOTE_ADDR');
  }
}
