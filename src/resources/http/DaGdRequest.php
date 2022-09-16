<?php

class DaGdRequest {
  private $cookies = array();
  private $request = array();
  private $server = array();
  private $method;
  private $route_matches = array();
  private $session;

  public function setCookies($cookies) {
    $this->cookies = $cookies;
    $this->session = id(new DaGdSession())->loadSession($this);
    return $this;
  }

  public function getCookies() {
    return $this->cookies;
  }

  public function getCookie($cookie, $default = null) {
    return idx($this->cookies, $cookie, $default);
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
    $this->method = $_SERVER['REQUEST_METHOD'];
    return $this;
  }

  public function getServer() {
    return $this->server;
  }

  public function getMethod() {
    return $this->method;
  }

  public function setRouteMatches($route_matches) {
    $this->route_matches = $route_matches;
    return $this;
  }

  public function getRouteMatches() {
    return $this->route_matches;
  }

  public function getRouteComponent($idx, $default = null, $urldecode = false) {
    $val = idx($this->route_matches, $idx, $default);
    if ($urldecode) {
      return urldecode($val);
    }
    return $val;
  }

  public function getSession() {
    if (!$this->session) {
      throw new Exception(
        'setCookies() must be called before a session can be established');
    }
    return $this->session;
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

  /**
   * A shorter alias for getParamOrDefault().
   */
  public function param(
    $key,
    $default = null,
    $allow_empty = false,
    $empty_default = null) {

    return $this->getParamOrDefault(
      $key,
      $default,
      $allow_empty,
      $empty_default);
  }

  public function getHeaders() {
    $headers = array();
    foreach ($this->server as $k => $v) {
      if (strpos($k, 'HTTP_') === 0) {
        $k = substr($k, strlen('HTTP_'));
        $headers[$k] = $v;
      }
    }
    return $headers;
  }

  public function getHeader($header) {
    $header = strtoupper($header);
    $header = str_replace('-', '_', $header);
    $header = 'HTTP_'.$header;
    return idx($this->server, $header);
  }

  public function wantsCow() {
    return $this->getParamOrDefault('cow', false, true, true);
  }

  public function wantsText() {
    $text = $this->getParamOrDefault('text', null, true, true);
    if ($text !== null) {
      return $text;
    }

    if ($accept = $this->getHeader('Accept')) {
      $accept = strtolower(str_replace(' ', '', $accept));
      $html_accept_regex = implode('|', DaGdConfig::get('general.html_accept'));
      return !preg_match('#(?:'.$html_accept_regex.')#i', $accept);
    }

    // If all else fails, cater to simple clients and assume text.
    return true;
  }

  public function wantsJson() {
    return $this->getParamOrDefault('json', false, true, true);
  }

  public function getClientIP() {
    if ($this->getHeader('x-dagd-proxy') &&
        $ip = $this->getHeader('X-Forwarded-For')) {
      return $ip;
    }
    return idx($this->server, 'REMOTE_ADDR');
  }

  public function getFullRequestUri($include_querystring = true) {
    $protocol = idx($this->getServer(), 'HTTPS') ? 'https' : 'http';
    $host_and_port = idx($this->getServer(), 'HTTP_HOST');
    $path = idx($this->getServer(), 'REQUEST_URI');

    if (!$include_querystring) {
      $path = strtok($path, '?');
    }

    return $protocol.'://'.$host_and_port.$path;
  }
}
