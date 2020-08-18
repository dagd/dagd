<?php

/**
 * A custom "fake session" implementation.
 *
 * Session information is encrypted and stored in cookies.
 *
 * We need to ensure we never go over the 4k cookie size limit in modern
 * browsers.
 */
final class DaGdSession {
  private $data = array();

  public function loadSession(DaGdRequest $request) {
    $cookies = $request->getCookies();
    $encrypted_data = '';

    // We need to make sure the components get concatenated in order
    $tmp = array();
    foreach ($cookies as $k => $v) {
      if (strpos($k, 'DaGdSession_') === 0) {
        $tmp[$k] = $v;
      }
    }

    // If we have no session data to load, return the empty session
    if (count($tmp) == 0) {
      return $this;
    }

    ksort($tmp);

    foreach ($tmp as $k => $v) {
      $encrypted_data .= $v;
    }

    return $this->loadFromCookies($encrypted_data);
  }

  // This is abstracted out mainly for tests to call into, without having an
  // actual DaGdRequest at their disposal. In normal cases, first-party code
  // should go through loadSession instead.
  public function loadFromCookies($encrypted_data) {
    $iv_and_data = explode('.', $encrypted_data);
    if (count($iv_and_data) != 2) {
      throw new Exception('Could not parse IV and encrypted session data');
    }

    $iv = hex2bin($iv_and_data[0]);
    $data = $iv_and_data[1];
    $unser = unserialize($this->decryptData($data, $iv));
    if ($unser === false) {
      return $this->destroy();
    }
    $this->data = $unser;
    return $this;
  }

  public function destroy() {
    $this->data = array();
    return $this;
  }

  private function decryptData($str, $iv) {
    $method = DaGdConfig::get('session.encryption_method');
    $key = DaGdConfig::get('session.encryption_key');
    if (!$key) {
      throw new Exception('You must set session.encryption_key to use sessions');
    }

    return openssl_decrypt($str, $method, $key, 0, $iv);
  }

  private function encryptData($str) {
    $method = DaGdConfig::get('session.encryption_method');
    $iv_length = openssl_cipher_iv_length($method);
    $secure_iv = false;
    $iv = openssl_random_pseudo_bytes($iv_length, $secure_iv);
    if (!$secure_iv) {
      throw new Exception('Unable to obtain a secure IV');
    }

    $key = DaGdConfig::get('session.encryption_key');
    if (!$key) {
      throw new Exception('You must set session.encryption_key to use sessions');
    }
    return array(
      'iv' => bin2hex($iv),
      'data' => openssl_encrypt($str, $method, $key, 0, $iv),
    );
  }

  public function emit() {
    $serialized = serialize($this->data);
    $data = $this->encryptData($serialized);
    // We use 3500 because the max is 4000 and we want some leeway
    // We still hit the server header limit on requests eventually, though.
    $data_chunks = str_split($data['iv'].'.'.$data['data'], 3500);
    $cookies = array();
    $i = 0;
    foreach ($data_chunks as $chunk) {
      $cookies['DaGdSession_'.$i] = $chunk;
      $i++;
    }
    return $cookies;
  }

  public function get($key, $default = null) {
    return idx($this->data, $key, $default);
  }

  public function set($key, $value) {
    $this->data[$key] = $value;
    return $this;
  }

  public function remove($key) {
    unset($this->data[$key]);
    return $this;
  }
}
