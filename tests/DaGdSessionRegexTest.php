<?php

final class DaGdSessionRegexTest extends DaGdTest {
  private $regex;
  private $invert;

  public function __construct(
    $test_path,
    $key,
    $pattern,
    $invert = false) {

    $this->path = $test_path;
    $this->key = $key;
    $this->regex = $pattern;
    $this->invert = $invert;
  }

  public function run() {
    $content = $this->retrieve(true);
    $headers = $this->getHeaders();
    $session_data = '';

    foreach ($headers as $k => $v) {
      if ($k == 'Set-Cookie') {
        $parsed = array();
        parse_str(
          strtr(
            $v,
            array(
              '&' => '%26',
              '+' => '%2B',
              ';' => '&'
            )
          ),
          $parsed);
        foreach ($parsed as $ck => $cv) {
          if (strpos($ck, 'DaGdSession_') === 0) {
            $session_data .= $cv;
          }
        }
      }
    }

    $session = id(new DaGdSession())->loadFromCookies($session_data);
    $value = $session->get($this->key);

    if (!$value) {
      return $this->test(false, 'session key "'.$key.'" must exist');
    }

    if ($this->invert) {
      $match = !preg_match($this->regex, $value);
      return $this->test(
        $match,
        'session value for "'.$this->key.'" must NOT match: '.$this->regex,
        $value);
    } else {
      $match = preg_match($this->regex, $value);
      return $this->test(
        $match,
        'session value for "'.$this->key.'" must match: '.$this->regex,
        $value);
    }
  }
}
