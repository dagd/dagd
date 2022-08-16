<?php

const COWSAY_SAY = 1;
const COWSAY_THINK = 2;

class DaGdCowsay {
  protected $cow = '';
  protected $eyes = 'oo';
  protected $thoughts = '\\';
  protected $tongue = '  ';
  protected $message = '';
  protected $action = COWSAY_SAY;
  protected $width = 80;

  public function __construct() {
    $this->setCow('default');
  }

  public function setCow($cow) {
    $cow = str_replace('.cow', '', $cow);
    // This logic looks weird, but it's safer to hardcode this than to risk
    // getting the regex wrong (now or later) and end up open to directory
    // traversal attacks.
    $custom_cow = false;
    if (strpos($cow, 'dagd/') === 0) {
      $custom_cow = true;
      $cow = str_replace('dagd/', '', $cow);
    }
    if (!preg_match('/^[0-9a-z\-]+$/i', $cow)) {
      throw new Exception('Cow name must only include 0-9, a-z, A-Z, -');
    }
    if ($custom_cow) {
      $cow = file_get_contents(dirname(__FILE__).'/dagd/'.$cow.'.cow');
    } else {
      $cow = file_get_contents(dirname(__FILE__).'/'.$cow.'.cow');
    }
    $this->cow = $cow;
    return $this;
  }

  public function setEyes($eyes) {
    $this->eyes = $eyes;
    return $this;
  }

  public function setThoughts($thoughts) {
    $this->thoughts = $thoughts;
    return $this;
  }

  public function setTongue($tongue) {
    $this->tongue = $tongue;
    return $this;
  }

  public function setMessage($message) {
    $this->message = $message;
    return $this;
  }

  public function setAction($action) {
    $this->action = $action;
    return $this;
  }

  public function setWidth($width) {
    $this->width = $width;
    return $this;
  }

  public function getCow() {
    return $this->cow;
  }

  public function getEyes() {
    return $this->eyes;
  }

  public function getThoughts() {
    return $this->thoughts;
  }

  public function getTongue() {
    return $this->tongue;
  }

  public function getMessage() {
    return $this->message;
  }

  public function getAction() {
    return $this->action;
  }

  public function getWidth() {
    return $this->width;
  }

  protected function getBorders() {
    $out = array();
    switch ($this->getAction()) {
    case COWSAY_SAY:
      $out['single'] = '<>';
      $out['first'] = '/\\';
      $out['intermediate'] = '||';
      $out['last'] = '\\/';
      break;
    case COWSAY_THINK:
      $out['single'] = '()';
      $out['first'] = '()';
      $out['intermediate'] = '()';
      $out['last'] = '()';
      break;
    default:
      throw new Exception('Invalid cowsay action');
    }
    return $out;
  }

  protected function balloon() {
    $message = str_replace("\r\n", "\n", $this->getMessage());
    $message = str_replace("\t", ' ', $message);
    $wrapped = wordwrap(
      $message,
      $this->getWidth() - 4,
      "\n",
      true);
    $lines = explode("\n", $wrapped);
    $last_idx = count($lines) - 1;
    $longest = max(array_map('strlen', $lines));
    $borders = $this->getBorders();
    $out = array();

    $out[] = array(' '.str_repeat('_', $longest + 2));
    // Are we multiline or not?
    if ($last_idx == 0) {
      $out[] = array(
        $borders['single'][0],
        str_pad($lines[0], $longest),
        $borders['single'][1]
      );
    } else {
      foreach ($lines as $idx => $line) {
        $key = '';
        switch ($idx) {
        case 0:
          $key = 'first';
          break;
        case $last_idx:
          $key = 'last';
          break;
        default:
          $key = 'intermediate';
        }
        $out[] = array(
          $borders[$key][0],
          str_pad($line, $longest),
          $borders[$key][1]
        );
      }
    }
    $out[] = array(' '.str_repeat('-', $longest + 2));

    $ret = '';
    foreach ($out as $line) {
      $ret .= implode(' ', $line)."\n";
    }
    return $ret;
  }

  public function render() {
    $out = array();
    $lines = explode("\n", $this->getCow());
    foreach ($lines as $line) {
      if (strpos($line, 'EOC') !== false || strpos($line, '#') === 0) {
        continue;
      }

      $line = preg_replace('/\\\\(.)/', '$1', $line);
      $line = preg_replace('/\$eyes/', $this->getEyes(), $line);
      $line = preg_replace('/\$thoughts/', $this->getThoughts(), $line);
      $line = preg_replace('/\$tongue/', $this->getTongue(), $line);

      $out[] = $line;
    }

    $ret = $this->balloon();
    $ret .= rtrim(implode("\n", $out));
    return $ret;
  }
}
