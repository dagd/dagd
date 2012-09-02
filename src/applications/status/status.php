<?php
final class DaGdStatusController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Generate a response with the given status code.',
    'path' => 'status',
    'examples' => array(
      array(
        'arguments' => array('404'),
        'summary' => 'Generate a 404 (not found) response.'),
      array(
        'arguments' => array('500', 'testing foo'),
        'summary' => 'Generate a 500 response with the message "testing foo"'),
    ));

  public function render() {
    $code = $this->route_matches[1];
    if (!is_numeric($code)) {
      error400('You should give a numeric HTTP status code.');
      return;
    }
    if ((int)$code > 999) {
      error400('The given HTTP status code must be under 1000.');
      return;
    }
    if (count($this->route_matches) == 2) {
      header('HTTP/1.1 '.$code.' da.gd header test');
      return;
    } else {
      $text = $this->route_matches[2];
      header('HTTP/1.1 '.$code.' '.$text);
      return;
    }
  }
}