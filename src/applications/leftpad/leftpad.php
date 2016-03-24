<?php
final class DaGdLeftPadController extends DagdBaseClass {
  public static $__help__ = array(
    'summary' => 'Left pad a string. As a service.',
    'path' => 'leftpad',
    'examples' => array(
      array(
        'arguments' => array('10', '*', 'hi'),
        'summary' =>
          'Pads the input string (last parameter) with the padding character '.
          '(second parameter) up to the specified length (first parameter)'),
    ));

  public function render() {
    $length = (int)$this->route_matches[1];
    $padchar = $this->route_matches[2];
    $input = $this->route_matches[3];
    return str_pad($input, $length, $padchar, STR_PAD_LEFT);
  }
}