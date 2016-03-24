<?php
final class DaGdLeftPadController extends DagdBaseClass {
  public static $__help__ = array(
    'summary' => 'Left pad a string. As a service.',
    'path' => 'leftpad',
    'examples' => array(
      array(
        'arguments' => array('[length]', '[padding character]', '[input]'),
        'summary' => 'Left-pads the input string with padding character',
      ),
      array(
        'arguments' => array('10', '@', 'hello'),
        'summary' => 'Returns "@@@@@hello"')
    ));

  public function render() {
    $length = (int)$this->route_matches[1];
    $padchar = $this->route_matches[2];
    $input = $this->route_matches[3];
    return str_pad($input, $length, $padchar, STR_PAD_LEFT);
  }
}