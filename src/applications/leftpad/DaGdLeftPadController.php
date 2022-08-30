<?php
final class DaGdLeftPadController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'leftpad',
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
  }

  public function execute(DaGdResponse $response) {
    $length = (int)$this->getRequest()->getRouteComponent(1);
    $padchar = $this->getRequest()->getRouteComponent(2);
    $input = $this->getRequest()->getRouteComponent(3);
    return str_pad($input, $length, $padchar, STR_PAD_LEFT);
  }
}
