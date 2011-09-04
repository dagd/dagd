<?php
class DaGdUserAgentController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Show the user agent that your browser is sending.',
    'path' => 'ua',
    'examples' => array(
      array(
        'arguments' => null,
        'summary' => null),
    ));
  
  public function render() {
    return $_SERVER['HTTP_USER_AGENT'];
  }
}
