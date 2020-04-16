<?php
final class DaGdUserAgentController extends DaGdBaseClass {
  public function getHelp() {
    return array(
      'title' => 'useragent',
      'summary' => 'Show the user agent that your browser is sending.',
      'path' => 'ua',
      'examples' => array(
        array(
          'arguments' => null,
          'summary' => null),
      ));
  }

  protected $wrap_html = true;

  public function render() {
    return $_SERVER['HTTP_USER_AGENT'];
  }
}
