<?php
final class DaGdUserAgentController extends DaGdController {
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

  public function execute(DaGdResponse $response) {
    return $this->getRequest()->getHeader('User-Agent');
  }
}
