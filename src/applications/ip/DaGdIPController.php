<?php

final class DaGdIPController extends DaGdController {
  public function getHelp() {
    return array(
      'title' => 'ip',
      'summary' => 'Returns your current IP address.',
      'path' => 'ip',
      'examples' => array(
        array(
          'arguments' => null,
          'summary' => null),
      ));
  }

  public function execute(DaGdResponse $response) {
    return $this->getRequest()->getClientIP();
  }
}
