<?php

final class DaGdIPController extends DaGdController {
  public static function getHelp() {
    return id(new DaGdHelp())
      ->setTitle('ip')
      ->setDescription('Returns your current IP address.')
      ->addPath(
        id(new DaGdHelpPath())
          ->setPath('ip')
          ->setMethods(array('GET')));
  }

  public function execute(DaGdResponse $response) {
    return $this->getRequest()->getClientIP();
  }
}
