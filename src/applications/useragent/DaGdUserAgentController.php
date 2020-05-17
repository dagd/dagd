<?php
final class DaGdUserAgentController extends DaGdController {
  public static function getHelp() {
    return id(new DaGdHelp())
      ->setTitle('useragent')
      ->setDescription('Show the user agent that your client is sending.')
      ->addPath(
        id(new DaGdHelpPath())
          ->setPath('ua')
          ->setMethods(array('GET')));
  }

  public function execute(DaGdResponse $response) {
    return $this->getRequest()->getHeader('User-Agent');
  }
}
