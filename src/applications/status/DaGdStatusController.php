<?php
final class DaGdStatusController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'status',
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
  }

  public function execute(DaGdResponse $response) {
    $code = $this->getRequest()->getRouteComponent('code');
    if (!is_numeric($code)) {
      // TODO: Common codes/messages should be predefined somewhere.
      $response
        ->setCode(400)
        ->setMessage('Bad request');
      return 'You should give a numeric HTTP status code.';
    }
    if ((int)$code > 999) {
      $response
        ->setCode(400)
        ->setMessage('Bad request');
      return 'The given HTTP status code must be under 1000.';
    }

    $text = $this
      ->getRequest()
      ->getParamOrDefault('message');

    if ($text === null) {
      $text = $this
        ->getRequest()
        ->getRouteComponent('message', null, true);
    }

    $response->setCode((int)$code);
    $response->setMessage($text);

    if ((int)$code == 418) {
      $cs = new DaGdCowsay();
      $cs->setCow('dagd/teapot');
      $cs->setThoughts('/');
      $cs->setMessage("418 I'm a badly drawn teapot");
      return $cs->render();
    }
    return $response->getMessage();
  }
}
