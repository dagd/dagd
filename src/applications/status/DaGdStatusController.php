<?php
final class DaGdStatusController extends DaGdController {
  public function getHelp() {
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

  public function execute($response) {
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

    $response->setCode($code);
    if (count($this->getRequest()->getRouteMatches()) == 2) {
      $response->setMessage('da.gd header test');
      return '';
    } else {
      $text = $this->getRequest()->getRouteComponent('text');
      $response->setMessage($text);
      return '';
    }
  }
}
