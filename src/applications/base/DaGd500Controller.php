<?php

final class DaGd500Controller extends DaGdController {
  private function setup($response) {
    $response->setCode(500);
  }

  public function render(DaGdHTMLResponse $response) {
    $this->setup($response);

    $responses = array(
      'This action has successfully failed.',
      'Well, this is just not good.',
      'Something has happened. Something bad. Really bad.',
      'This page died. Just like my hopes and dreams.',
      'Sorry, I\'m not feeling well right now.',
    );

    $text = tag(
      'div',
      array(
        tag('h1', $responses[array_rand($responses)]),
        tag('h2', '500.'),
        tag('h3', 'Internal server error.'),
      ),
      array(
        'style' => 'text-align: center;',
      )
    );

    $bigcloud = tag(
      'div',
      '🤒',
      array(
        'style' => 'font-size: 6em; text-align: center; margin-top: 20px;',
      )
    );

    $template = $this
      ->getBaseTemplate()
      ->setBody(array($bigcloud, $text))
      ->setStyle($this->getStyle())
      ->setTitle('500')
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function execute(DaGdResponse $response) {
    $response->setCode(500);
    return '500 - internal server error';
  }
}
