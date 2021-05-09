<?php

final class DaGd404Controller extends DaGdErrorController {
  private function setup($response) {
    statsd_bump('status,code=404');
    $response->setCode(404);
  }

  public function render(DaGdHTMLResponse $response) {
    $this->setup($response);

    $responses = array(
      'The clouds have failed you.',
      'When it rains, it 404s.',
      'This page was not found in the clouds.',
      'The clouds got a little too dark to locate that page.',
    );

    $default = $responses[array_rand($responses)];
    $message = $this->getMessage($default);

    $text = tag(
      'div',
      array(
        tag('h1', $message),
        tag('h2', '404.'),
        tag('h3', 'Page not found.'),
        $this->getHtmlMessage(),
      ),
      array(
        'style' => 'text-align: center;',
      )
    );

    $bigcloud = tag(
      'div',
      '🌧',
      array(
        'style' => 'font-size: 6em; text-align: center; margin-top: 20px;',
      )
    );

    $template = $this
      ->getBaseTemplate()
      ->setBody(array($bigcloud, $text))
      ->setStyle($this->getStyle())
      ->setTitle('404')
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function execute(DaGdResponse $response) {
    $this->setup($response);
    return $this->getMessage('404 - route not found');
  }
}
