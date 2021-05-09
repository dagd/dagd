<?php

final class DaGd403Controller extends DaGdErrorController {
  private function setup($response) {
    statsd_bump('status,code=403');
    $response->setCode(403);
  }

  public function render(DaGdHTMLResponse $response) {
    $this->setup($response);

    $responses = array(
      'You walk down the dark hall and come to a locked door.',
      'By no means can I permit you to do that.',
      'Unfortunately, I must forbid you from accessing this page.',
      'There is something really cool here, but you cannot see it.',
    );

    $default = $responses[array_rand($responses)];
    $message = $this->getMessage($default);

    $text = tag(
      'div',
      array(
        tag('h1', $message),
        tag('h2', '403.'),
        tag('h3', 'Forbidden.'),
        $this->getHtmlMessage(),
      ),
      array(
        'style' => 'text-align: center;',
      )
    );

    $bigcloud = tag(
      'div',
      '🔒',
      array(
        'style' => 'font-size: 6em; text-align: center; margin-top: 20px;',
      )
    );

    $template = $this
      ->getBaseTemplate()
      ->setBody(array($bigcloud, $text))
      ->setStyle($this->getStyle())
      ->setTitle('403')
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function execute(DaGdResponse $response) {
    $this->setup($response);
    return $this->getMessage('403 - forbidden');
  }
}
