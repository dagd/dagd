<?php

final class DaGd400Controller extends DaGdErrorController {
  private function setup($response) {
    $response->setCode(400);
  }

  public function render(DaGdHTMLResponse $response) {
    $this->setup($response);

    $text = tag(
      'div',
      array(
        tag('h1', $this->getMessage('Oh deer.')),
        tag('h2', '400.'),
        tag('h3', 'Bad request.'),
        $this->getHtmlMessage(),
      ),
      array(
        'style' => 'text-align: center;',
      )
    );

    $bigcloud = tag(
      'div',
      '🦌',
      array(
        'style' => 'font-size: 6em; text-align: center; margin-top: 20px;',
      )
    );

    $template = $this
      ->getBaseTemplate()
      ->setBody(array($bigcloud, $text))
      ->setStyle($this->getStyle())
      ->setTitle('400')
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function execute(DaGdResponse $response) {
    $response->setCode(400);
    return $this->getMessage('400 - bad request');
  }
}
