<?php

final class DaGd401Controller extends DaGdErrorController {
  private function setup($response) {
    $response->setCode(401);
  }

  public function render(DaGdHTMLResponse $response) {
    $this->setup($response);

    $text = tag(
      'div',
      array(
        tag('h1', $this->getMessage('You shall not pass.')),
        tag('h2', '401.'),
        tag('h3', 'Unauthorized.'),
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
      ->setTitle('401')
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function execute(DaGdResponse $response) {
    $response->setCode(401);
    return $this->getMessage('401 - unauthorized');
  }
}
