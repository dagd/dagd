<?php

/**
 * This controller gets instantiated and rendered when a URL is successfully
 * stored in the database.
 */
final class DaGdShortenStoredController extends DaGdController {
  private $short_url;

  public function setShortUrl(DaGdShortURL $short_url) {
    $this->short_url = $short_url;
    return $this;
  }

  public function getShortUrl() {
    return $this->short_url;
  }

  public function render(DaGdHTMLResponse $response) {
    $baseurl = DaGdConfig::get('general.baseurl');

    $link = tag(
      'a',
      $baseurl.'/'.$this->getShortUrl()->getShortUrl(),
      array(
        'href' => $this->getShortUrl()->getShortUrl(),
        'style' => 'color: #888; font-family: monospace;',
      )
    );

    $h1 = tag('h1', $link, array('style' => 'text-align: center;'));

    $template = $this
      ->getBaseTemplate()
      ->setBody($h1)
      ->setTitle('shorten')
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function execute(DaGdResponse $response) {
    $baseurl = DaGdConfig::get('general.baseurl');
    $url = $baseurl.'/'.$this->getShortUrl()->getShortUrl();
    return $url;
  }
}
