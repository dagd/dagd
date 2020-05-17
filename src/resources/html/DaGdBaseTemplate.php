<?php

class DaGdBaseTemplate {
  private $title = '';
  private $style = array();
  private $body;
  private $escape = true;

  public function setTitle($title) {
    $this->title = $title;
    return $this;
  }

  public function getTitle() {
    return tag('title', 'da.gd: '.$this->title);
  }

  public function setStyle($style) {
    $this->style = $style;
    return $this;
  }

  public function getStyle() {
    return tag('style', $this->style, array(), true);
  }

  public function setBody($body) {
    $this->body = $body;
    return $this;
  }

  public function getBody() {
    return $this->body;
  }

  public function setEscape($escape) {
    $this->escape = $escape;
    return $this;
  }

  public function getEscape() {
    return $this->escape;
  }

  public function getHead() {
    return tag(
      'head',
      array(
        tag(
          'meta',
          null,
          array('charset' => 'utf-8')
        ),
        tag(
          'meta',
          null,
          array(
            'name' => 'keywords',
            'content' =>
            'dagd,da.gd,url,shorten,shortening,open,source,foss',
          )
        ),
        tag(
          'meta',
          null,
          array(
            'name' => 'description',
            'content' => 'The da.gd URL shortening service',
          )
        ),
        $this->getTitle(),
        $this->getStyle(),
      )
    );
  }

  public function getHtml($preamble = true) {
    return tag(
      'html',
      array(
        $this->getHead(),
        tag(
          'body',
          $this->getBody(),
          array(),
          !$this->getEscape() // Potentially dangerous
        ),
      )
    );
  }
}
