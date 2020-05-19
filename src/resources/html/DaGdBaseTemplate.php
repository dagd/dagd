<?php

class DaGdBaseTemplate {
  private $title = '';
  private $style = array();
  private $body;
  private $escape = true;
  private $darkmode = false;

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
    return '';
  }

  public function getStyleTag() {
    return tag('style', $this->style, array(), true);
  }

  public function setBody($body) {
    $this->body = $body;
    return $this;
  }

  public function getBody() {
    return $this->body;
  }

  // Only provided for legacy controllers.
  // New controllers should never call this.
  public function setEscape($escape) {
    $this->escape = $escape;
    return $this;
  }

  public function getEscape() {
    return $this->escape;
  }

  public function setDarkmode($darkmode) {
    $this->darkmode = $darkmode;
    return $this;
  }

  public function getDarkmode() {
    return $this->darkmode;
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
        $this->getStyleTag(),
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
          array(
            'class' => $this->getDarkmode() ? 'darkmode' : 'lightmode',
          ),
          !$this->getEscape() // Potentially dangerous
        ),
      )
    );
  }
}
