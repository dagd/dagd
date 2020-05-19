<?php

abstract class DaGdTemplate {
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
    return $this->title;
  }

  public function getTitleTag() {
    return tag('title', $this->getTitle());
  }

  public function setStyle($style) {
    $this->style = $style;
    return $this;
  }

  public function getStyle() {
    return $this->style;
  }

  public function getStyleTag($minify = true) {
    $css = $this->getStyle();
    // Some silly minification
    if ($minify) {
      $css = preg_replace('/^\s*/m', '', $css);
      $css = preg_replace("/\s*[\n\r]\s*/", '', $css);
      $css = preg_replace('/;\s*/', ';', $css);
      $css = preg_replace('/:\s*/', ':', $css);
      $css = preg_replace('/\s*\{\s*/', '{', $css);
    }
    return tag('style', $css, array(), true);
  }

  public function setBody($body) {
    $this->body = $body;
    return $this;
  }

  public function getBody() {
    return $this->body;
  }

  public function getBodyTag() {
    return tag(
      'body',
      $this->getBody(),
      array(
        'class' => $this->getDarkmode() ? 'darkmode' : 'lightmode',
      ),
      !$this->getEscape() // Potentially dangerous
    );
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

  public function getHeadTag() {
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
        $this->getTitleTag(),
        $this->getStyleTag(),
      )
    );
  }

  public function getHtmlTag($preamble = true) {
    return tag(
      'html',
      array(
        $this->getHeadTag(),
        $this->getBodyTag(),
      )
    );
  }
}
