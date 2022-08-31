<?php

abstract class DaGdTemplate {
  private $title = '';
  private $style = array();
  private $stylesheets = array();
  private $javascripts = array();
  private $debug_body;
  private $body;
  private $escape = true;
  private $darkmode = false;
  private $alerts = array();

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

  public function setStyle(array $style) {
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

  public function setStylesheets($stylesheets) {
    $this->stylesheets = $stylesheets;
    return $this;
  }

  public function getStylesheets() {
    return $this->stylesheets;
  }

  public function addStylesheet($stylesheet) {
    if (!in_array($stylesheet, $this->getStylesheets())) {
      $this->stylesheets[] = $stylesheet;
    }
    return $this;
  }

  public function setJavascripts($javascripts) {
    $this->javascripts = $javascripts;
    return $this;
  }

  public function getJavascripts() {
    return $this->javascripts;
  }

  public function addJavascript($javascript) {
    if (!in_array($javascript, $this->getJavascripts())) {
      $this->javascripts[] = $javascript;
    }
    return $this;
  }

  private function stylesheetsToTags() {
    $out = array();
    foreach ($this->getStylesheets() as $stylesheet) {
      $out[] = tag(
        'link',
        null,
        array(
          'href' => DaGdStaticController::url($stylesheet),
          'rel' => 'stylesheet',
          'type' => 'text/css',
        )
      );
    }
    return $out;
  }

  private function javascriptsToTags() {
    $out = array();
    foreach ($this->getJavascripts() as $javascript) {
      $out[] = tag(
        'script',
        '',
        array(
          'src' => DaGdStaticController::url($javascript),
          'type' => 'text/javascript',
        )
      );
    }
    return $out;
  }

  public function setDebugBody($debug_body) {
    $this->debug_body = $debug_body;
    return $this;
  }

  public function getDebugBody() {
    return $this->debug_body;
  }

  public function setBody($body) {
    $this->body = $body;
    return $this;
  }

  public function getBody() {
    return $this->body;
  }

  public function getBodyTag() {
    $debug = DaGdConfig::get('general.debug');
    return tag(
      'body',
      array(
        $debug ? $this->getDebugBody() : null,
        $this->getBody(),
        $this->javascriptsToTags(),
      ),
      array(
        'class' => $this->getDarkmode() ? 'darkmode' : 'lightmode',
      ),
      // Potentially dangerous
      !$this->getEscape());
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

  public function setAlerts($alerts) {
    $this->alerts = $alerts;
    return $this;
  }

  public function addAlert($alert) {
    $this->alerts[] = $alert;
    return $this;
  }

  public function getAlerts() {
    return $this->alerts;
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
        $this->stylesheetsToTags(),
        $this->getStyleTag(),
      )
    );
  }

  public function getHtmlTag($preamble = true, $lang = null) {
    if ($lang === null) {
      $lang = DaGdConfig::get('general.default_language_code');
    }

    return tag(
      'html',
      array(
        $this->getHeadTag(),
        $this->getBodyTag(),
      ),
      array(
        'lang' => $lang,
      )
    );
  }
}
