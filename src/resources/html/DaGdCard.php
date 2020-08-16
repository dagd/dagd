<?php

class DaGdCard implements DaGdToTagInterface {
  private $title;
  private $body;
  private $footer;
  private $classes = array('card');

  public function setTitle($title) {
    $this->title = $title;
    return $this;
  }

  public function getTitle() {
    return $this->title;
  }

  public function setBody($body) {
    $this->body = $body;
    return $this;
  }

  public function getBody() {
    return $this->body;
  }

  public function setFooter($footer) {
    $this->footer = $footer;
    return $this;
  }

  public function getFooter() {
    return $this->footer;
  }

  public function setClasses($classes) {
    $this->classes = $classes;
    return $this;
  }

  public function getClasses() {
    return $this->classes;
  }

  public function addClass($class) {
    $this->classes[] = $class;
    return $this;
  }

  public function toTag() {
    $components = array();

    if ($title = $this->getTitle()) {
      $components[] = tag(
        'div',
        tag('strong', $title),
        array(
          'class' => 'card-title',
        )
      );
    }

    $components[] = tag(
      'div',
      $this->getBody(),
      array(
        'class' => 'card-body',
      )
    );

    if ($footer = $this->getFooter()) {
      $components[] = tag(
        'div',
        $footer,
        array(
          'class' => 'card-footer',
        )
      );
    }

    $card = tag(
      'div',
      $components,
      array(
        'class' => implode(' ', $this->getClasses()),
      )
    );

    return $card;
  }
}
