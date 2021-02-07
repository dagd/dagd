<?php

/**
 * NOTE: Instances of this class get serialized and stored in the session.
 */
class DaGdAlert implements DaGdToTagInterface {
  const SUCCESS = 0;
  const INFO = 1;
  const WARNING = 2;
  const FAILURE = 3;
  const FATAL = 4;

  private $message;
  private $category = self::INFO;

  public function setMessage($message) {
    $this->message = $message;
    return $this;
  }

  public function getMessage() {
    return $this->message;
  }

  public function setCategory($category) {
    $this->category = $category;
    return $this;
  }

  public function getCategory() {
    return $this->category;
  }

  private function getCategoryClass() {
    switch ($this->getCategory()) {
    case self::SUCCESS: return 'success';
    case self::INFO: return 'info';
    case self::WARNING: return 'warning';
    case self::FAILURE: return 'failure';
    case self::FATAL: return 'fatal';
    }
  }

  public function toTag() {
    return tag(
      'div',
      tag('p', $this->getMessage()),
      array(
        'class' => 'alert alert-'.$this->getCategoryClass(),
      )
    );
  }
}
