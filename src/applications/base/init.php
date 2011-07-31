<?php
abstract class DaGdBaseClass {
  protected $escape = true;
  
  public function __construct() {}

  public function render() {
    return 'Override this method to make stuff happen!';
  }

  public function finalize() {
    if ($this->escape) {
      return htmlspecialchars($this->render());
    } else {
      return $this->render();
    }
  }
}