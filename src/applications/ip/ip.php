<?php
class DaGdIPController extends DaGdBaseClass {
  public function render() {
    return $_SERVER['REMOTE_ADDR'];
  }
}