<?php
require_once dirname(__FILE__).'/../base/init.php';

class DaGdIPController extends DaGdBaseClass {
  public function render() {
    return $_SERVER['REMOTE_ADDR'];
  }
}