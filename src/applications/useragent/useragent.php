<?php
require_once dirname(__FILE__).'/../base/init.php';

class DaGdUserAgentController extends DaGdBaseClass {
  public function render() {
    return $_SERVER['HTTP_USER_AGENT'];
  }
}