<?php
class DaGdUserAgentController extends DaGdBaseClass {
  public function render() {
    return $_SERVER['HTTP_USER_AGENT'];
  }
}