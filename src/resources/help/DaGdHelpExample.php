<?php

final class DaGdHelpExample {
  private $get_args = array();
  private $commentary;

  public function setGetArgs($get_args) {
    $this->get_args = $get_args;
    return $this;
  }

  public function addGetArg($key, $value) {
    $this->get_args[$key] = $value;
    return $this;
  }

  public function getGetArgs() {
    return $this->get_args;
  }

  public function setCommentary($commentary) {
    $this->commentary = $commentary;
    return $this;
  }

  public function getCommentary() {
    return $this->commentary;
  }
}
