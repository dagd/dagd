<?php

final class DaGdHelpExample {
  private $path;
  private $path_args = array();
  private $get_args = array();
  private $commentary;

  public function setPath($path) {
    $this->path = $path;
    return $this;
  }

  public function getPath() {
    return $this->path;
  }

  public function setPathArgs($path_args) {
    $this->path_args = $path_args;
    return $this;
  }

  public function getPathArgs() {
    return $this->path_args;
  }

  public function addPathArg($path_arg) {
    $this->path_args[] = $path_arg;
    return $this;
  }

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

  public function render() {
    $baseurl = DaGdConfig::get('general.baseurl');
    $out = $baseurl.'/'.$this->getPath()->getPath().'/';
    $out .= implode('/', $this->getPathArgs());
    $first = true;
    foreach ($this->getGetArgs() as $arg => $value) {
      $prefix = '&';
      if ($first) {
        $prefix = '?';
      }
      $out .= $prefix.$arg.'='.$value;
      $first = false;
    }
    return $out;
  }
}
