<?php

final class DaGdHelpPath {
  private $path;
  private $path_args = array();
  private $get_args = array();
  private $aliases = array();
  private $methods = array();
  private $examples = array();

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

  public function getGetArgs() {
    return $this->get_args;
  }

  public function addGetArg($arg, $description) {
    $this->get_args[$arg] = $description;
    return $this;
  }

  public function setAliases($aliases) {
    $this->aliases = $aliases;
    return $this;
  }

  public function getAliases() {
    return $this->aliases;
  }

  public function setMethods($methods) {
    $this->methods = $methods;
    return $this;
  }

  public function getMethods() {
    return $this->methods;
  }

  public function setExamples($examples) {
    $this->examples = $examples;
    return $this;
  }

  public function addExample(DaGdHelpExample $example) {
    $example->setPath($this);
    $this->examples[] = $example;
    return $this;
  }

  public function getExamples() {
    return $this->examples;
  }

  public function render() {
    $methods = null;
    if (count($this->getMethods()) === 1) {
      $methods = $this->getMethods()[0].' ';
    } else {
      $methods = '{'.implode(', ', $this->getMethods()).'} ';
    }

    $tags = array(
      $methods,
      '/',
      $this->getPath(),
    );
    foreach ($this->getPathArgs() as $arg) {
      $tags[] = '/<';
      $tags[] = tag('strong', $arg);
      $tags[] = '>';
    }
    return $tags;
  }
}
