<?php

final class DaGdHelpPath {
  private $path;
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
    $this->examples[] = $example;
    return $this;
  }

  public function getExamples() {
    return $this->examples;
  }
}
