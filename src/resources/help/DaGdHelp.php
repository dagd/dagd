<?php

/**
 * A new help system for da.gd.
 */
final class DaGdHelp {
  private $title;
  private $help_path;
  private $description;
  private $paths = array();
  private $examples = array();
  private $warnings = array();

  public function setTitle($title) {
    $this->title = $title;
    return $this;
  }

  public function getTitle() {
    return $this->title;
  }

  public function setHelpPath($help_path) {
    $this->help_path = $help_path;
    return $this;
  }

  public function getHelpPath() {
    return $this->help_path;
  }

  public function setDescription($description) {
    $this->description = $description;
    return $this;
  }

  public function getDescription() {
    return $this->description;
  }

  public function setPaths($paths) {
    $this->paths = $paths;
    return $this;
  }

  public function addPath(DaGdHelpPath $path) {
    $this->paths[] = $path;
    return $this;
  }

  public function getPaths() {
    return $this->paths;
  }

  public function setWarnings($warnings) {
    $this->warnings = $warnings;
    return $this;
  }

  public function addWarning($warning) {
    $this->warnings[] = $warning;
    return $this;
  }

  public function getWarnings() {
    return $this->warnings;
  }

  /**
   * This only exists so controllers can define help the new way and the
   * old (current) help system can render it.
   */
  public function toOldHelp() {
    $examples = array();

    foreach ($this->getPaths() as $path) {
      $path_examples = $path->getExamples();
      if ($path_examples) {
        foreach ($path_examples as $example) {
          $examples[] = array(
            'summary' => $example->getCommentary(),
            'arguments' => $example->getPathArgs(),
            'request' => $example->getGetArgs(),
          );
        }
      } else if (empty($examples)) {
        $examples[] = array(
          'summary' => null,
          'arguments' => null,
          'request' => null,
        );
      }
    }

    return array(
      'title' => $this->getTitle(),
      'summary' => $this->getDescription(),
      // Old help only supports one path
      'path' => $this->getPaths()[0]->getPath(),
      'examples' => $examples,
    );
  }
}
