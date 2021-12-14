<?php

class DaGdCLITableCell {
  private $contents;

  public function __construct($contents) {
    $this->contents = $contents;
  }

  public function setContents($contents) {
    $this->contents = $contents;
    return $this;
  }

  public function getContents() {
    return $this->contents;
  }

  /**
   * Calculate minimum width needed to render this cell.
   *
   * This includes 2 padding spaces, but no borders. ANSI codes do not count.
   */
  public function getMinimumWidth() {
    return DaGdCLI::strlen($this->getContents()) + 2;
  }

  public function render($width, $echo = true) {
    $len = DaGdCLI::strlen($this->getContents());
    if ($echo) {
      echo $this->getContents().str_repeat(' ', $width - $len);
    }
    return $this->getContents().str_repeat(' ', $width - $len);
  }
}
