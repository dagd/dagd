<?php

abstract class DaGdCLITableRow {
  private $cells = array();
  protected $cli;

  public function __construct(DaGdCLI $cli) {
    $this->cli = $cli;
  }

  public function setCells($cells) {
    $this->cells = $cells;
    return $this;
  }

  public function getCells() {
    return $this->cells;
  }

  public function addCell(DaGdCLITableCell $cell) {
    $this->cells[] = $cell;
    return $this;
  }

  public function addCells(array $cells) {
    foreach ($cells as $cell) {
      $this->addCell($cell);
    }
    return $this;
  }

  public function getCLI() {
    return $this->cli;
  }

  abstract public function render(
    array $widths,
    $is_last_row = false,
    $row_separation = true
  );
}
