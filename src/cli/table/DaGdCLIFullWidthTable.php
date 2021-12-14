<?php

/**
 * A CLI table that takes up the full terminal width by creating equal-width
 * columns (rounding where necessary).
 *
 * Depends on `tput cols` working.
 */
final class DaGdCLIFullWidthTable extends DaGdCLITable {
  private $terminal_width;

  public function __construct() {
    $this->terminal_width = (int)exec("tput cols");
  }

  public function addRow($row) {
    $num_cells = count($row->getCells());
    foreach ($row->getCells() as $idx => $cell) {
      $this->column_widths[$idx] = floor($this->terminal_width / $num_cells) - 2;
    }
    return parent::addRow($row);
  }
}
