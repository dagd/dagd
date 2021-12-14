<?php

/**
 * Effectively a blend of DaGdCLIFullWidthTable and DaGdCLIMinimumWidthTable.
 *
 * This is a full-width table, but all columns are minimal-width except a
 * glorified "weighted" column, whose zero-based index is specified in the
 * constructor.
 */
final class DaGdCLIWeightedColumnTable extends DaGdCLITable {
  private $weighted_column;

  public function __construct($weighted_column) {
    // Index of column that is weighted
    $this->weighted_column = $weighted_column;
    $this->terminal_width = (int)exec("tput cols");
  }

  public function setWeightedColumn($weighted_column) {
    $this->weighted_column = $weighted_column;
    return $this;
  }

  public function getWeightedColumn() {
    return $this->weighted_column;
  }

  public function addRow($row) {
    $all_but_weighted_width = 0;

    foreach ($row->getCells() as $idx => $cell) {
      if ($idx != $this->getWeightedColumn()) {
        $all_but_weighted_width += $cell->getMinimumWidth() + 4;
      }
      $this->column_widths[$idx] = $cell->getMinimumWidth();
    }
    $leftover = $this->terminal_width - $all_but_weighted_width;
    $this->column_widths[$this->getWeightedColumn()] = $leftover + 1;
    return parent::addRow($row);
  }
}
