<?php

final class DaGdCLIMinimumWidthTable extends DaGdCLITable {
  public function addRow($row) {
    foreach ($row->getCells() as $idx => $cell) {
      if (idx($this->column_widths, $idx, 0) < $cell->getMinimumWidth()) {
        $this->column_widths[$idx] = $cell->getMinimumWidth() - 1;
      }
    }
    return parent::addRow($row);
  }
}
