<?php

abstract class DaGdCLITable {
  private $rows = array();
  protected $column_widths = array();
  private $row_separation = true;

  public function setRows($rows) {
    $this->rows = $rows;
    return $this;
  }

  public function getRows() {
    return $this->rows;
  }

  public function addRow($row) {
    $this->rows[] = $row;
    return $this;
  }

  public function getColumnWidths() {
    return $this->column_widths;
  }

  public function setRowSeparation($row_separation) {
    $this->row_separation = $row_separation;
    return $this;
  }

  public function getRowSeparation() {
    return $this->row_separation;
  }

  public function render() {
    foreach ($this->getRows() as $idx => $row) {
      $is_last_row = $idx == count($this->getRows()) - 1;
      $row->render(
        $this->getColumnWidths(),
        $is_last_row,
        $this->getRowSeparation());
      echo "\n";
    }
  }
}
