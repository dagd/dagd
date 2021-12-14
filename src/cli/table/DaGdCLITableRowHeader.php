<?php

final class DaGdCLITableRowHeader extends DaGdCLITableRow {
  public function render(
    array $widths,
    $is_last_row = false,
    $row_separation = true
  ) {
    $row_width = array_sum($widths) + count($widths) + 2;

    // Top Border
    $top = str_repeat("\xE2\x94\x80", $row_width);

    // Top left Corner
    echo "\xE2\x94\x8c";

    // Top line border
    foreach ($this->getCells() as $idx => $cell) {
      echo str_repeat("\xE2\x94\x80", $widths[$idx] + 1);
      if ($idx != count($widths) - 1) {
        echo "\xE2\x94\xac";
      }
    }

    // Top right corner
    echo "\xE2\x94\x90\n";

    // Start of row
    echo "\xE2\x94\x82 ";

    foreach ($this->getCells() as $idx => $cell) {
      $contents = $cell->render($widths[$idx], false);
      echo $this->getCLI()->bold($contents);
      echo "\xE2\x94\x82";

      // If we aren't the last column, add a padding space.
      if ($idx != count($widths) - 1) {
        echo ' ';
      }
    }
    echo "\n";

    // Bottom Border
    echo "\xE2\x94\x9d";
    foreach ($this->getCells() as $idx => $cell) {
      echo str_repeat("\xE2\x94\x81", $widths[$idx] + 1);
      if ($idx != count($widths) - 1) {
        echo "\xE2\x94\xbf";
      }
    }
    echo "\xE2\x94\xa5";
  }
}
