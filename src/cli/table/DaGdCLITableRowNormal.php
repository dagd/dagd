<?php

final class DaGdCLITableRowNormal extends DaGdCLITableRow {
  public function render(
    array $widths,
    $is_last_row = false,
    $row_separation = true
  ) {
    echo "\xe2\x94\x82 ";

    foreach ($this->getCells() as $idx => $cell) {
      $cell->render($widths[$idx]);
      echo "\xe2\x94\x82";

      // If we aren't the last column, add a padding space.
      if ($idx != count($widths) - 1) {
        echo ' ';
      }
    }

    if (!$is_last_row) {
      if ($row_separation) {
        echo "\n\xE2\x94\x9c";
        foreach ($this->getCells() as $idx => $cell) {
          echo str_repeat("\xe2\x94\x80", $widths[$idx] + 1);
          if ($idx != count($widths) - 1) {
            // Cell separator
            echo "\xe2\x94\xbc";
          }
        }
        echo "\xE2\x94\xa4";
      }
      return;
    }

    // Bottom border:
    // newline + bottom left corner
    echo "\n\xe2\x94\x94";

    foreach ($this->getCells() as $idx => $cell) {
      echo str_repeat("\xe2\x94\x80", $widths[$idx] + 1);
      if ($idx != count($widths) - 1) {
        // Cell separator
        echo "\xe2\x94\xb4";
      }
    }

    // Bottom right corner
    echo "\xe2\x94\x98";
  }
}
