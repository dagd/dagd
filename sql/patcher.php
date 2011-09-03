#!/usr/bin/env php
<?php
require_once dirname(__FILE__).'/../src/resources/global_resources.php';
$current_schema = (int)file_get_contents(dirname(__FILE__).'/current_schema');
$patch_number = 0;

foreach (glob(dirname(__FILE__).'/*.sql') as $patch) {
  $patch_number = explode('sql/', $patch, 2);
  $patch_number = explode('.', $patch_number[1], 2);
  $patch_number = (int)$patch_number[0];
  if ($patch_number <= $current_schema) {
    continue;
  }
  echo "Applying SQL patch {$patch}...\n";
  $raw_query = file_get_contents($patch);
  if ($__db_handler->multi_query($raw_query)) {
    do {
      if ($result = $__db_handler->store_result()) {
        $result->free();
      }
    } while ($__db_handler->next_result());
  }
}

$patch_number++;
file_put_contents(dirname(__FILE__).'/current_schema', $patch_number++);