#!/usr/bin/env php
<?php

require_once dirname(__FILE__).'/../src/resources/global_resources.php';

// We manage our own raw connection handler here; this might be better
// abstracted somewhere at some point.
$__db_handler = DaGdStartup::getWritableDbh();

$patch_number = 0;
$current_schema = 0;
if (file_exists(dirname(__FILE__).'/current_schema')) {
  $current_schema = (int)file_get_contents(dirname(__FILE__).'/current_schema');
}

$patches_to_apply = array();
$all_patches = glob(dirname(__FILE__).'/*.sql');

// Loop through all the files, and find ones that need to be applied.
foreach ($all_patches as $patch) {
  $patch_number = explode('sql/', $patch, 2);
  $patch_number = explode('.', $patch_number[1], 2);
  $patch_number = (int)$patch_number[0];
  if ($patch_number > $current_schema) {
    $patches_to_apply[] = $patch;
  }
}

if (count($patches_to_apply)) {
  // Ask for a confirmation of what we're about to do.
  echo "The following patches will be applied:\n";
  echo implode("\n", $patches_to_apply)."\n";
  if (idx($argv, 1) != '--yes') {
    echo "*** PRESS CTRL-C TO ABORT, OR ENTER TO CONTINUE ***";
    fgets(STDIN);
  }

  foreach ($patches_to_apply as $patch) {
    echo "Applying SQL patch {$patch}...\n";
    $raw_query = file_get_contents($patch);
    if ($__db_handler->multi_query($raw_query)) {
      do {
        if ($result = $__db_handler->store_result()) {
          $result->free();
        }
      } while ($__db_handler->more_results() && $__db_handler->next_result());
    } else {
      echo 'ERROR: '.$__db_handler->error."\n";
      echo "*** EXITING ***\n";
      exit(1);
    }
  }
} else {
  echo "No patches to apply. :-)\n";
}

file_put_contents(dirname(__FILE__).'/current_schema', $patch_number++);
