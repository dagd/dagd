<?php

function idx(array $array, $key, $default = null) {
  if (isset($array[$key])) {
    return $array[$key];
  } else {
    return $default;
  }
}

function intersperse($glue, array $pieces) {
  $len = count($pieces);
  $out = array();
  $i = 0;
  foreach ($pieces as $piece) {
    $out[] = $piece;
    if ($i != $len - 1) {
      $out[] = $glue;
    }
    $i++;
  }
  return $out;
}
