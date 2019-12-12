<?php
function randstr($length) {
  $charset = DaGdConfig::get('shorten.random_charset');
  $result = '';

  while ($length--) {
    $result .= $charset[mt_rand(0, strlen($charset)-1)];
  }

  return $result;
}
