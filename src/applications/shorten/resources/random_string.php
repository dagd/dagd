<?php
function randstr($length) {
  $charset = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  $result = '';

  while ($length--) {
    $result .= $charset[mt_rand(0, strlen($charset)-1)];
  }

  return $result;
}
