<?php

/**
 * Determine if a string starts with a given substring.
 */
function startswith($str, $needle) {
  if (function_exists('str_starts_with')) {
    return str_starts_with($str, $needle);
  } else {
    return substr($str, 0, strlen($needle)) === $needle;
  }
}
