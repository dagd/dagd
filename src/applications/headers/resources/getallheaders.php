<?php

// Attempt a fallback if getallheaders() doesn't exist for some reason.
// via http://www.php.net/manual/en/function.getallheaders.php#84262
if (!function_exists('getallheaders')) {
  function getallheaders() {
    $headers = [];
    foreach ($_SERVER as $name => $value) {
      if (substr($name, 0, 5) == 'HTTP_') {
        $hdr_name = str_replace(
          ' ',
          '-',
          ucwords(strtolower(str_replace('_', ' ', substr($name, 5)))));
        $headers[$hdr_name] = $value;
      }
    }
    return $headers;
  }
}