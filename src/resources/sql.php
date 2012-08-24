<?php

// For better or worse, right now this is our gateway to the world of SQL.
if (!function_exists('mysqli_connect')) {
  throw new Exception(
    'da.gd heavily depends on mysqli, and this library was not found. Please '.
    'ensure that mysqli is installed, then restart your httpd and try again.');
}

$__db_handler = new mysqli(
    DaGdConfig::get('mysql.host'),
    DaGdConfig::get('mysql.user'),
    DaGdConfig::get('mysql.password'),
    DaGdConfig::get('mysql.database'));
