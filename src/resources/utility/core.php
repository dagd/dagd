<?php

function id($a) {
  return $a;
}

function tag(
  $name,
  $body = null,
  array $attributes = array(),
  $cdata = false) {

  return id(new DaGdTag($name, $body, $attributes, $cdata));
}

function class_repr($obj) {
  $cls = 'non-object';
  if (is_string($obj)) {
    $cls = 'string';
  } else {
    $get_cls = @get_class($obj);
    if (is_string($get_cls)) {
      $cls = $get_cls;
    }
  }
  return $cls;
}

/**
 * Return true if the key exists in config, false otherwise.
 */
function config_key_exists($key) {
  return array_key_exists($key, DaGdConfig::$config);
}
