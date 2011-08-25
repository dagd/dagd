<?php

require_once dirname(dirname(__FILE__)).'/config.php';
require_once dirname(__FILE__).'/dagdmarkup.php';

function handle_exception(Exception $e) {
  $debug = DaGdConfig::get('general.debug');
  if ($debug) {
    echo $e."<br />\n";
  }
  echo 'An error has occurred within dagd! Sorry about that!';
  header('HTTP/1.0 500 Internal Server Error (Exception)');
  die();
}
set_exception_handler('handle_exception');

function is_text_useragent() {
  return preg_match(
    '#(?:'.DaGdConfig::get('general.text_useragent_search').')#',
    $_SERVER['HTTP_USER_AGENT']);
}

function debug($title, $text=null) {
  $debug = DaGdConfig::get('general.debug');
  if ($debug) {
    echo '<h5>'.$title.'</h5>';
    if ($text) {
      echo '<div style="color: red;"><pre>'.$text.'</pre></div><br />';
    }
  }
}