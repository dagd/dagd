<?php

require_once dirname(dirname(__FILE__)).'/config.php';
require_once dirname(__FILE__).'/dagdmarkup.php';
require_once dirname(__FILE__).'/sql.php';

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
  if (array_key_exists('HTTP_USER_AGENT', $_SERVER)) {
    return preg_match(
      '#(?:'.DaGdConfig::get('general.text_useragent_search').')#',
      $_SERVER['HTTP_USER_AGENT']);
  } else {
    // If the useragent field is not defined for whatever reason, assume it
    // is a text/cli thing. Browsers are smart, they know how to send a UA. ;)
    return true;
  }
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

function error404($echo = '404 - route not found', $status_text = 'Not Found') {
  header('HTTP/1.0 404 '.$status_text);
  echo $echo;
}

function error400($echo = '400 - bad request', $status_text = 'Bad Request') {
  header('HTTP/1.0 400 '.$status_text);
  echo $echo;
}