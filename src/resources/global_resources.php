<?php

$config_file = getenv('DaGdConfigFile');
if (!$config_file) {
  $config_file = 'config.php';
}
require_once dirname(dirname(__FILE__)).'/'.$config_file;

$display_errors = DaGdConfig::get('general.display_errors');
if ($display_errors) {
  ini_set('error_reporting', E_ALL);
  ini_set('display_startup_errors', true);
  ini_set('display_errors', true);
}

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
  if (!isset($_REQUEST['text']) &&
    array_key_exists('HTTP_USER_AGENT', $_SERVER)) {
    $useragents = DaGdConfig::get('general.text_useragent_search');
    $useragents = implode('|', $useragents);
    return preg_match(
      '#(?:'.$useragents.')#',
      $_SERVER['HTTP_USER_AGENT']);
  } else {

    // Force text useragent response to be off...
    if (request_or_default('text')) {
      return false;
    }

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

function request_or_default($key, $default = null) {
  if (array_key_exists($key, $_REQUEST) && strlen($_REQUEST[$key])) {
    return $_REQUEST[$key];
  } else {
    return $default;
  }
}

/*
 * Takes the given query string, minus __path__ used internally,
 * and makes one that we can use for various things like redirecting.
 * It also allows for empty values. ?foo&asdf=fdsa is valid.
 *
 * @returns string The query string, starting with a '?'.
 */
function build_given_querystring() {
  $querystring = '?';
  foreach ($_GET as $key => $value) {
    if ($key != '__path__') {
      $querystring .= $key;
      if (!empty($value)) {
        $querystring .= '='.$value;
      }
      $querystring .= '&';
    }
  }

  foreach ($_POST as $key => $value) {
    $querystring .= $key;
    if (!empty($value)) {
      $querystring .= '='.$value;
    }
    $querystring .= '&';
  }

  return rtrim($querystring, '&');
}