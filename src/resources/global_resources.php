<?php

$config_file = getenv('DaGdConfigFile');

if (!$config_file ||
    !@include_once(dirname(dirname(__FILE__))).'/'.$config_file) {
  throw new Exception("No configuration file could be loaded.");
}

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
  $email = DaGdConfig::get('exceptions.email');
  $email_in_debug = DaGdConfig::get('exceptions.email_in_debug');
  $mail_to = DaGdConfig::get('exceptions.mail_to');
  $really_send_email = false;

  if ($debug) {
    if ($email && $email_in_debug) {
      $really_send_email = true;
    }
    echo $e."<br />\n";
  } else {
    if ($email) {
      $really_send_email = true;
    }
  }

  echo 'An error has occurred within dagd! Sorry about that!';
  header('HTTP/1.0 500 Internal Server Error (Exception)');

  if ($really_send_email) {
    mail(
      implode(',', $mail_to),
      '[DAGD Exception] '.$e->getMessage(),
      'Greetings,
You are receiving this email because da.gd has had an exception, which caused
us to render a 500 error.

The following exception was raised and uncaught:
'.$e->getMessage().'

It happened in file: '.$e->getFile().'
on line: '.$e->getLine().'
around the following code: '.$e->getCode().'

Trace:
'.$e->getTraceAsString().'

Please commit a fix for this as soon as possible.
Thanks,
Your friendly da.gd server',
      'From: php@da.gd');
  }
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

    // Force text useragent response to be on/off...
    $force_text = request_or_default('text');
    if ($force_text != 0 || $force_text == null) {
      return true;
    } else {
      return false;
    }

    // If the useragent field is not defined for whatever reason, assume it
    // is a text/cli thing. Browsers are smart, they know how to send a UA. ;)
    return true;
  }
}

function debug($title, $text = null) {
  $debug = DaGdConfig::get('general.debug');
  if ($debug) {
    echo '<h5>'.$title.'</h5>';
    if ($text) {
      echo '<div style="color: red;"><pre>'.$text.'</pre></div><br />';
    }
  }
}

function error404($echo = '404 - route not found', $status_text = 'Not Found') {
  header('HTTP/1.1 404 '.$status_text);
  echo $echo;
}

function error400($echo = '400 - bad request', $status_text = 'Bad Request') {
  header('HTTP/1.1 400 '.$status_text);
  echo $echo;
}

function idx(array $array, $key, $default = null) {
  if (array_key_exists($key, $array) && strlen($array[$key])) {
    return $array[$key];
  } else {
    return $default;
  }
}

function request_or_default($key, $default = null) {
  return idx($_REQUEST, $key, $default);
}

function server_or_default($key, $default = null) {
  return idx($_SERVER, $key, $default);
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

/** Get the IP for a client.
 *  Use the header X-Forwarded-For if it exists.
 */
function client_ip() {
  if (server_or_default('HTTP_DAGD_PROXY') == "1" &&
      $ip = server_or_default('HTTP_X_FORWARDED_FOR')) {
    return $ip;
  } else {
    return $_SERVER['REMOTE_ADDR'];
  }
}