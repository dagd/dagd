<?php

$config_file = getenv('DaGdConfigFile');

if (!$config_file ||
    !@include_once(dirname(dirname(__FILE__))).'/'.$config_file) {
  throw new Exception("No configuration file could be loaded.");
}

function __dagd_autoload($cls) {
  $paths = DaGdConfig::get('general.autoload_search');
  foreach ($paths as $path) {
    $path = trim($path, '/').'/';
    // Paths are expected to be relative to 'src'
    $src = dirname(dirname(__FILE__));
    $files = glob($src.'/'.$path.'/'.$cls.'.php');
    if (!empty($files)) {
      include_once $files[0];
      break;
    }
  }
}

spl_autoload_register('__dagd_autoload', $throw = true);

include_once(dirname(__FILE__).'/tag.php');
include_once(dirname(__FILE__).'/cowsay.php');
include_once(dirname(__FILE__).'/statsd.php');

$display_errors = DaGdConfig::get('general.display_errors');
if ($display_errors) {
  ini_set('error_reporting', E_ALL);
  ini_set('display_startup_errors', true);
  ini_set('display_errors', true);
}

function handle_exception($e) {
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

  statsd_bump('dagd_unhandled_exception');
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

function require_application($name) {
  require_once dirname(__FILE__).'/../applications/'.$name.'/'.$name.'.php';
  return;
}

function is_html_useragent() {
  if (!isset($_REQUEST['text']) &&
    array_key_exists('HTTP_ACCEPT', $_SERVER)) {
    $accept = strtolower(str_replace(' ', '', $_SERVER['HTTP_ACCEPT']));
    $html_accept_regex = implode('|', DaGdConfig::get('general.html_accept'));
    return preg_match('#(?:'.$html_accept_regex.')#i', $accept);
  } else {

    // Force text useragent response to be on/off...
    $force_text = request_or_default('text');
    if ($force_text != 0 || $force_text == null) {
      return false;
    } else {
      return true;
    }

    // ?text wasn't specified and there was no Accept header.
    // Default to text and assume browsers are smart.
    return false;
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

function error403($echo = '403 - forbidden', $status_text = 'Forbidden') {
  header('HTTP/1.1 403 '.$status_text);
  echo $echo;
}

function error400($echo = '400 - bad request', $status_text = 'Bad Request') {
  header('HTTP/1.1 400 '.$status_text);
  echo $echo;
}

function error405(
  $echo = '405 - method not allowed',
  $status_text = 'Method not allowed') {
  header('HTTP/1.1 405 '.$status_text);
  echo $echo;
}

function error500(
  $echo = '500 - internal server error',
  $status_text = 'Internal Server Error') {
  header('HTTP/1.1 500 '.$status_text);
  echo $echo;
}

function idx(array $array, $key, $default = null) {
  if (isset($array[$key])) {
    return $array[$key];
  } else {
    return $default;
  }
}

function request_or_default(
  $key,
  $default = null,
  $allow_empty = false,
  $empty_default = null) {
  if ($allow_empty && isset($_REQUEST[$key]) && strlen($_REQUEST[$key]) == 0) {
    return $empty_default;
  }
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
  $querystring = '';
  foreach ($_GET as $key => $value) {
    if ($key == '__path__') {
        continue;
    }
    $querystring .= $key;
    if (!empty($value)) {
      $querystring .= '='.$value;
    }
    $querystring .= '&';
  }
  if ($querystring != '') {
    $querystring = '?'.$querystring;
    $querystring = rtrim($querystring, '&');
    return $querystring;
  } else {
    return '';
  }
}

/** Get the IP for a client.
 *  Use the header X-Forwarded-For if it exists.
 */
function client_ip() {
  if (server_or_default('HTTP_X_DAGD_PROXY') == "1" &&
      $ip = server_or_default('HTTP_X_FORWARDED_FOR')) {
    return $ip;
  } else {
    return $_SERVER['REMOTE_ADDR'];
  }
}

/** Get help for a given class. */
function help($class) {
  $prefix = request_or_default('url_prefix', '/');
  $separator = request_or_default('url_separator', '/');
  $request_sep = request_or_default('url_request_sep', null);

  $return = '';

  $help_getter = new ReflectionClass($class);
  $instance = $help_getter->newInstance();
  $help = $instance->getHelp();
  if ($help) {
    $return .= '<h3 id="'.$help['title'].'">';
    $return .= '<a href="#'.$help['title'].'">#</a> ';
    $return .= $help['summary']."</h3>\n";
    $return .= '<ul>';
    foreach ($help['examples'] as $example) {
      $return .= '<li>    ';
      if ($example['summary']) {
        $return .= $example['summary'].': ';
      }
      $link = $prefix.$help['path'];
      if (array_key_exists('arguments', $example)) {
        $arguments = $example['arguments'];
        if ($arguments) {
          if ($help['path']) {
            $link .= $separator;
          }
          $link .= implode($separator, $arguments);
        }
      }
      if (array_key_exists('request', $example)) {
        $iteration = 0;
        foreach ($example['request'] as $param => $param_example) {
          if ($request_sep) {
            $link .= $request_sep;
          } else {
            $link .= ($iteration === 0) ? '?' : '&';
          }
          $link .= $param.'='.$param_example;
          $iteration++;
        }
      }
      $return .= '<a href="'.$link.'">'.$link.'</a>';
      $return .= "</li>\n";
    }
    $return .= "</ul>\n";
  }
  return $return;
}

function id($a) {
  return $a;
}
