<?php

// If we are to care about backwards compatibility, we are somewhat locked into
// doing this setup in this file, because scripts can (and do, in /scripts/)
// include this file for their setup.
require_once dirname(__FILE__).'/startup/DaGdStartup.php';

$config_file = getenv('DaGdConfigFile');
// We don't have id() loaded yet.
$framework = new DaGdStartup();
$framework
  ->loadConfig($config_file)
  ->establishGlobalState()
  ->establishAutoloader();

// TODO: Move this into DaGdStartup, and clean it up.
function handle_exception($e) {
  // Handle logging the exception early in case anything below fails.
  $log_msg = 'Exception ('.$e->getFile().', line '.$e->getLine().'): ';
  $log_msg .= $e->getMessage();
  $log_msg .= "\nTrace follows:\n".$e->getTraceAsString();
  error_log($log_msg);

  $debug = DaGdConfig::get('general.debug');
  $display_errors = DaGdConfig::get('general.display_errors');
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

  statsd_bump('unhandled_exception');
  statsd_bump('status,code=500');

  $request = id(new DaGdRequest())
    ->setCookies($_COOKIE)
    ->setRequest($_REQUEST)
    ->setServer($_SERVER);

  $ctrl = new DaGd500Controller();
  $ctrl->setRequest($request);
  $ctrl->setException($e);
  $ctrl->finalize()->render();

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

// TODO: Nix this
function require_application($name) {
  require_once dirname(__FILE__).'/../applications/'.$name.'/'.$name.'.php';
  return;
}

// TODO: Roll to DaGdRequest
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

// TODO: Roll this into DaGdController's debug card stuff.
function debug($title, $text = null) {
  trigger_error('debug() is now a no-op and should not be called.');
}

// TODO: Remove these error*() functions
function error404($echo = '404 - route not found', $status_text = 'Not Found') {
  statsd_bump('status,code=404');
  header('HTTP/1.1 404 '.$status_text);
  echo $echo;
}

function error403($echo = '403 - forbidden', $status_text = 'Forbidden') {
  statsd_bump('status,code=403');
  header('HTTP/1.1 403 '.$status_text);
  echo $echo;
}

function error400($echo = '400 - bad request', $status_text = 'Bad Request') {
  statsd_bump('status,code=400');
  header('HTTP/1.1 400 '.$status_text);
  echo $echo;
}

function error405(
  $echo = '405 - method not allowed',
  $status_text = 'Method not allowed') {
  statsd_bump('status,code=405');
  header('HTTP/1.1 405 '.$status_text);
  echo $echo;
}

function error500(
  $echo = '500 - internal server error',
  $status_text = 'Internal Server Error') {
  statsd_bump('status,code=500');
  header('HTTP/1.1 500 '.$status_text);
  echo $echo;
}

// Already replaced by DaGdRequest#getParamOrDefault.
// TODO: Nix this.
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

// Replaced by idx(DaGdRequest->getServer(), $key, $default)
// TODO: Nix this.
function server_or_default($key, $default = null) {
  return idx($_SERVER, $key, $default);
}

// TODO: Move to DaGdRequest.
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

// TODO: Nix this, already replaced.
/** Get the IP for a client.
 *  Use the header X-Forwarded-For if it exists.
 *
 * Deprecated: New controllers should use DaGdRequest#getClientIP
 */
function client_ip() {
  if (server_or_default('HTTP_X_DAGD_PROXY') == "1" &&
      $ip = server_or_default('HTTP_X_FORWARDED_FOR')) {
    return $ip;
  } else {
    return $_SERVER['REMOTE_ADDR'];
  }
}

// TODO: Nix this after new help system is finished.
/** Get help for a given class. */
function help($class) {
  $prefix = request_or_default('url_prefix', '/');
  $separator = request_or_default('url_separator', '/');
  $request_sep = request_or_default('url_request_sep', null);

  $return = '';

  $help_getter = new ReflectionClass($class);
  $instance = $help_getter->newInstance();
  $help = $instance->getHelp();
  if ($help instanceof DaGdHelp) {
    // Until the help system is rewritten entirely
    $help = $help->toOldHelp();
  }
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
      if (array_key_exists('request', $example) &&
          is_array($example['request'])) {
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

// TODO: Move to utility, but also make it stop using shorten's config because
// that's weird.
function randstr($length) {
  $charset = DaGdConfig::get('shorten.random_charset');
  $result = '';

  while ($length--) {
    $result .= $charset[mt_rand(0, strlen($charset)-1)];
  }

  return $result;
}
