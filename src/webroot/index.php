<?php

// Resources that help us do cool things.
require_once dirname(dirname(__FILE__)).'/resources/global_resources.php';

$start = microtime(true);

require_application('base');

// All of the applications that we route to.
// This is a tad inefficient as we actually load every app's code into
// mem on each page load, and don't end up using it. If this ends up biting us
// at some point, we can optimize this.
$applications = DaGdConfig::get('general.applications');
foreach ($applications as $application) {
  require_application($application);
}

ini_set('user_agent', DaGdConfig::get('general.useragent'));

$required_extensions = DaGdConfig::get('general.required_extensions');
foreach ($required_extensions as $extension) {
  if (!extension_loaded($extension)) {
    throw new Exception(
      'Missing extension is required: '.$extension);
  }
}

$request_uri = $_SERVER['REQUEST_URI'];
$requested_path = preg_replace('/\?.*/', '', $request_uri);
$request_method = $_SERVER['REQUEST_METHOD'];
$route_matches = null;
$metadata_match = null;
$regex_match_wrong_method = false;
$routes = array();
$routes += DaGdConfig::get('general.redirect_map');

if (!is_html_useragent()) {
  $routes += DaGdConfig::get('general.cli_routemap');
}
$routes += DaGdConfig::get('general.routemap');

foreach ($routes as $route => $metadata) {
  if (preg_match('#^'.$route.'#', $requested_path, $route_matches)) {
    if (is_string($metadata) && preg_match('#^https?://#', $metadata)) {
      // If the "controller" side starts with http://, we can just redirect.
      // This lets us do things like '/foo/(.*)' => 'http://google.com/$1'
      array_shift($route_matches);
      $new_location = preg_replace(
          '@^'.$route.'@',
          $metadata,
          $requested_path);
      $new_location .= build_given_querystring();
      debug('New Location', $new_location);
      header('Location: '.$new_location);
      return;
    } else {
      if (!array_key_exists('methods', $metadata)) {
        $default_methods = DaGdConfig::get('general.default_methods');
        $metadata['methods'] = $default_methods;
      }
      if (!in_array($request_method, $metadata['methods'])) {
        // If the current request method doesn't match, continue on, but
        // mark that we found a controller that regex-matched, so we can return
        // a 405 instead of a 404.
        $regex_match_wrong_method = true;
        continue;
      }
      $metadata_match = $metadata;
      $regex_match_wrong_method = false;
      break;
    }
  }
}

$debug = DaGdConfig::get('general.debug');

if (!$route_matches) {
  error404();
  if (!$debug) {
    die();
  }
}

if ($regex_match_wrong_method) {
  error405();
  if (!$debug) {
    die();
  }
}

debug('REQUEST variables', print_r($_REQUEST, true));
debug('Route matches', print_r($route_matches, true));
debug('Controller', $metadata_match['controller']);
debug('Metadata', print_r($metadata_match, true));
debug('Pass-off', 'Passing off to controller.');

$instance = new ReflectionClass($metadata_match['controller']);
$instance = $instance->newInstance();

$write_dbh = new mysqli(
  DaGdConfig::get('mysql.host'),
  DaGdConfig::get('mysql.user'),
  DaGdConfig::get('mysql.password'),
  DaGdConfig::get('mysql.database'));

$read_dbh = $write_dbh;
$readonly_host = DaGdConfig::get('readonly_mysql.host');

if (!empty($readonly_host)) {
  $read_dbh = new mysqli(
    DaGdConfig::get('readonly_mysql.host'),
    DaGdConfig::get('readonly_mysql.user'),
    DaGdConfig::get('readonly_mysql.password'),
    DaGdConfig::get('readonly_mysql.database'));
}

debug('Response from Controller', '');

$response = $instance
  ->setRouteMatches($route_matches)
  ->setWriteDB($write_dbh)
  ->setReadDB($read_dbh)
  ->finalize();

$git_dir = escapeshellarg(dirname($_SERVER['SCRIPT_FILENAME']).'/../../.git/');
$git_latest_commit = shell_exec(
  'git --git-dir='.$git_dir.' log -1 --pretty=format:%h');

// Temporary, handle migration to DaGdResponse
if ($response instanceof DaGdResponse) {
  $response->addHeader('X-Git-Commit', $git_latest_commit);
  echo $response->render();
} else {
  // DaGdResponse handles adding these itself, but legacy controllers need them
  // added here before they get rendered.
  $headers = DaGdConfig::get('general.extra_headers');
  foreach ($headers as $header) {
    header($header);
  }
  header('X-Git-Commit: '.$git_latest_commit);
  echo $response;
}

$end = microtime(true);
statsd_time('response_time', ($end - $start) * 1000);
