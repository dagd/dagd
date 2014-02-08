<?php

// Resources that help us do cool things.
require_once dirname(dirname(__FILE__)).'/resources/global_resources.php';
require_once dirname(__FILE__).'/resources/php/index_resources.php';

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

if (!$_GET['__path__']) {
  throw new Exception(
    'No __path__ GET variable was found. '.
    'Your rewrite rules are incorrect!');
}

$required_extensions = DaGdConfig::get('general.required_extensions');
foreach ($required_extensions as $extension) {
  if (!extension_loaded($extension)) {
    throw new Exception(
      'Missing extension is required: '.$extension);
  }
}

$requested_path = $_GET['__path__'];
$route_matches = null;
$controller_match = null;
$routes = array();
$routes += DaGdConfig::get('general.redirect_map');

if (!is_html_useragent()) {
  $routes += DaGdConfig::get('general.cli_routemap');
}
$routes += DaGdConfig::get('general.routemap');

foreach ($routes as $route => $controller) {
  if (preg_match('#^'.$route.'#', $requested_path, $route_matches)) {
    if (preg_match('#^https?://#', $controller)) {
      // If the "controller" side starts with http://, we can just redirect.
      // This lets us do things like '/foo/(.*)' => 'http://google.com/$1'
      array_shift($route_matches);
      $new_location = preg_replace('@^'.$route.'@', $controller, $requested_path);
      $new_location .= build_given_querystring();
      debug('New Location', $new_location);
      header('Location: '.$new_location);
      return;
    } else {
      $controller_match = $controller;
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

debug('REQUEST variables', print_r($_REQUEST, true));
debug('Route matches', print_r($route_matches, true));
debug('Controller', $controller_match);
debug('Pass-off', 'Passing off to controller.');

// Extra headers
$headers = DaGdConfig::get('general.extra_headers');
foreach ($headers as $header) {
  header($header);
}
$git_dir = escapeshellarg(dirname($_SERVER['SCRIPT_FILENAME']).'/../../.git/');
$git_latest_commit = shell_exec(
  'git --git-dir='.$git_dir.' log -1 --pretty=format:%h');
header('X-Git-Commit: '.$git_latest_commit);

$instance = new ReflectionClass($controller_match);
$instance = $instance->newInstance();
$instance->setRouteMatches($route_matches);
debug('Response from Controller', '');
echo $instance->finalize();

if (!isset($_REQUEST['strip'])) {
  echo "\n";
}
