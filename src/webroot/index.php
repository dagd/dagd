<?php

// Resources that help us do cool things.
require_once dirname(dirname(__FILE__)).'/resources/global_resources.php';
require_once dirname(__FILE__).'/resources/php/index_resources.php';

// All of the applications that we route too.
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
    'No __path__ GET variable was found.'.
    'Your rewrite rules are incorrect!');
}

$requested_path = $_GET['__path__'];

if (is_text_useragent()) {
  // If a text useragent hits the site, check against text.redirects first.
  $text_redirects = DaGdConfig::get('text.redirects');
  foreach ($text_redirects as $request => $redirect) {
    if (preg_match('#^'.$request.'#', $requested_path)) {
      $new_route = preg_replace('#'.$request.'#', $redirect, $requested_path);
      header('Location: '.$new_route);
      return;
    }
  }
}

$route_matches = null;
$controller_match = null;
$routes = DaGdConfig::get('general.routemap');
foreach ($routes as $route => $controller) {
  if(preg_match('#^'.$route.'#', $requested_path, $route_matches)) {
    $controller_match = $controller;
    break;
  }
}

$DEBUG = DaGdConfig::get('general.debug');
if (!$route_matches) {
  error404();
  if (!$DEBUG) {
    die();
  }
}

debug('Route matches', print_r($route_matches, true));
debug('Controller', $controller_match);
debug('Pass-off', 'Passing off to controller.');

$git_dir = dirname($_SERVER['SCRIPT_FILENAME']).'/../../.git/';
$git_latest_commit = shell_exec('git --git-dir='.$git_dir.' log -1 --pretty=format:%h');
header('X-Git-Commit: '.$git_latest_commit);

$instance = new ReflectionClass($controller_match);
$instance = $instance->newInstance();
$instance->setRouteMatches($route_matches);
debug('Response from Controller', '');
echo $instance->finalize();

if (!isset($_GET['strip'])) {
  echo "\n";
}
