<?php

// Resources that help us do cool things.
require_once dirname(dirname(__FILE__)).'/resources/global_resources.php';
require_once dirname(dirname(__FILE__)).'/resources/index_resources.php';
require_once dirname(dirname(__FILE__)).'/resources/dagdmarkup.php';
// require_once dirname(dirname(__FILE__)).'/resources/sql.php';

// All of the applications that we route too.
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
  header('HTTP/1.0 404 Not Found');
  echo "404 - route not found.\n";
  if (!$DEBUG) {
    die();
  }
}

debug('Route matches', print_r($route_matches, true));
debug('Controller', $controller_match);
debug('Pass-off', 'Passing off to controller.');

$instance = new ReflectionClass($controller_match);
$instance = $instance->newInstance();
$instance->setRouteMatches($route_matches);
debug('Response from Controller', '');
echo $instance->finalize();

if (!isset($_GET['strip'])) {
  echo "\n";
}
