<?php

// Resources that help us do cool things.
require_once dirname(dirname(__FILE__)).'/resources/global_resources.php';
require_once dirname(dirname(__FILE__)).'/resources/index_resources.php';
require_once dirname(dirname(__FILE__)).'/resources/dagdmarkup.php';

// All of the applications that we route too.
$applications = DaGdConfig::get('general.applications');
foreach ($applications as $application) {
  require_application($application);
}

$DEBUG = DaGdConfig::get('general.debug');

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

if (!$route_matches) {
  header('HTTP/1.0 404 Not Found');
  echo '404 - route not found.';
  if (!$DEBUG) {
    die();
  }
}

if ($DEBUG) {
  echo '<pre>'.print_r($route_matches, true).'</pre>';
  echo '<br />';
  echo '<pre>CONTROLLER: '.$controller_match.'</pre>';
  echo "\n";
  echo '<br />Passing off to controller.<br />';
}

$instance = new ReflectionClass($controller_match);
$instance = $instance->newInstanceArgs($route_matches);
echo $instance->finalize();
echo "\n"; // Ewwww, hardcode for now.