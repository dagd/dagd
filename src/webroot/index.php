<?php

$DEBUG=false;

// Resources that help us do cool things.
require_once dirname(dirname(__FILE__)).'/resources/global_resources.php';
require_once dirname(dirname(__FILE__)).'/route.php';
require_once dirname(dirname(__FILE__)).'/resources/index_resources.php';

// All of the applications that we route too.
require_application('ip');
require_application('useragent');
require_application('comingsoon');

if (!$_GET['__path__']) {
  throw new Exception(
    'No __path__ GET variable was found.'.
    'Your rewrite rules are incorrect!');
}

$requested_path = $_GET['__path__'];

$route_matches = null;
$controller_match = null;
foreach ($routes as $route => $controller) {
  if(preg_match('#^'.$route.'#', $requested_path, $route_matches)) {
    $controller_match = $controller;
    break;
  }
}

if (!$route_matches) {
  header('HTTP/1.0 404 Not Found');
  echo '404 - route not found.';
  die();
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