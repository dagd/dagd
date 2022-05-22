<?php

$start = microtime(true);

// Resources that help us do cool things.
require_once dirname(dirname(__FILE__)).'/resources/global_resources.php';

statsd_bump('framework_entry');

// This mechanism is now deprecated in favor of autoloading.
// This is kept around for now, for deployments which might have custom apps
// and load them with general.applications. New applications should use
// autoloading instead.
require_once dirname(__FILE__).'/../applications/base/DaGdBaseClass.php';
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
      header('Location: '.$new_location);
      return;
    } else {
      // We aren't just redirecting to a URL, we're passing to a controller.

      // First see if the route has request methods assigned for it.
      // If not, give it the default set of them.
      if (!array_key_exists('methods', $metadata)) {
        $default_methods = DaGdConfig::get('general.default_methods');
        $metadata['methods'] = $default_methods;
      }

      // Now, look at the methods a route has defined for it.
      // If the client's $request_method is defined in it, we're done. Use that
      // controller.
      $methods = $metadata['methods'];
      if ($controller = idx($methods, $request_method)) {
        $metadata['controller'] = $controller;
        $metadata_match = $metadata;
        $regex_match_wrong_method = false;
        break;
      } else {
        // If we don't have an inner-mapping to a controller by request method,
        // then let's see if if the client's $request_method is in a non-assoc.
        // methods list.
        if (in_array($request_method, $metadata['methods'])) {
          // We have a match. We can safely assume that the 'controller' field
          // accepts requests of this method.
          $regex_match_wrong_method = false;
          $metadata_match = $metadata;
          break;
        } else {
          // Otherwise it's not in the assoc. map (if it exists), it's not in
          // a non-assoc. map. But we do have a route match.
          $regex_match_wrong_method = true;
          break;
        }
      }
    }
  }
}

$debug = DaGdConfig::get('general.debug');

$controller = null;
$error_controllers = DaGdConfig::get('general.error_controllers');

if (!$route_matches) {
  $controller = $error_controllers['404'];
} else if ($regex_match_wrong_method) {
  $controller = $error_controllers['405'];
} else {
  $controller = $metadata_match['controller'];
}

$instance = new ReflectionClass($controller);
$instance = $instance->newInstance();
$response = '';

// Thread this through even for old controllers, so they can beta some of the
// newer, fun stuff like cows.
$request = id(new DaGdRequest())
  ->setCookies($_COOKIE)
  ->setRequest($_REQUEST)
  ->setServer($_SERVER)
  ->setRouteMatches($route_matches);

$write_dbh = null;

if ($debug) {
  $write_dbh = new DaGdMySQLiDebug(
    DaGdConfig::get('mysql.host'),
    DaGdConfig::get('mysql.user'),
    DaGdConfig::get('mysql.password'),
    DaGdConfig::get('mysql.database'));
} else {
  $write_dbh = new mysqli(
    DaGdConfig::get('mysql.host'),
    DaGdConfig::get('mysql.user'),
    DaGdConfig::get('mysql.password'),
    DaGdConfig::get('mysql.database'));
}

$read_dbh = $write_dbh;
$readonly_host = DaGdConfig::get('readonly_mysql.host');

if (!empty($readonly_host)) {
  $read_dbh = new mysqli(
    DaGdConfig::get('readonly_mysql.host'),
    DaGdConfig::get('readonly_mysql.user'),
    DaGdConfig::get('readonly_mysql.password'),
    DaGdConfig::get('readonly_mysql.database'));
}

// Temporary conditional, handle migration to DaGdController
if ($instance instanceof DaGdController) {
  $instance->setRequest($request);

  $cache_backend = DaGdConfig::get('cache.backend');
  if ($cache_backend) {
    $backend = id(new ReflectionClass($cache_backend))->newInstance();
    $instance->setCache($backend);
  }
} else {
  // This has moved to DaGdRequest in the new model
  $instance->setRouteMatches($route_matches);
  $instance->setRequest($request);
}

// New and old controllers provide this same interface
$response = $instance
  ->setWriteDB($write_dbh)
  ->setReadDB($read_dbh)
  ->finalize();

$git_dir = escapeshellarg(dirname($_SERVER['SCRIPT_FILENAME']).'/../../.git/');
$git_latest_commit = shell_exec(
  'git --git-dir='.$git_dir.' log -1 --pretty=format:%h');

// Temporary, handle migration to DaGdResponse
if ($response instanceof DaGdResponse) {
  $response->addHeader('X-Git-Commit', $git_latest_commit);
  $response->render();
  $response->postRender();
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
