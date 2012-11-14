<?php
final class DaGdHelpController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Provides a list of valid commands based on the route map.',
    'path' => 'help',
    'examples' => array(
      array(
        'arguments' => null,
        'summary' => 'Provide an overall list of commands'),
      ));

  protected $wrap_pre = false;
  protected $escape = false;

  public function render() {
    $routes = DaGdConfig::get('general.routemap');
    $return = '';
    $controllers_visited = array();

    foreach ($routes as $path => $controller) {
      if (in_array($controller, $controllers_visited)) {
        continue;
      }
      $instance = new ReflectionClass($controller);
      $instance = $instance->newInstance();
      $return .= $instance->help();
      $controllers_visited[] = $controller;
    }
    return $return;
  }
}
