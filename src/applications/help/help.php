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
    
    foreach ($routes as $path => $controller) {
      $instance = new ReflectionClass($controller);
      $instance = $instance->newInstance();
      $return .= $instance->help();
    }
    return $return;
  }
}
