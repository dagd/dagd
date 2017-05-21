<?php
final class DaGdHelpController extends DaGdBaseClass {
  public $__help__ = array(
    'title' => 'help',
    'summary' => 'Provides a list of valid commands based on the route map.',
    'path' => 'help',
    'examples' => array(
      array(
        'arguments' => null,
        'summary' => 'Provide an overall list of commands'),
      ));

  protected $wrap_html = true;
  protected $wrap_pre = false;
  protected $escape = false;

  public function render() {
    $routes = DaGdConfig::get('general.routemap');
    $return = '';
    $controllers_visited = array();

    foreach ($routes as $path => $metadata) {
      if (in_array($metadata['controller'], $controllers_visited)) {
        continue;
      }
      $return .= help($metadata['controller']);
      $controllers_visited[] = $metadata['controller'];
    }
    return $return;
  }
}
