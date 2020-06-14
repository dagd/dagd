<?php
final class DaGdHelpController extends DaGdBaseClass {
  public function getHelp() {
    return array(
      'title' => 'help',
      'summary' => 'Provides a list of valid commands based on the route map.',
      'path' => 'help',
      'examples' => array(
        array(
          'arguments' => null,
          'summary' => 'Provide an overall list of commands'),
        ));
  }

  public function configure() {
    $bgcolor = '#ddd';

    if ($this->getDarkmode()) {
      $bgcolor = '#455';
    }

    return $this
      ->setWrapHtml(true)
      ->setWrapPre(false)
      ->setEscape(false)
      ->setStyle(':target { background-color: '.$bgcolor.'; }');
  }

  public function render() {
    $routes = DaGdConfig::get('general.routemap');
    $return = '';
    $controllers_visited = array();

    foreach ($routes as $path => $metadata) {
      // First handle the more complex case of routing based on method.
      // Help will render these controllers as distinct apps.
      $methods = idx($metadata, 'methods');
      if ($methods) {
        foreach ($methods as $method_or_index => $controller_or_method) {
          if (is_string($method_or_index)) {
            // We can assume the value is a controller if we're in an
            // assoc array.
            if (in_array($controller_or_method, $controllers_visited)) {
              continue;
            }

            $return .= help($controller_or_method);
            $controllers_visited[] = $controllers_visited;
          }
        }
      }

      // Now handle the simpler case of having a 'controller' key.
      $controller = idx($metadata, 'controller');
      if ($controller) {
        if (in_array($controller, $controllers_visited)) {
          continue;
        }
        $return .= help($controller);
        $controllers_visited[] = $controller;
      }
    }
    return $return;
  }
}
