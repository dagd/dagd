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
      if (in_array($metadata['controller'], $controllers_visited)) {
        continue;
      }
      $return .= help($metadata['controller']);
      $controllers_visited[] = $metadata['controller'];
    }
    return $return;
  }
}
