<?php
class DaGdHelpController extends DaGdBaseClass {
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

    // TODO:  Get $_REQUEST['url_prefix'] and ['url_suffix'] here.
    $prefix = '/';
    
    foreach ($routes as $path => $controller) {
      $vars = get_class_vars($controller);
      if ($help = $vars['__help__']) {
        $return .= '<h3>'.$help['summary']."</h3>\n";
        $return .= '<ul>';
        foreach ($help['examples'] as $example) {
          $return .= '<li>    '.$prefix;
          $arguments = $example['arguments'];
          if ($arguments) {
            $return .= implode($prefix, $arguments);
          }
          if (array_key_exists('request', $example)) {
            $iteration = 0;
            foreach ($example['request'] as $param => $param_example) {
              if ($iteration === 0) {
                $return .= '?';
              } else {
                $return .= '&';
              }
              $return .= $param.'='.$param_example;
            }
          }
          
          if ($example['summary']) {
            $return .= ': '.$example['summary'];
          }
          
          $return .= "</li>\n";
        }
        $return .= "</ul>\n";
      }
    }
    return $return;
  }
}