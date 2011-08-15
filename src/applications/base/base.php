<?php
abstract class DaGdBaseClass {

  // Automatically escape stuff to prevent against xss.
  protected $escape = true;

  // Wrap the response in <pre>...</pre> in non-cli browsers.
  protected $wrap_pre = true;

  // This is "global" for now, and probably needs to be refactored.
  protected $db_connection;

  // This contains matches that the router finds in the accessed URL.
  protected $route_matches = null;
  
  public function __construct() {
    global $__db_handler;
    $this->db_connection = $__db_handler;
  }

  public function setRouteMatches($matches=null) {
    $this->route_matches = $matches;
  }

  public function render() {
    return 'Override this method to make stuff happen!';
  }

  /*
   * A function that, when overridden, returns a certain version of a response
   * to a CLI browser. By default handle things normally.
   */
  public function renderCLI() {
    return strip_tags($this->render());
  }

  public function finalize() {
    $response = null;

    if (is_text_useragent()) {
      header('Content-type: text/plain');
      $response = $this->renderCLI();
    } else {
      $response = $this->render();
    }

    if ($this->escape) {
      $response = htmlspecialchars($response);
    }

    if (!is_text_useragent() && $this->wrap_pre) {
      $response = '<pre>'.$response.'</pre>';
    }
    
    return $response;
  }
}
