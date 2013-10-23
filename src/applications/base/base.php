<?php
abstract class DaGdBaseClass {

  // Automatically escape stuff to prevent against xss.
  protected $escape = true;

  // Wrap the response in <pre>...</pre> in non-cli browsers.
  protected $wrap_pre = true;

  // Enable text-UA specific html stripping?
  protected $text_html_strip = true;

  // This is "global" for now, and probably needs to be refactored.
  protected $db_connection;

  // This contains matches that the router finds in the accessed URL.
  protected $route_matches = null;

  // Set text/plain by default for all text-useragent responses.
  protected $text_content_type = true;

  // Acceptable request types to listen for in this controller.
  protected $request_methods = array('GET');

  // This is used for DaGdHelpController to generate its list of commands.
  public static $__help__ = null;

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
    if (!in_array($_SERVER['REQUEST_METHOD'], $this->request_methods)) {
      error405();
      return;
    }

    $response = null;

    if ($this->text_html_strip && is_text_useragent()) {
      if ($this->text_content_type) {
        header('Content-type: text/plain; charset=utf-8');
        header('X-Content-Type-Options: nosniff');
      }
      $response = $this->renderCLI();
    } else {
      $response = $this->render();
      if ($this->escape) {
        $response = htmlspecialchars($response);
      }
    }

    if (!is_text_useragent() && $this->wrap_pre) {
      $response = '<pre>'.$response.'</pre>';
    }

    return $response;
  }
}
