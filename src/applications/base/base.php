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

  // Wrap the HTML boilerplate around this response?
  // Default to false because this is a new thing and might break some
  // controllers.
  protected $wrap_html = false;

  // Custom stylization for apps.
  protected $style = '';

  // This is used for DaGdHelpController to generate its list of commands.
  public $__help__ = null;

  // If true, make ?strip ineffective -- never allow a newline to terminate
  // output.
  protected $never_newline = false;

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

    if ($this->text_html_strip && !is_html_useragent()) {
      if ($this->text_content_type) {
        header('Content-type: text/plain; charset=utf-8');
        header('X-Content-Type-Options: nosniff');
      }
      $response = $this->renderCLI();
    } else {
      $response = '';
      if ($this->wrap_html) {
        $title = idx($this->__help__, 'title', 'Welcome!');
        $response .= "<!doctype html>\n";
        $response .= '<html>';
        $response .= '  <head>';
        $response .= '    <meta charset="utf-8">';
        $response .= '    <meta name="keywords" content="dagd,da.gd,url,'.
                     'shorten,shortening,open,source,foss,github">';
        $response .= '    <meta name="description" content="The da.gd URL '.
                     'shortening service">';
        $response .= '    <title>da.gd: '.$title.'</title>';
        $response .= '    <style>';
        $response .= '      *:not(pre):not(code) { font-family: sans-serif; }';
        $response .= $this->style;
        $response .= '    </style>';
        $response .= '  </head>';
        $response .= '  <body>';
      }

      $controller_response = $this->render();

      if ($this->escape) {
        $controller_response = htmlspecialchars(
          $controller_response,
          ENT_HTML5,
          'UTF-8');
      }

      if ($this->wrap_pre) {
        $controller_response = '<pre>'.$controller_response.'</pre>';
      }

      $response .= $controller_response;

      if ($this->wrap_html) {
        $response .= '  </body>';
        $response .= '</html>';
      }
    }

    if (!$this->never_newline && !request_or_default('strip', false, true, true)) {
        $response .= "\n";
    }

    return $response;
  }
}
