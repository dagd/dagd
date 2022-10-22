<?php
abstract class DaGdBaseClass {

  // Automatically escape stuff to prevent against xss.
  protected $escape = true;

  // Wrap the response in <pre>...</pre> in non-cli browsers.
  protected $wrap_pre = true;

  // Enable text-UA specific html stripping?
  protected $text_html_strip = true;

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

  // A database handler we can safely write to. Might be geographically distant
  // and thus slow for common reads.
  protected $write_db;

  // A database handler we can read from. Usually geographically local and
  // therefore faster for reads than the write database handler.
  protected $read_db;

  // A temporary bridge that lets old-style controllers access some new-style
  // goodies.
  protected $request;

  // This is used for DaGdHelpController to generate its list of commands.
  public function getHelp() {
    return array();
  }

  // If true, make ?strip ineffective -- never allow a newline to terminate
  // output.
  protected $never_newline = false;

  // Dark mode!
  protected $darkmode = false;

  public function __construct() {
  }

  public function setEscape($escape) {
    $this->escape = $escape;
    return $this;
  }

  public function getEscape() {
    return $this->escape;
  }

  public function setWrapPre($wrap_pre) {
    $this->wrap_pre = $wrap_pre;
    return $this;
  }

  public function getWrapPre() {
    return $this->wrap_pre;
  }

  public function setTextHtmlStrip($text_html_strip) {
    $this->text_html_strip = $text_html_strip;
    return $this;
  }

  public function getTextHtmlStrip() {
    return $this->text_html_strip;
  }

  public function setRouteMatches($matches=null) {
    $this->route_matches = $matches;
    return $this;
  }

  public function getRouteMatches() {
    return $this->route_matches;
  }

  public function setTextContentType($text_content_type) {
    $this->text_content_type = $text_content_type;
    return $this;
  }

  public function getTextContentType() {
    return $this->text_content_type;
  }

  public function setWrapHtml($wrap_html) {
    $this->wrap_html = $wrap_html;
    return $this;
  }

  public function getWrapHtml() {
    return $this->wrap_html;
  }

  public function setStyle($style) {
    $this->style = $style;
    return $this;
  }

  public function getStyle() {
    return $this->style;
  }

  public function setReadDB($read_db) {
    $this->read_db = $read_db;
    return $this;
  }

  public function getReadDB() {
    if ($this->read_db) {
      return $this->read_db;
    }

    $readonly_host = DaGdConfig::get('readonly_mysql.host');

    if (empty($readonly_host)) {
      return $this->getWriteDB();
    }

    $debug = DaGdConfig::get('general.debug');
    $this->read_db = DaGdStartup::getReadableDbh($debug);
    return $this->read_db;
  }

  public function setWriteDB($write_db) {
    $this->write_db = $write_db;
    return $this;
  }

  public function getWriteDB() {
    if ($this->write_db) {
      return $this->write_db;
    }

    $debug = DaGdConfig::get('general.debug');
    $this->write_db = DaGdStartup::getWritableDbh($debug);
    return $this->write_db;
  }

  public function setNeverNewline($never_newline) {
    $this->never_newline = $never_newline;
    return $this;
  }

  public function getNeverNewline() {
    return $this->never_newline;
  }

  public function setDarkmode($darkmode) {
    $this->darkmode = $darkmode;
    return $this;
  }

  public function getDarkmode() {
    return $this->darkmode;
  }

  public function setRequest($request) {
    $this->request = $request;
    return $this;
  }

  public function getRequest() {
    return $this->request;
  }

  // Controllers can implement this to set various controller configuration
  // parameters by calling the setters above. It gets called in finalize().
  public function configure() {
    return $this;
  }

  private function configureDarkmodeIfNecessary() {
    if (isset($_REQUEST['darkmode'])) {
      $darkmode_req = request_or_default('darkmode', false, true, true);
      $darkmode_bool = 'false';
      if ($darkmode_req) {
        $darkmode_bool = 'true';
      }
      setcookie(
        'darkmode',
        $darkmode_bool,
        time() + (60 * 60 * 24 * 365),
        '/');
      $_COOKIE['darkmode'] = $darkmode_bool;
    }

    $darkmode_cookie = idx($_COOKIE, 'darkmode');
    $this->setDarkmode($darkmode_cookie === 'true');

    $darkmode = '';
    if (idx($_COOKIE, 'darkmode') === 'true') {
      statsd_bump('dagd_dark_mode_active');
      $darkmode = 'body { ';
      $darkmode .= '  background-color: #333;';
      $darkmode .= '  color: #ddd;';
      $darkmode .= '}';
      $darkmode .= 'a, a:active, a:visited { color: #ccc; }';
    }
    return $darkmode;
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

  public function renderCowsay() {
    $cs = new DaGdCowsay();
    $cs->setMessage($this->renderCLI());
    return $cs->render();
  }

  public function finalize() {
    $darkmode_style = $this->configureDarkmodeIfNecessary();
    $this->configure();
    $response = null;

    // ?cow implies text handler
    $cow = request_or_default('cow', false, true, true);

    if ($this->getTextHtmlStrip() && (!is_html_useragent() || $cow)) {
      if ($this->getTextContentType()) {
        header('Content-type: text/plain; charset=utf-8');
        header('X-Content-Type-Options: nosniff');
      }
      if ($cow) {
        // This makes a good testing point for the new DaGdResponse work
        $response = id(new DaGdTextResponse())
          ->setRequest($this->getRequest())
          ->setBody($this->renderCowsay());
      } else {
        $response = $this->renderCLI();
      }
    } else {
      // We need this to be early-ish because it can set properties we use
      // below such as $this->style. However, controllers need to access
      // $this->darkmode, so it has to be after we set that.
      //
      // 2020-05-02: This is less-true now, controllers should only set
      // $this->style from configure(), and $this->getDarkmode() (set early)
      // should be used instead of direct access to $this->darkmode.
      $controller_response = $this->render();

      if (!$this->getWrapHtml()) {
        return $controller_response;
      }

      $style = array(
        $darkmode_style,
        $this->getStyle(),
      );

      if ($this->getWrapPre()) {
        $controller_response = tag(
          'pre',
          $controller_response);
      }

      // To be removed when the old help system goes away
      $help = $this->getHelp();
      if ($help instanceof DaGdHelp) {
        $help = $help->toOldHelp();
      }

      $template = id(new DaGdChromedAppTemplate())
        ->setTitle(idx($help, 'title', 'Welcome!'))
        ->setStyle($style)
        ->setEscape($this->getEscape())
        ->setDarkmode($this->getDarkmode())
        ->setBody($controller_response);

      $response = "<!doctype html>\n";
      $response .= $template->getHtmlTag()->renderSafe();
    }

    if (!$this->getNeverNewline() &&
        !request_or_default('strip', false, true, true)) {
      // Hack for now until everything moves to DaGdResponse
      if ($response instanceof DaGdResponse) {
        $response->setTrailingNewline(true);
      } else {
        $response .= "\n";
      }
    }

    return $response;
  }
}
