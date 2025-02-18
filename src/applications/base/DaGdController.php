<?php

/**
 * This class replaces (the badly named) DaGdBaseClass and is the parent class
 * for all controllers in da.gd applications.
 *
 */
abstract class DaGdController {
  private $request;
  private $read_db;
  private $write_db;
  private $cache;
  private $debug_cards = array();

  public function setRequest($request) {
    $this->request = $request;
    return $this;
  }

  public function getRequest() {
    return $this->request;
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

  public function setCache($cache) {
    $this->cache = $cache;
    return $this;
  }

  public function getCache() {
    return $this->cache;
  }

  // Another getter for the cache, just with a nicer name.
  public function cache() {
    return $this->cache;
  }

  public function setDebugCards($debug_cards) {
    $this->debug_cards = $debug_cards;
    return $this;
  }

  public function addDebugCard($debug_card) {
    $this->debug_cards[] = $debug_card;
    return $this;
  }

  public function getDebugCards() {
    return $this->debug_cards;
  }

  public function setAlerts($alerts) {
    $session = $this->getRequest()->getSession();
    $session->set('dagd-alerts', $alerts);
    return $this;
  }

  public function addAlert($alert) {
    $session = $this->getRequest()->getSession();
    $alerts = $session->get('dagd-alerts', array());
    $alerts[] = $alert;
    $session->set('dagd-alerts', $alerts);
    return $this;
  }

  public function getAlerts($clear_alerts = false) {
    $session = $this->getRequest()->getSession();
    $alerts = $session->get('dagd-alerts', array());
    $session->set('dagd-alerts', array());
    return $alerts;
  }

  public function getStyle() {
    return array();
  }

  public function getDarkmode() {
    $session = $this->getRequest()->getSession();
    $change_dm = $this->getRequest()->param('darkmode', null, true, true);
    // If $change_dm is not null, then the user wants to set darkmode value.
    if ($change_dm !== null) {
      $session->set('darkmode', $change_dm ? 'true' : 'false');
    }

    $wants_darkmode = $session->get('darkmode', 'false');
    if ($wants_darkmode === 'true') {
      statsd_bump('dagd_dark_mode_active');
      return true;
    }
    return false;
  }

  public static function getHelp() {
    return array();
  }

  /**
   * Return an instance of an error-code controller, which will automatically
   * set some things like status code/message for you and return a pre-composed
   * response for you, for various mime-types. If you need anything more
   * complex, you can either write your own error controllers, or just
   * $response->setCode() in your own handlers and render what you like. This
   * method is only here for convenience in the simple cases.
   *
   * If you use this, you MUST call the same method you are overriding on
   * the controller that gets returned. For example if you use this from an
   * execute() method, you must call `$this->error(404)->execute($response);`.
   * You MAY return the result, or do other processing and use the call only for
   * its side effects.
   */
  public function error($code, $message = null) {
    $controller = null;
    switch ($code) {
    case 400:
      return id(new DaGd400Controller())
        ->setRequest($this->getRequest())
        ->setMessage($message);
    case 401:
      return id(new DaGd401Controller())
        ->setRequest($this->getRequest())
        ->setMessage($message);
    case 403:
      return id(new DaGd403Controller())
        ->setRequest($this->getRequest())
        ->setMessage($message);
    case 404:
      return id(new DaGd404Controller())
        ->setRequest($this->getRequest())
        ->setMessage($message);
    case 405:
      return id(new DaGd405Controller())
        ->setRequest($this->getRequest())
        ->setMessage($message);
    }
  }

  /**
   * A string version of the response.
   *
   * This is used for simple controllers where the main difference between
   * response types is just the boilerplate around their bodies.
   *
   * This allows such simple controllers to implement one method and have
   * everything else "magically" work, while more complex controllers can
   * implement render(), renderText(), renderJson(), etc. (but must either
   * implement all of them or implement execute() so that they can fall back to
   * it.)
   */
  public function execute(DaGdResponse $response) {
    throw new Exception('execute() not rendered; unimplemented controller');
  }

  public function renderCow($response) {
    $cs = new DaGdCowsay();
    if ($cow = $this->getRequest()->getParamOrDefault('cow')) {
      $cs->setCow($cow);
    }
    $cs->setMessage($this->execute($response));
    return $response
      ->setBody($cs->render())
      ->setTrailingNewline(true);
  }

  public function renderText(DaGdTextResponse $response) {
    $body = $this->execute($response);
    return $response->setBody($body);
  }

  public function render(DaGdHTMLResponse $response) {
    // By default we wrap things in <pre> tags and wrap a template around it.
    $body = tag('pre', $this->execute($response));
    $template = $this->getBaseTemplate()->setBody($body)->getHtmlTag();
    return $response->setBody($template);
  }

  private function getDebugBody() {
    $query_cards = array();

    if ($this->getWriteDB() instanceof DaGdMySQLiDebug) {
      $queries = $this->getWriteDB()->getQueries();
      foreach ($queries as $idx => $query) {
        $human_idx = $idx + 1;
        $title = tag(
          'small',
          'Query '.$human_idx.' ('.$query->getMilliseconds().' ms)');

        $query_cards[] = id(new DaGdCard())
          ->setTitle($title)
          ->setBody(
            tag(
              'pre',
              $query->getQuery(),
              array(
                'style' => 'white-space: pre-wrap;',
              )
            ));
      }
    }

    $request_card = id(new DaGdCard())
      ->setTitle(tag('small', 'Request Details'))
      ->setBody(
        tag(
          'pre',
          print_r($this->getRequest(), true),
          array(
            'style' => 'white-space: pre-wrap;',
          )
        ));

    $debug_body = id(new DaGdCard())
      ->setTitle(
        tag(
          'div',
          'dagd debugger',
          array(
            'style' => 'text-align: center;',
          )))
      ->setBody(
        array(
          $query_cards,
          $request_card,
          $this->getDebugCards(),
        ))
      ->addClass('ow-bw')
      ->addClass('flex-1');

    return $debug_body;
  }


  public function getBaseTemplate() {
    $help = $this->getHelp();
    if ($help instanceof DaGdHelp) {
      $help = $help->toOldHelp();
    }

    $debug = DaGdConfig::get('general.debug');
    $debug_body = null;
    if ($debug) {
      $debug_body = $this->getDebugBody();
    }

    return id(new DaGdChromedAppTemplate())
      ->setStyle($this->getStyle())
      ->setTitle(idx($help, 'title', 'Welcome!'))
      ->setDebugBody($debug_body)
      ->setDarkmode($this->getDarkmode())
      ->setAlerts($this->getAlerts(true));
  }

  // TODO: Probably add some instanceof checks here
  protected function chooseRenderer() {
    $request = $this->getRequest();

    if ($request->wantsCow()) {
      return $this->renderCow(new DaGdTextResponse());
    }

    if ($request->wantsText()) {
      $wants_newline = !$this
        ->getRequest()
        ->getParamOrDefault('strip', false, true, true);

      return $this
        ->renderText(new DaGdTextResponse())
        ->setTrailingNewline($wants_newline);
    }

    if ($request->wantsJson()) {
      return null; // TODO
    }

    return $this->render(new DaGdHTMLResponse());
  }

  /**
   * This function by default will call chooseRenderer() to figure out, based
   * on the request, what kind of DaGdResponse to give.
   *
   * Controllers can override this to have full control over the response (for
   * example, to ALWAYS render an image). Controllers which override it must
   * always return a subclass of DaGdResponse which has had setRequest() called
   * on it (usually with $this->getRequest() as its argument). This ensures the
   * session system still works as expected.
   */
  public function finalize() {
    return $this
      ->chooseRenderer()
      ->setRequest($this->getRequest());
  }
}
