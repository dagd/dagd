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
    return $this->read_db;
  }

  public function setWriteDB($write_db) {
    $this->write_db = $write_db;
    return $this;
  }

  public function getWriteDB() {
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

  public function getStyle() {
    // The actual cookie is set in the legacy DaGdBaseClass for now, but we
    // have to handle it here if set, so we can show apps in dark mode.
    $darkmode = '';
    if ($this->getRequest()->getSession()->get('darkmode') === 'true') {
      statsd_bump('dagd_dark_mode_active');
    }

    return array(
      $darkmode,
    );
  }

  public function getDarkmode() {
    $session = $this->getRequest()->getSession();
    $change_dm = $this->getRequest()->param('darkmode', null, true, true);
    // If $change_dm is not null, then the user wants to set darkmode value.
    if ($change_dm !== null) {
      $session->set('darkmode', $change_dm ? 'true' : 'false');
    }

    return $session->get('darkmode', 'false') === 'true';
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
    $cs = new Cowsay();
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

  public function getBaseTemplate() {
    $help = $this->getHelp();
    if ($help instanceof DaGdHelp) {
      $help = $help->toOldHelp();
    }

    return id(new DaGdChromedAppTemplate())
      ->setStyle($this->getStyle())
      ->setTitle(idx($help, 'title', 'Welcome!'))
      ->setDarkmode($this->getDarkmode());
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
