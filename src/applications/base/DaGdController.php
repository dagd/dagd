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
    $help = $this->getHelp();
    if ($help instanceof DaGdHelp) {
      $help = $help->toOldHelp();
    }
    $body = tag('pre', $this->execute($response));
    $template = $this
      ->getBaseTemplate()
      ->setBody($body)
      ->setStyle($this->getStyle())
      ->setTitle(idx($help, 'title', 'Welcome!'))
      ->setDarkmode($this->getDarkmode())
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function getBaseTemplate() {
    return new DaGdChromedAppTemplate();
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

  public function finalize() {
    return $this
      ->chooseRenderer()
      ->setRequest($this->getRequest());
  }
}
