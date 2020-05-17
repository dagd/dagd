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
    if ($this->getRequest()->getCookie('darkmode') === 'true') {
      $darkmode = 'body { ';
      $darkmode .= '  background-color: #333;';
      $darkmode .= '  color: #ddd;';
      $darkmode .= '}';
      $darkmode .= 'a, a:active, a:visited { color: #ccc; }';
    }

    return array(
      '*:not(pre):not(code) { font-family: sans-serif; }',
      $darkmode,
    );
  }

  public function getHelp() {
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
    $template = $this
      ->getBaseTemplate()
      ->setBody($body)
      ->setStyle($this->getStyle())
      ->setTitle(idx($this->getHelp(), 'title', 'Welcome!'))
      ->getHtml();
    return $response->setBody($template);
  }

  public function getBaseTemplate() {
    return new DaGdBaseTemplate();
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
    return $this->chooseRenderer();
  }
}
