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

  public function renderCow() {
    $cs = new Cowsay();
    $cs->setMessage($this->renderText());
    return $cs->render();
  }

  public function renderText() {
    return strip_tags($this->render()->renderSafe());
  }

  public function render() {
    throw new Exception('Unimplemented controller');
  }

  public function getBaseTemplate() {
    return id(new DaGdBaseTemplate())
      ->setTitle(idx($this->getHelp(), 'title', 'Welcome!'))
      ->setStyle($this->getStyle());
  }

  public function renderWithTemplate() {
    return $this
      ->getBaseTemplate()
      ->setBody($this->render())
      //->setStyle($this->getStyle())
      ->setTitle(idx($this->getHelp(), 'title', 'Welcome!'))
      ->getHtml();
  }

  // TODO: Probably add some instanceof checks here
  protected function chooseRenderer() {
    $request = $this->getRequest();

    if ($request->wantsCow()) {
      return id(new DaGdTextResponse())
        ->setBody($this->renderCow())
        ->setTrailingNewline(true);
    }

    if ($request->wantsText()) {
      $wants_newline = !$this
        ->getRequest()
        ->getParamOrDefault('strip', false, true, true);

      return id(new DaGdTextResponse())
        ->setBody($this->renderText())
        ->setTrailingNewline($wants_newline);
    }

    if ($request->wantsJson()) {
      return null; // TODO
    }

    return id(new DaGdHTMLResponse())
      ->setBody($this->renderWithTemplate());
  }

  public function finalize() {
    return $this->chooseRenderer();
  }
}
