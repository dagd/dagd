<?php
abstract class DaGdBaseClass {

  // Automatically escape stuff to prevent against xss.
  protected $escape = true;

  // Wrap the response in <pre>...</pre> in non-cli browsers.
  protected $wrap_pre = true;
  
  public function __construct() {}

  public function render() {
    return 'Override this method to make stuff happen!';
  }

  /*
   * A function that, when overridden, returns a certain version of a response
   * to a CLI browser. By default handle things normally.
   */
  public function renderCLI() {
    return $this->render();
  }

  public function finalize() {
    $response = null;

    if (is_text_useragent()) {
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