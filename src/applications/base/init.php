<?php
abstract class DaGdBaseClass {

  // Automatically escape stuff to prevent against xss.
  protected $escape = true;

  /*
   * Adapt to CLI/non-CLI browsers. For exapmle, in non-cli browsers, wrap the
   * response in a <pre> in finalize().
   */
    protected $auto_adapt = true;
  
  public function __construct() {}

  public function render() {
    return 'Override this method to make stuff happen!';
  }

  public function finalize() {
    $response = $this->render();
    if ($this->escape) {
      $response = htmlspecialchars($response);
    }
    if ($this->auto_adapt) {
      if (!is_text_useragent()) {
        $response = '<pre>'.$response.'</pre>';
      }
    }
    return $response;
  }
}