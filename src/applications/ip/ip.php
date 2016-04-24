<?php
final class DaGdIPController extends DaGdBaseClass {
  public static $__help__ = array(
    'title' => 'ip',
    'summary' => 'Returns your current IP address.',
    'path' => 'ip',
    'examples' => array(
      array(
        'arguments' => null,
        'summary' => null),
    ));

  protected $wrap_html = true;

  public function render() {
    return client_ip();
  }
}
