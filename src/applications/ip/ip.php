<?php
final class DaGdIPController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Returns your current IP address.',
    'path' => 'ip',
    'examples' => array(
      array(
        'arguments' => null,
        'summary' => null),
    ));

  public function render() {
    return client_ip();
  }
}
