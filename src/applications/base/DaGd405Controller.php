<?php

final class DaGd405Controller extends DaGdController {
  public function execute(DaGdResponse $response) {
    $response->setCode(405);
    return '405 - method not allowed';
  }
}
