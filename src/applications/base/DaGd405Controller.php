<?php

final class DaGd405Controller extends DaGdErrorController {
  public function execute(DaGdResponse $response) {
    statsd_bump('status,code=405');
    $response->setCode(405);
    return $this->getMessage('405 - method not allowed');
  }
}
