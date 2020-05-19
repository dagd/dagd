<?php

final class DaGd404Controller extends DaGdController {
  public function execute(DaGdResponse $response) {
    $response->setCode(404);
    return '404 - route not found';
  }
}
