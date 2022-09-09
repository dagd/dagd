<?php

final class DaGdBreakController extends DaGdController {
  public function execute(DaGdResponse $response) {
    $environment = DaGdConfig::get('general.environment');
    if ($environment != 'development') {
      return $this
        ->error(400, 'This page is disabled in the production environment.')
        ->execute($response);
    } else {
      throw new Exception('This is a test exception.');
    }
  }
}
