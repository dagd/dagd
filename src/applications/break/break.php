<?php

final class DaGdBreakController extends DaGdBaseClass {
  public function render() {
    $environment = DaGdConfig::get('general.environment');
    if ($environment != 'development') {
      error400('This page is disabled in the production environment.');
      return false;
    } else {
      throw new Exception('This is a test exception.');
    }
  }
}