<?php

final class DaGdRequiredParameterNotPassedCLIException
  extends DaGdCLIException {

  private $parameter;

  public function __construct($parameter) {
    parent::__construct();
    $this->parameter = $parameter;
  }

  public function getCliMessage() {
    return 'Required parameter or flag not passed: '.
      $this->getProgram()->blue($this->parameter);
  }
}
