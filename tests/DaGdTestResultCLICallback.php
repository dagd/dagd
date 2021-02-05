<?php

final class DaGdTestResultCLICallback extends DaGdTestResultCallback {
  private $cli;

  public function __construct($cli) {
    $this->cli = $cli;
  }

  public function fail($text, $output = '') {
    if ($this->getTest()->getTolerateFailure()) {
      echo $this->cli->yellow(
        $this->cli->bold('*** (tolerable failure) '.$text.' ***'));
      echo "\n";
    } else {
      echo $this->cli->red(
        $this->cli->bold('*** '.$text.' ***'));
      echo "\n";
    }
    if ($output != '') {
      echo '    Output was: ';
      echo $this->cli->magenta($output);
      echo "\n";
    }
    if ($this->getTest()->getBody() &&
        preg_match('@500@', $this->getTest()->getResponse())) {
      echo '    Framework output: ';
      $lines = explode("\n", $this->getTest()->getBody());
      foreach ($lines as $line) {
        echo '      '.$this->cli->blue($line)."\n";
      }
    }
  }

  public function pass($text) {
    echo $this->cli->green(
      $this->cli->bold('*** '.$text.' ***'));
    echo "\n";
  }
}
