<?php

final class DaGdNotTTYCLIException extends DaGdCLIException {
  public function getCliMessage() {
    return 'TTY required and seemingly not present';
  }
}
