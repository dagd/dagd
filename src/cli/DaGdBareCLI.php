<?php

/**
 * This provides a concrete implementation of DaGdCLI that enables the use of
 * some of the infrastructure around DaGdCLI (such as tables) without having the
 * full context of a terminal.
 *
 * That is - this is kind of a misnomer, as this class isn't expected to be
 * used in a CLI context. It makes no assumptions about a CLI (for example, we
 * disable ANSI color support).
 */
final class DaGdBareCLI extends DaGdCLI {
  public function getColorEnabled() {
    return false;
  }
}
