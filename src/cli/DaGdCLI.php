<?php

/**
 * This is the base of a DaGdCLIProgram, and exists primarily to provide helper
 * methods to DaGdCLIProgram. In theory could be extended by programs wishing to
 * implement their own argument parsing and such, but that seems unlikely in the
 * normal use case.
 */
abstract class DaGdCLI {
  public function getColorEnabled() {
    return true;
  }

  protected function wrapANSIColor($str, $color_code) {
    if ($this->getColorEnabled()) {
      return "\033[".$color_code.'m'.$str."\033[0m";
    } else {
      return $str;
    }
  }

  public function black($str) {
    return $this->wrapANSIColor($str, '30');
  }

  public function red($str) {
    return $this->wrapANSIColor($str, '31');
  }

  public function green($str) {
    return $this->wrapANSIColor($str, '32');
  }

  public function yellow($str) {
    return $this->wrapANSIColor($str, '33');
  }

  public function blue($str) {
    return $this->wrapANSIColor($str, '34');
  }

  public function magenta($str) {
    return $this->wrapANSIColor($str, '35');
  }

  public function cyan($str) {
    return $this->wrapANSIColor($str, '36');
  }

  public function white($str) {
    return $this->wrapANSIColor($str, '37');
  }

  public function reset() {
    return "\033[0m";
  }

  public function bold($str) {
    return $this->wrapANSIColor($str, '1');
  }

  public function status($str, $colored, $plain, $colors = true) {
    $out = '[';

    if ($colors) {
      $out .= $colored;
    } else {
      $out .= $plain;
    }

    $out .= '] ';
    $out .= $str;
    $out .= "\n";
    return $out;
  }

  public function error($str, $colors = true) {
    return $this->status($str, $this->red('ERROR'), 'ERROR');
  }

  public function ok($str, $colors = true) {
    return $this->status($str, $this->green('OK'), 'OK');
  }

  public function info($str, $colors = true) {
    return $this->status($str, $this->yellow('INFO'), 'INFO');
  }

  public function important($str, $colors = true) {
    return $this->status($str, $this->cyan('IMPORTANT'), 'IMPORTANT');
  }

  public function isTTY($throw = false) {
    $is_tty = function_exists('posix_isatty') && posix_isatty(STDIN);

    if (!$is_tty && $throw) {
      throw new DaGdNotTTYCLIException();
    }

    return $is_tty;
  }

  public function prompt($prompt) {
    // Ensure TTY, throw if not.
    $this->isTTY(true);

    echo $prompt;
    $response = fgets(STDIN);
    return trim($response);
  }

  /**
   * A wrapper around strlen() which (optionally and by default) strips ANSI
   * codes from the input before checking its length.
   */
  public static function strlen($str, $strip_ansi = true) {
    if ($strip_ansi) {
      $str = preg_replace("/\033\[(?:\d*|\d+;\d+)m/", '', $str);
    }
    return strlen($str);
  }
}
