<?php

/**
 * This is the base of a DaGdCLIProgram, and exists primarily to provide helper
 * methods to DaGdCLIProgram. It's abstract and in theory could be extended by
 * programs wishing to implement their own argument parsing and such, but that
 * seems unlikely at best.
 */
abstract class DaGdCLI {
  public function black($str) {
    return "\033[30m".$str."\033[0m";
  }

  public function red($str) {
    return "\033[31m".$str."\033[0m";
  }

  public function green($str) {
    return "\033[32m".$str."\033[0m";
  }

  public function yellow($str) {
    return "\033[33m".$str."\033[0m";
  }

  public function blue($str) {
    return "\033[34m".$str."\033[0m";
  }

  public function magenta($str) {
    return "\033[35m".$str."\033[0m";
  }

  public function cyan($str) {
    return "\033[36m".$str."\033[0m";
  }

  public function white($str) {
    return "\033[37m".$str."\033[0m";
  }

  public function reset() {
    return "\033[0m";
  }

  public function bold($str) {
    return "\033[1m".$str."\033[0m";
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
