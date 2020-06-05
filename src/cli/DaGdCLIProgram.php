<?php

require_once(dirname(dirname(__FILE__)).'/resources/global_resources.php');
set_exception_handler(null);

abstract class DaGdCLIProgram {
  private $name;
  private $description;
  private $parameters = array();
  private $given_parameters = array();

  public function setName($name) {
    $this->name = $name;
    return $this;
  }

  public function getName() {
    return $this->name;
  }

  public function setDescription($description) {
    $this->description = $description;
    return $this;
  }

  public function getDescription() {
    return $this->description;
  }

  public function addParameter(DaGdCLIParameter $param) {
    $this->parameters[$param->getName()] = $param;
    return $this;
  }

  public function getFlags() {
    $flags = array();
    foreach ($this->parameters as $param) {
      if ($param->getKind() == 'flag') {
        $flags[] = $param;
      }
    }
    return $flags;
  }

  public function getArguments() {
    $arguments = array();
    foreach ($this->parameters as $param) {
      if ($param->getKind() == 'argument') {
        $arguments[] = $param;
      }
    }
    return $arguments;
  }

  /**
   * Get an expected parameter by its long or short name.
   */
  public function param($key) {
    foreach ($this->parameters as $name => $param) {
      if ($param->getName() == $key || $param->getShortname() == $key) {
        return $param;
      }
    }
    return null;
  }

  public function parseArgs(array $argv, $drop_first = true) {
    $state = 'key';
    $param = null;

    // Remove program name
    if ($drop_first) {
      array_shift($argv);
    }

    foreach ($argv as $i => $arg) {
      if ($state == 'value') {
        // We got an argument, which needs a value (otherwise it would be a
        // flag).
        $param->setValue($arg);
        $state = 'key';
        continue;
      }

      // Check for --foo=bar style arguments
      $value = null;
      if (strpos($arg, '=') !== false) {
        $parts = explode('=', $arg, 2);
        $arg = $parts[0];
        $value = $parts[1];
      }

      $param = $this->param($arg);

      if ($param) {
        $this->given_parameters[$param->getName()] = $param;

        if ($param->getKind() == 'argument') {
          // If we found a = above, we already know the value, use it.
          // Otherwise, the value is the next thing in $argv, so we have to set
          // $state accordingly and move on to it so we can pull it out.
          if ($value) {
            $param->setValue($value);
          } else {
            $state = 'value';
          }
        }
      } else {
        // See if someone passed a bunch of flags
        $has_valid_flags = false;
        if ($arg[0] == '-' && $arg[1] != '-') {
          $flags = str_split($arg);
          array_shift($flags);
          foreach ($flags as $flag) {
            $param = $this->param('-'.$flag);
            if ($param) {
              $this->given_parameters[$param->getName()] = $param;
              $has_valid_flags = true;
            } else {
              throw new Exception('Unknown flag: -'.$flag);
            }
          }
        }
        if (!$has_valid_flags) {
          throw new Exception('Unknown parameter: '.$arg);
        }
      }
    }

    foreach ($this->parameters as $k => $v) {
      if ($v->getRequired() && !idx($this->given_parameters, $k)) {
        throw new Exception('Required parameter '.$k.' not passed');
      }
    }
  }

  private function bold($str) {
    return "\033[1m".$str."\033[0m";
  }

  public function debugArgs() {
    var_dump($this->given_parameters);
  }

  private function showParameterUsage($param) {
    if (idx($this->given_parameters, $param->getName())) {
      echo $this->bold($param->getName());
    } else {
      echo $param->getName();
    }
    echo "\t";
    echo $param->getShortName();
    echo "\t";
    echo $param->getDescription();
    if ($param->getRequired()) {
      echo ' (required)';
    }
    echo "\n";
  }

  public function showUsage() {
    $title = $this->getName().' - '.$this->getDescription();
    echo "\n";
    echo $title;
    echo "\n";
    echo str_repeat('-', strlen($title));
    echo "\n\n";

    $flags = $this->getFlags();
    if (count($flags) > 0) {
      echo 'Flags:';
      echo "\n\n";
      foreach ($flags as $flag) {
        $this->showParameterUsage($flag);
      }
      echo "\n";
    }

    $args = $this->getArguments();
    if (count($args) > 0) {
      echo 'Arguments:';
      echo "\n\n";
      foreach ($args as $arg) {
        $this->showParameterUsage($arg);
      }
    }
  }

  public function run() {
    if (idx($this->given_parameters, '--help')) {
      $this->showUsage();
      exit(0);
    }

    if (idx($this->given_parameters, '--debug-args')) {
      $this->debugArgs();
      exit(0);
    }
  }
}
