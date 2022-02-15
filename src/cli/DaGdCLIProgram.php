<?php

require_once(dirname(dirname(__FILE__)).'/resources/global_resources.php');
set_exception_handler(null);
DaGdConfig::$config['general.autoload_search'][] = 'cli/error/';

abstract class DaGdCLIProgram extends DaGdCLI {
  private $name;
  private $description;
  private $parameters = array();

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

  public function getFreeformParameters() {
    $arguments = array();
    foreach ($this->parameters as $param) {
      if ($param->getKind() == 'freeform_parameter') {
        $arguments[] = $param;
      }
    }
    return $arguments;
  }

  /**
   * Get a parameter by its long or short name.
   */
  public function param($key) {
    foreach ($this->parameters as $name => $param) {
      if ($param->getName() == $key || $param->getShortname() == $key) {
        return $param;
      }
    }
    return null;
  }

  /**
   * Find the first non-given freeform parameter if it exists.
   */
  public function next_freeform_param() {
    foreach ($this->parameters as $name => $param) {
      if ($param->getKind() == 'freeform_parameter' && !$param->getGiven()) {
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
        $param->setGiven(true);
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
        if ($param->getKind() == 'argument') {
          // If we found a = above, we already know the value, use it.
          // Otherwise, the value is the next thing in $argv, so we have to set
          // $state accordingly and move on to it so we can pull it out.
          if ($value) {
            $param->setValue($value);
            $param->setGiven(true);
          } else {
            $state = 'value';
          }
        } else {
          // Handle this case separately -- we don't want to set 'given' for
          // arguments unless their value is known to exist which might be
          // unknown until the next iteration. But if we aren't an argument
          // (e.g. we're a flag or something else), then we can set it right
          // away.
          $param->setGiven(true);
        }
      } else {
        // See if someone passed a bunch of flags
        $has_valid_flags = false;
        if ($arg[0] == '-') {
          if ($arg[1] != '-') {
            $flags = str_split($arg);
            array_shift($flags);
            foreach ($flags as $flag) {
              $param = $this->param('-'.$flag);
              if ($param) {
                $this->parameters[$param->getName()]->setGiven(true);
                $has_valid_flags = true;
              } else {
                throw new DaGdInvalidParameterCLIException('-'.$flag);
              }
            }
          }
          if (!$has_valid_flags) {
            throw new DaGdInvalidParameterCLIException($arg);
          }
        } else {
          // We got a parameter that didn't map to a param by name and didn't
          // start with a leading "-". So it must be freeform. Find the next
          // freeform parameter if it exists, otherwise throw.
          $param = $this->next_freeform_param();
          if (!$param) {
            throw new DaGdInvalidParameterCLIException(
              'freeform parameter "'.$arg.'"');
          }
          $this->parameters[$param->getName()]->setGiven(true);
          $this->parameters[$param->getName()]->setValue($arg);
        }
      }
    }
  }

  public function debugArgs() {
    var_dump($this->parameters);
  }

  private function showParameterUsage($param) {
    if ($param->getGiven()) {
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
      echo "\n";
    }

    $args = $this->getFreeformParameters();
    if (count($args) > 0) {
      echo 'Freeform Parameters:';
      echo "\n\n";
      foreach ($args as $arg) {
        $this->showParameterUsage($arg);
      }
    }
  }

  public function run() {
    // Things to handle before checking for require parameters.
    // In particular, we want to allow --help always
    if ($this->param('--help')->getGiven()) {
      $this->showUsage();
      exit(0);
    }

    $debug_args = $this->param('--debug-args');
    if ($debug_args && $debug_args->getGiven()) {
      $this->debugArgs();
      exit(0);
    }

    foreach ($this->parameters as $k => $v) {
      if ($v->getRequired() && !$v->getGiven()) {
        throw new DaGdRequiredParameterNotPassedCLIException($k);
      }
    }
  }

  /**
   * A function that gets called even if an exception is thrown, allowing
   * programs to clean up anything they need to before exiting.
   */
  public function cleanup() {
    return null;
  }

  /**
   * A wrapper around run() which handles exceptions and cleanup.
   */
  public function execute(array $args) {
    try {
      $this->parseArgs($args);
      $this->run();
      $this->cleanup();
      exit(0);
    } catch (DaGdCLIException $ex) {
      $ex->setProgram($this);
      echo $ex->toCli();
      $this->cleanup();
      exit(1);
    } catch (Exception $ex) {
      // It's an exception we didn't throw ourselves, so don't bother
      // pretty-printing it, just clean up and rethrow it.
      $this->cleanup();
      throw $ex;
    }
  }
}
