<?php

/**
 * This class handles setting up dagd to handle a request.
 */
final class DaGdStartup {
  private $config_loaded = false;

  public function __construct() {
    $this->loadAllUtilities();
  }

  public function isConfigLoaded() {
    return $this->config_loaded;
  }

  public function ensureConfigLoaded() {
    if (!$this->isConfigLoaded()) {
      throw new Exception(
        'No configuration file has been loaded. Call DaGdStartup#loadConfig '.
        'first.');
    }
  }

  private function loadAllUtilities() {
    $utility_path = dirname(dirname(__FILE__)).'/utility';
    $utilities = glob($utility_path.'/*.php');
    foreach ($utilities as $path) {
      $name = str_replace('.php', '', basename($path));
      $this->loadUtility($name);
    }
  }

  private function loadUtility($name) {
    require_once dirname(dirname(__FILE__)).'/utility/'.$name.'.php';
  }

  /**
   * Load the configuration file.
   *
   * Ultimately this will load the DaGdConfig class (into global scope)
   * which exposes configuration and getters statically.
   */
  public function loadConfig($file) {
    if (!$file ||
        !@include_once(dirname(dirname(dirname(__FILE__)))).'/'.$file) {
      throw new Exception("No configuration file could be loaded.");
    }
    $this->config_loaded = true;
    return $this;
  }

  /**
   * Establish some global framework state by setting things like timezone and
   * error message display.
   */
  public function establishGlobalState() {
    $this->ensureConfigLoaded();

    $timezone = DaGdConfig::get('general.timezone');
    if ($timezone) {
      date_default_timezone_set($timezone);
    }

    $display_errors = DaGdConfig::get('general.display_errors');
    if ($display_errors) {
      ini_set('error_reporting', E_ALL);
      ini_set('display_startup_errors', true);
      ini_set('display_errors', true);
    }

    return $this;
  }

  /**
   * Establish the class autoloader, the preferred way to load in applications
   * and their utility classes.
   */
  public function establishAutoloader() {
    $this->ensureConfigLoaded();
    spl_autoload_register('DaGdStartup::__dagd_autoload', $throw = true);
  }

  /**
   * Perform a singular auto-load, given a class name.
   */
  public static function __dagd_autoload($cls) {
    $paths = DaGdConfig::get('general.autoload_search');
    foreach ($paths as $path) {
      $path = rtrim($path, '/').'/';
      $files = array();
      // Paths are expected to be relative to 'src', or absolute for custom apps
      if ($path[0] == '/') {
        $files = glob($path.'/'.$cls.'.php');
      } else {
        $src = dirname(dirname(dirname(__FILE__)));
        $files = glob($src.'/'.$path.'/'.$cls.'.php');
      }
      if (!empty($files)) {
        include_once $files[0];
        break;
      }
    }
  }
}
