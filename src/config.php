<?php
// This is the general config file for DaGd stuff.
class DaGdConfig {
  private static $config = array(
    'general.text_useragent_search' => 'Wget|curl|libcurl|Elinks',
  );

  public static function get($key) {
    return self::$config[$key];
  }
}