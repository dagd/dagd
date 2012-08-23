<?php
// This is the general config file for DaGd stuff.
class DaGdConfig {
  public static $config = array(
    'general.environment' => 'development',
    'general.debug' => false,
    'general.display_errors' => false,
    'general.baseurl' => 'http://dagd.local', // DO *NOT* include trailing '/'.

    // This array is imploded by | into a regex. Escape any | or # used in it.
    // I am not including ELinks here, because it can support linking
    'general.text_useragent_search' => array(
      'Wget',
      'curl',
      'libcurl',
      'Supybot',
      'Ruby',
    ),

    'general.useragent' => 'da.gd/1.0',
    'general.applications' => array(
      'base', // This absolutely MUST be first.
      'ip',
      'useragent',
      'comingsoon',
      'help',
      'whois',
      'editcount',
      'shorten',
      'pastebin',
      'headers',
      'up',
      'host',
      'break',
      'commander',
      'image',
    ),
    'general.routemap' => array(
      '/help/?$' => 'DaGdHelpController',
      '/ua/?$' => 'DaGdUserAgentController',
      '/ip/?$' => 'DaGdIPController',
      '/w/(.+)/?$' => 'DaGdWhoisController',
      '/ec/(.+)/?$' => 'DaGdEditCountController',
      '/up/(.+)/?$' => 'DaGdIsItUpController',
      '/host/(.+)/?$' => 'DaGdHostController',
      '/headers/?(.+)?/?$' => 'DaGdHeadersController',
      '/break/?$' => 'DaGdBreakController',
      '/c/(store)/(.+?)/(.+?)/?$' => 'DaGdCommanderController',
      '/c/(.+?)(?:/| )(.+?)/?$' => 'DaGdCommanderController',
      '/c/?(json|)?/?$' => 'DaGdCommanderController',
      '/image/([0-9x*]+)(?:\.|/|)(\w+)?/?$' => 'DaGdImageController',
      '/(?:p|paste|pastebin)/?(\d+)?/?$' => 'DaGdPastebinController',
      '/(?:(?:shorten|s|)(?:/|$))?(.+?)?/?(original)?$' => 'DaGdShortenController'),

    // These redirects take place on CLI useragents only.
    'general.cli_routemap' => array(
      '/$' => 'DaGdHelpController',
    ),

    // These are just full-out redirects.
    'general.redirect_map' => array(
      '/et/(\d+)/?$' => 'http://www.etsy.com/listing/$1'),
      

    // These are extra headers that get applied globally
    'general.extra_headers' => array(
      'Cache-Control: no-cache',
    ),

    // A hardcoded map of whois servers to use for certain domains.
    'whois.hardcode_map' => array(
      // tld (WITHOUT '.') => server
      'gd' => array(
        // Bit of a hack, the whois-servers.net answers .gd directly for
        // some reason, so we trick fetchWhoisServer().
        'server' => 'gd.whois-servers.net',
      ),
      'ly' => array(
        'server' => 'whois.nic.ly',
      ),
      'de' => array(
        'server' => 'whois.denic.de',
        'query' => '-T dn,ace',
      ),
    ),

    // Should error emails get sent out?
    'exceptions.email' => false,
    
    // Only send emails in non-debug mode. 'exceptions.email' must be true.
    'exceptions.email_in_debug' => false,

    // The list of people to email on exceptions.
    'exceptions.mail_to' => array(
      'ricky@elrod.me',
    ),

    // MySQL settings
    'mysql.host' => 'localhost',
    'mysql.user' => 'root',
    'mysql.password' => '',
    'mysql.database' => 'dagd',

    // IsItUp Settings
    'isitup.max_redirects' => 5,
    'isitup.timeout' => 3,

    // Image settings
    'image.max_height' => 7000,
    'image.max_width' => 7000,
    'image.fontpath' => '/usr/share/fonts/dejavu/DejaVuSansMono.ttf',
    'image.fontsize' => 20,
    'image.default_bg_rgb' => array(44, 44, 44),
    'image.default_text_rgb' => array(150, 150, 150),
    'image.default_filetype' => 'png', // Must be a key of the below array.
    'image.imagetypes' => array(
      // extension => ('contenttype' => $a, 'phpfunction' => $b)
      'png' => array(
        'contenttype' => 'image/png',
        'phpfunction' => 'imagepng',
      ),
      'jpg' => array(
        'contenttype' => 'image/jpeg',
        'phpfunction' => 'imagejpeg',
      ),
      'jpeg' => array(
        'contenttype' => 'image/jpeg',
        'phpfunction' => 'imagejpeg',
      ),
      'gif' => array(
        'contenttype' => 'image/gif',
        'phpfunction' => 'imagegif',
      ),
      'xbm' => array(
        'contenttype' => 'image/x-xbitmap',
        'phpfunction' => 'imagexbm',
      ),
      'wbmp' => array(
        'contenttype' => 'image/vnd.wap.wbmp',
        'phpfunction' => 'imagewbmp',
      ),
    ),
  );

  public static function get($key) {
    return self::$config[$key];
  }
}
