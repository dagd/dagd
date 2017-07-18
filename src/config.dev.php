<?php
// This is the general config file for DaGd stuff.
class DaGdConfig {
  public static $config = array(
    'general.environment' => 'development',

    'general.debug' => false,

    'general.display_errors' => false,

    'general.baseurl' => 'http://dagd.local', // DO *NOT* include trailing '/'.

    'general.useragent' => 'da.gd/1.0',

    // These are the "Accept:" headers we return html for.
    // Any of these can match anywhere in the Accept header.
    // These are imploded by "|", so all |'s should be escaped.
    'general.html_accept' => array(
      'text/html',
      'application/xhtml+xml'
    ),

    'general.applications' => array(
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
      'status',
      'isp',
      'dns',
      'leftpad',
    ),

    'general.default_methods' => array(
      'GET',
      'HEAD',
    ),

    'general.routemap' => array(
      '/help/?$' => array(
        'controller' => 'DaGdHelpController',
      ),
      '/ua/?$' => array(
        'controller' => 'DaGdUserAgentController',
      ),
      '/ip/?$' => array(
        'controller' => 'DaGdIPController',
      ),
      '/w/(.+)/?$' => array(
        'controller' => 'DaGdWhoisController',
      ),
      '/ec/(.+)/?$' => array(
        'controller' => 'DaGdEditCountController',
      ),
      '/up/(.+)/?$' => array(
        'controller' => 'DaGdIsItUpController',
      ),
      '/host/(.+)/?$' => array(
        'controller' => 'DaGdHostController',
      ),
      '/headers/?(.+)?/?$' => array(
        'controller' => 'DaGdHeadersController',
      ),
      '/break/?$' => array(
        'controller' => 'DaGdBreakController',
      ),
      '/c/(store)/(.+?)/(.+?)/?$' => array(
        'controller' => 'DaGdCommanderController',
      ),
      '/c/(.+?)(?:/| )(.+?)/?$' => array(
        'controller' => 'DaGdCommanderController',
      ),
      '/c/?(json|)?/?$' => array(
        'controller' => 'DaGdCommanderController',
      ),
      '/dns/(.+)/?$' => array(
        'controller' => 'DaGdDNSController',
      ),
      '/status/(\d+)/?(.+)?/?' => array(
        'controller' => 'DaGdStatusController',
      ),
      '/image/([0-9x*]+)(?:\.|/|)(\w+)?/?$' => array(
        'controller' => 'DaGdImageController',
      ),
      '/(?:p|paste|pastebin)/?(\d+)?/?$' => array(
        'controller' => 'DaGdPastebinController',
      ),
      '/isp/?(.+)?/?$' => array(
        'controller' => 'DaGdISPController',
      ),
      '/leftpad/([0-9]+)/(.+)/(.+)/?$' => array(
        'controller' => 'DaGdLeftPadController',
      ),
      '/coshorten/([^/]+)?/?(.*)?$' => array(
        'controller' => 'DaGdCoShortenController',
      ),
      '/(?:(?:shorten|s|)(?:/|$))?([^/]+)?/?(.*)?$' => array(
        'controller' => 'DaGdShortenController',
        'methods' => array('GET', 'HEAD', 'POST'),
      ),
    ),

    // These routes take place on CLI useragents only.
    'general.cli_routemap' => array(
      '/$' => array(
        'controller' => 'DaGdHelpController',
        'methods' => array('GET', 'HEAD'),
      ),
    ),

    // These are just full-out redirects.
    'general.redirect_map' => array(
      '/et/(\d+)/?$' => 'http://www.etsy.com/listing/$1',
    ),

    // These are extra headers that get applied globally
    'general.extra_headers' => array(
      'Cache-Control: no-cache',
      'Access-Control-Allow-Origin: *', // CORS
      'Expires: -1',
    ),

    // Required PHP extensions
    // We fatal if these aren't found.
    'general.required_extensions' => array(
      'gd',
    ),

    // Regexes we blacklist on
    'shorten.longurl_blacklist' => array(),

    // Regex to validate custom short URLs against
    'shorten.custom_url_regex' => '@^[\d\w-_]+$@i',

    // The default transient whois server.
    'whois.transient_server' => array(
      'server' => 'whois.arin.net',
      'port' => 43,
      'query' => 'n +',
    ),

    // General TLD-based whois server lookup. The string "TLD" wlil be replaced
    // with the tld.
    'whois.generic_tld_servers' => array(
      array(
        'server' => 'whois.nic.TLD',
        'query' => '',
      ),
      array(
        'server' => 'TLD.whois-servers.net',
        'query' => 'domain ',
      ),
    ),

    // How long should we wait before we try the next server in the above list?
    'whois.generic_tld_timeout' => 1,

    // A hardcoded map of whois servers to use for certain domains.
    // Often these use the typical "whois.nic.TLD" without any 'domain ' prefix
    // in their query.
    'whois.hardcode_map' => array(
      // tld (WITHOUT '.') => server
      'com' => array(
        // Since whois.nic.com doesn't work. We would fall back, but this makes
        // things a bit quicker.
        'server' => 'com.whois-servers.net',
        'query' => 'domain ',
      ),
      'org' => array(
        // Same deal here: whois.nic.org times out. Fallback works, but let's
        // avoid needing it.
        'server' => 'org.whois-servers.net',
        'query' => 'domain ',
      ),
      'net' => array(
        // Yep.
        'server' => 'net.whois-servers.net',
        'query' => 'domain ',
      ),
      'de' => array(
        'server' => 'whois.denic.de',
        'query' => '-T dn,ace',
      ),
    ),

    // These referral servers are blacklisted. If we hit one of them, we simply
    // bail out and return the transient result.
    'whois.referral_blacklist' => array(
      'rwhois.eng.bellsouth.net', // Service Not Available: exceeded max client sessions
      'ipmt.rr.com',
    ),

    // How long should we wait before timing out trying to hit a server we are
    // referred to? (in seconds)
    'whois.redirect_timeout' => 2,

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
