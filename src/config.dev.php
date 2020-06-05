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

    // general.applications is now deprecated in favor of autoloading.
    // Instead of having your controller in `src/applications/foo/foo.php` and
    // adding `foo` to this array, and the controller name in
    // `general.routemap`, simply name the controller file the same as the class
    // name, and reference that in `general.routemap` instead.
    // Applications loaded the legacy way will continue to work for now, for
    // compatibility with anyone who has extended da.gd with custom apps, but
    // autoloading should be preferred going forward.
    'general.applications' => array(
    ),

    // Directories searched for class autoloading. Paths relative to 'src/'.
    // These are passed to glob(), so can contain wildcards. Each directory is
    // searched for *.php files, and the class name exported by the file *must*
    // match the filename (sans ".php"). This is not yet used for application
    // loading but that is a planned enhancement.
    // Note that this does NOT recurse into subdirectories, they must be listed
    // individually.
    'general.autoload_search' => array(
      'resources/http/',
      'resources/html/',
      'resources/help/',
      'applications/*/',
      'applications/*/resources/',
      'cli/',
    ),

    // Control statsd metrics collection.
    'general.statsd' => false,

    'general.statsd_host' => 'localhost',

    // Note, we solely use UDP here.
    'general.statsd_port' => 9125,

    'general.statsd_prefix' => 'dagd_',

    'general.default_methods' => array(
      'GET',
      'HEAD',
    ),

    'general.routemap' => array(
      '/help/?$' => array(
        'controller' => 'DaGdHelpController',
      ),
      '/newhelp/?(.+)?/?$' => array(
        'controller' => 'DaGdNewHelpController',
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
      '/ec/(?P<username>.+)/?$' => array(
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
      '/c/(.+?)(?:/| |%20)(.+?)/?$' => array(
        'controller' => 'DaGdCommanderController',
      ),
      '/c/?(json|)?/?$' => array(
        'controller' => 'DaGdCommanderController',
      ),
      '/dns/(.+)/?$' => array(
        'controller' => 'DaGdDNSController',
      ),
      '/roll/(\d+)?d(\d+)(?:([ +-])(\d+))?/?$' => array(
          'controller' => 'DaGdRollController',
      ),
      '/status/(?P<code>\d+)/?(?P<message>.+)?/?' => array(
        'controller' => 'DaGdStatusController',
      ),
      '/image/([0-9x*]+)(?:\.|/|)(\w+)?/?$' => array(
        'controller' => 'DaGdImageController',
      ),
      '/isp/?(.+)?/?$' => array(
        'controller' => 'DaGdISPController',
      ),
      '/leftpad/([0-9]+)/(.+)/(.+)/?$' => array(
        'controller' => 'DaGdLeftPadController',
      ),
      '/metrics/(.+)/(.+)/?$' => array(
        'controller' => 'DaGdMetricsController',
      ),
      '/coshorten/([^/]+)?/?(.*)?$' => array(
        'controller' => 'DaGdCoShortenController',
      ),
      '/stats/([^/]+)?/?(.*)?$' => array(
        'controller' => 'DaGdStatsController',
      ),
      '/cow/?$' => array(
        'controller' => 'DaGdCowController',
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

    // How should the session data be encrypted?
    'session.encryption_method' => 'aes-256-cbc',

    // And with what key? This must be set if using any apps that use
    // DaGdSession, or else DaGdSession will fail loudly.
    'session.encryption_key' => null,

    // How long should sessions last? This sets the cookie expiry time.
    // This is in seconds. Default is 30 days (60 * 60 * 24 * 30 seconds).
    'session.expiry' => 2592000,

    // Which IPs can access /metrics?
    'metrics.allowed_ips' => array(
      '127.0.0.1',
      '::1',
      '172.19.0.1',
    ),

    // Regexes we blacklist on.
    'shorten.longurl_blacklist' => array(),

    // Strings we blacklist on. These are attempted before
    // shorten.longurl_blacklist because doing substring searches is
    // significantly faster than doing regex matches.
    'shorten.longurl_blacklist_strings' => array(
      // A non-existent default, mainly for integration tests
      'some.spam.url',
    ),

    // Regexes we whitelist on to avoid checking dnsbl.
    'shorten.longurl_whitelist' => array(),

    // Strings we whitelist on as above. These are attempted before
    // shorten.longurl_whitelist because doing substring searches is
    // significantly faster than doing regex matches.
    'shorten.longurl_whitelist_strings' => array(),

    // A list of DNS servers to query for checking against DNSBL databases.
    // One of these is randomly selected each time. We default to OpenDNS,
    // as Google's public DNS servers do not work here for certain databases.
    'shorten.blacklist_via' => array(
      '208.67.222.222',
      '208.67.220.220',
    ),

    // The actual DNSBL databases to query. The domain of the URL being
    // shortened is prepended to these. All of them must return NXDOMAIN
    // otherwise the URL is rejected. These are checked at shorturl creation
    // time and at access time.
    // Make this array empty to disable this functionality.
    'shorten.dnsbl' => array(
      '.dbl.spamhaus.org',
      '.multi.surbl.org',
    ),

    // Should we send URLs to Google Safe Browsing? This check will take place
    // both on URL storing and on URL access.
    'shorten.safe_browsing' => false,

    // How long before we give up? (seconds)
    'shorten.safe_browsing_timeout' => 2,

    // Should we default to being accepted?
    // If true, if we timeout or otherwise can't access Safe Browsing, we
    // accept the URL anyway. If false, we reject it.
    'shorten.safe_browsing_default_accept' => true,

    // Client ID sent to the Google Safe Browsing API. Must be one word,
    // lowercase.
    'shorten.safe_browsing_client_id' => 'dagd',

    // Client version sent to the Google Safe Browsing API.
    'shorten.safe_browsing_client_version' => 'git-HEAD',

    // Threat types from this list:
    // https://developers.google.com/safe-browsing/v4/reference/rest/v4/ThreatType
    'shorten.safe_browsing_threat_types' => array(
      'MALWARE',
      'SOCIAL_ENGINEERING',
    ),

    // Platform types from this list:
    // https://developers.google.com/safe-browsing/v4/reference/rest/v4/PlatformType
    'shorten.safe_browsing_platform_types' => array(
      'ANY_PLATFORM',
    ),

    // API key from Google Console.
    'shorten.safe_browsing_api_key' => '',

    // Regex to validate custom short URLs against
    'shorten.custom_url_regex' => '@^[\d\w-_]+$@i',

    // Minimum length random shorturl.
    'shorten.random_min_length' => 4,

    // Maximum length random shorturl.
    'shorten.random_max_length' => 6,

    // Which characters can be in a random shorturl?
    'shorten.random_charset' =>
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',

    // The default transient whois server.
    'whois.transient_server' => array(
      'server' => 'whois.arin.net',
      'port' => 43,
      'query' => 'n +',
    ),

    // General TLD-based whois server lookup. The string "TLD" will be replaced
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
      'edu' => array(
        // This one requires that we don't prefix the query with 'domain '.
        // Oh, and avoid fallback.
        'server' => 'edu.whois-servers.net',
        'query' => '',
      ),
      'de' => array(
        'server' => 'whois.denic.de',
        'query' => '-T dn,ace',
      ),
      'me' => array(
        // It appears whois.nic.me will handle its own responses without any
        // need for a redirect. Further, it seems the redirect attempt on some
        // domains here is futile. e.g. Following the redirect for
        // "hotspot.me" takes us to a server that doesn't know that domain even
        // exists.
        'server' => 'whois.nic.me',
        'query' => '',
        'query_directly' => true,
      ),
      'frl' => array(
        // The .frl v6 servers don't respond. Hardcode the v4 address for now.
        'server' => '119.252.181.87',
        'query' => '',
        'query_directly' => true,
      ),
      'space' => array(
        // Same here - v6 doesn't respond.
        'server' => '119.252.181.51',
        'query' => '',
        'query_directly' => true,
      ),
      'xyz' => array(
        // Same here - v6 doesn't respond.
        'server' => '119.252.181.42',
        'query' => '',
        'query_directly' => true,
      ),
      'name' => array(
        // This server requires a special format because it wants to be special.
        'server' => 'whois.nic.name',
        'query' => 'domain = '
      ),
    ),

    // These referral servers are blacklisted. If we hit one of them, we simply
    // bail out and return the transient result.
    'whois.referral_blacklist' => array(
      'ipmt.rr.com',
      // Service Not Available: exceeded max client sessions
      'rwhois.eng.bellsouth.net',
      // Parsed as referral target in some cases but doesn't answer
      'www.enom.com',
      // Referral target but doesn't seem to know anything
      'whois.1api.net',
      'whois.comlaude.com',
      // Referral target but utterly useless (wants you to visit their web
      // service)
      'whois.godaddy.com',
      // Google can't handle its own domains
      // "That TLD is not handled by this service."
      'whois.google.com',
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

    // MySQL settings - these can be used for reads *and* writes, but if
    // readonly_mysql.host is set, it will be preferred in certain cases for
    // reads.
    'mysql.host' => 'localhost',
    'mysql.user' => 'root',
    'mysql.password' => '',
    'mysql.database' => 'dagd',

    // Read-only MySQL settings. These will be preferred for most reads if
    // readonly_mysql.host is non-null. This allows for distributing dagd
    // servers and giving them local reads while still writing to a distant
    // master. It is recommended to use a separate, read-only user for this.
    'readonly_mysql.host' => null,
    'readonly_mysql.user' => 'readonly',
    'readonly_mysql.password' => '',
    'readonly_mysql.database' => 'dagd',

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
