<?php

require_once dirname(__FILE__).'/resources/random_string.php';
require_once dirname(__FILE__).'/resources/blacklist.php';

class DaGdShortenController extends DaGdController {
  // TODO: Port to new help system
  public static function getHelp() {
    return array(
      'title' => 'shorten',
      'summary' => 'Shorten your long URLs (/, /s, /shorten).',
      'path' => '',
      'examples' => array(
        array(
          'arguments' => null,
          'request' => array(
            'url' => 'http://some_long_url',
            'shorturl' => 'slug'),
          'summary' => 'Shorten a URL'),
        array(
          'arguments' => array('g'),
          'summary' => 'An example short URL with a custom suffix'),
      ));
  }

  public function getStyle() {
    $style = <<<EOD
#app input.submit:hover {
  transition: all 0.5s ease;
  background-color: #3a9;
  color: #fff;
}
#app input.submit {
  background-color: transparent;
  transition: background-color 0.5s ease;
  border: none;
  font-size: 1em;
  color: #888;
}
#app input.textinput, #app span#shorturl_label {
  padding: 20px;
  box-sizing: border-box;
  font-family: "Proxima Nova Condensed", Roboto, Ubuntu, sans-serif;
  font-weight: 100;
  font-size: 2em;
}
#app input#url {
  border-radius: 3px;
  border: 2px solid #ddd;
  width: 100%;
}
#app input.textinput:focus, #app #flex:focus-within { border-color: #3a9 !important; }
#app #shorturl_label {
  box-sizing: border-box;
  padding: 20px;
  padding-right: 0;
  font-size: 2.5em;
  background-color: #fff;
}
#app input#shorturl {
  padding-left: 0;
  border: none;
}
#app #flex {
  border: 1px solid #ddd;
}
EOD;

    return array_merge(
      parent::getStyle(),
      array($style)
    );
  }

  /*
  public function getStatsForURL($shorturl) {
    // This function costs two queries, but they both hit already-existing
    // indexes and so they should be nearly trivial.
    $id = null;
    $creation_dt = null;
    $longurl = null;

    // Get some initial info
    $shorturls_query = $this->getReadDB()->prepare(
      'SELECT id,creation_dt,longurl FROM shorturls '.
      'WHERE shorturl=? AND enabled=1');
    $shorturls_query->bind_param('s', $shorturl);
    $start = microtime(true);
    $shorturls_query->execute();
    $end = microtime(true);
    statsd_time('query_time_stats_select', ($end - $start) * 1000);
    $shorturls_query->bind_result($id, $creation_dt, $longurl);
    $shorturls_query->fetch();
    $shorturls_query->close();

    // If $id never gets set, the shorturl doesn't exist. Bail out and return
    // null.
    if ($id === null) {
      return null;
    }

    $count_accesses = null;

    $access_query = $this->getReadDB()->prepare(
      'SELECT count(id) FROM shorturl_access '.
      'WHERE shorturl_id=?');
    $access_query->bind_param('i', $id);
    $start = microtime(true);
    $access_query->execute();
    $end = microtime(true);
    statsd_time('query_time_stats_access', ($end - $start) * 1000);
    $access_query->bind_result($count_accesses);
    $access_query->fetch();
    $access_query->close();

    $res = array(
      'id' => $id,
      'creation_dt' => $creation_dt,
      'longurl' => $longurl,
      'accesses' => $count_accesses,
    );
    return $res;
  }
  */

  private function redirect($matches) {
    $surl = id(new DaGdShortURLQuery($this))
      ->fromShort($matches[1]);

    if (!$surl) {
      return null;
    }

    // This check is best-effort to allow all older entries to continue
    // working even if they don't host-parse.
    // If it's whitelisted, don't even bother checking dnsbl
    if (!$surl->isWhitelisted() && $surl->isBlacklisted()) {
      return null;
    }

    $logger = new DaGdShortURLAccessLogger($this, $surl);
    // It's fine if getHeader() is false -- the column is nullable.
    $logger->log($this->getRequest()->getHeader('user-agent'));

    // TODO: Move build_given_querystring() to DaGdRequest.
    $qs = build_given_querystring();

    $response = new DaGdRedirectResponse();

    if ($matches[2]) {
      $response->setTo($surl->getLongUrl().'/'.$matches[2].$qs);
    } else {
      $response->setTo($surl->getLongUrl().$qs);
    }

    statsd_bump('shorturl_access');
    $response->addHeader('X-Original-URL', $surl->getLongUrl());
    return $response;
  }

  /*
  public function execute(DaGdResponse $response) {
    $matches = $this->getRequest()->getRouteMatches();
    if ($matches[1]) {
      $will_redirect = $this->redirect($matches, $response);
      if ($will_redirect) {
        return '';
      } else {
        return id(new DaGd404Controller())->execute($response);
      }
    }
  }*/

  /**
   * Try to set up a DaGdRedirectResponse to the long url if it exists.
   * Otherwise set up a 404 and render it with hopefully the right renderer.
   *
   * @return DaGdRedirectResponse or more generally a DaGdResponse
   */
  private function getRedirectResponse($matches) {
    $redir = $this->redirect($matches);
    if ($redir) {
      return $redir;
    }
    return $this->error(404)->finalize($response);
  }

  public function renderText(DaGdTextResponse $response) {
    $matches = $this->getRequest()->getRouteMatches();
    if ($matches[1]) {
      return $this->getRedirectResponse($matches);
    }
  }

  public function render(DaGdHTMLResponse $response) {
    $matches = $this->getRequest()->getRouteMatches();
    if ($matches[1]) {
      return $this->getRedirectResponse($matches);
    }

    $template = $this
      ->getBaseTemplate()
      ->setBody($this->form())
      ->setStyle($this->getStyle())
      ->setTitle(idx($this->getHelp(), 'title', 'Welcome!'))
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function execute(DaGdResponse $response) {
    return 'That request was a bit too fancy for us.';
  }

  /*
  private function store_shorturl() {
    if (!$this->store_url) {
      return true;
    }
    $query = $this->getWriteDB()->prepare(
      'INSERT INTO shorturls(shorturl, longurl, owner_ip, custom_shorturl, '.
      'longurl_hash) VALUES(?, ?, ?, ?, ?);');
    $query->bind_param(
      'sssis',
      $this->short_url,
      $this->long_url,
      $this->owner_ip,
      $this->custom_url,
      $this->longurl_hash);

    $start = microtime(true);
    $res = $query->execute();
    $end = microtime(true);
    statsd_time('query_time_store_shorturl_insert', ($end - $start) * 1000);

    if ($res) {
      statsd_bump('shorturl_store');
      return true;
    } else {
      error500('Something has gone wrong! :( ... Try again? Please?');
      statsd_bump('shorturl_store_fail');
      return false;
    }
  }

  private function set_shorturl_or_400() {
    if ($short_url = request_or_default('shorturl')) {
      $this->custom_url = true;
      $valid_char_pattern = DaGdConfig::get('shorten.custom_url_regex');
      if (!preg_match($valid_char_pattern, $short_url)) {
        error400('Invalid short URL entered. Alphanumeric only, please.');
        return false;
      } else {
        $routes = DaGdConfig::get('general.routemap');
        foreach ($routes as $route => $metadata) {
          if ($metadata['controller'] == 'DaGdShortenController') {
            continue;
          }
          $route = substr($route, 1);
          if (preg_match('@^'.$route.'@', $short_url)) {
            error400(
              'That URL conflicts with other da.gd URLs. Please use another '.
              'URL.');
            return false;
          }
        }
        $this->short_url = substr($short_url, 0, 10);
        if (!$this->isFreeShortURL()) {
          error400('That custom URL was already taken, go back and try again!');
          return false;
        }
      }
      statsd_bump('shorturl_new_custom');
    } else {
      $this->longurl_hash = hash('sha256', $this->long_url);
      $this->getNonCustomShortURL($this->longurl_hash);
      if ($this->short_url) {
        // The idea here is that we query against the hash which has an index
        // that we hit with getNonCustomShortURL.
        // If we get a result, then it's stored in $this->short_url, and we don't
        // want to re-insert it, so we set store_url false.
        //
        // See 1d3974d741eab5765cf9e20ca5e7277be1747699 for some reasoning, but
        // ultimately it comes down to maximum InnoDB index key length and this
        // being a workaround to make it quicker to add random longurls. We want
        // to re-use random shorturls when shortening the same longurl and
        // without being able to add longurl into an index, our lookup times
        // were crazy on random url inserts.
        $this->store_url = false;
      } else {
        $min = DaGdConfig::get('shorten.random_min_length');
        $max = DaGdConfig::get('shorten.random_max_length');
        $this->short_url = randstr(rand($min, $max));
        while (!$this->isFreeShortURL()) {
          debug('Hash collision', 'Calling randstr again');
          statsd_bump('shorturl_random_hash_collision');
          $this->short_url = randstr(rand($min, $max));
        }
      }
      statsd_bump('shorturl_new_random');
    }

    $this->short_url = htmlspecialchars(urlencode($this->short_url));
    return true;
  }

  public function set_longurl_or_400() {
    if ($_REQUEST['url'] == '') {
      // If url was there but is an empty string, say so.
      error400('Error: Cannot create something out of nothing.');
      return false;
    }

    if ($long_url = request_or_default('url')) {
      // Something has at least been submitted. Is it valid?
      // Good enough for now...probably needs some better checks.
      if (preg_match('@^https?://@', $long_url)) {

        if ($this->isBannedAuthor()) {
          statsd_bump('shorturl_author_ip_banned');
          error403();
          return false;
        }

        // If it's not whitelisted, and it IS blacklisted, then bail out.
        if (!$this->whitelisted($long_url)) {
          if ($this->blacklisted($long_url)) {
            error400('Blacklisted original URL.');
            return false;
          }
        }

        $this->long_url = $long_url;
        return true;
      } else {
        error400('http and https protocols only, please.');
        return false;
      }
    } else {
      return false;
    }
  }

  public function render() {
    if (array_key_exists('url', $_REQUEST)) {
      $this->owner_ip = client_ip();
      if ($this->set_longurl_or_400() && $this->set_shorturl_or_400()) {
        if ($this->store_shorturl()) {
          header('X-Short-URL: '.$this->short_url);
          $this->escape = false;
          $new_link = DaGdConfig::get('general.baseurl').'/'.$this->short_url;
          return tag(
            'a',
            $new_link,
            array(
              'href' => $new_link,
              'style' => 'text-align: center; display: block; margin-top: 20px;',
            )
          )->renderSafe();
        }
      }
      return;
    }

    // No 'url' was passed, so we are not creating a new short-url.
    if ($this->route_matches[1]) {
      // Attempt to access a stored URL
      $this->redirect_from_shorturl();
      return;
    } else {
      // We are not attempting to access a stored URL, but we also don't have
      // a 'url' - Show the form so that we can create a new short-url.
      if (!is_html_useragent()) {
        // No use in showing a form for text UAs. Rather, show help text.
        // TODO: Move this to renderCLI()
        return help('DaGdShortenController');
      }
  */

  private function form() {
    $branding = tag('h1', 'Private. Simple. Open.');

    $longurl_field = tag(
      'input',
      null,
      array(
        'type' => 'url',
        'name' => 'url',
        'id' => 'url',
        'size' => '35',
        'placeholder' => 'https://example.com/long-url-here',
        'autofocus' => TAG_ATTR_BARE,
        'required' => TAG_ATTR_BARE,
        'class' => 'textinput',
      )
    );

    $shorturl_label = tag(
      'div',
      'https://da.gd/',
      array(
        'id' => 'shorturl_label',
        'style' => 'font-family: monospace;',
      )
    );

    $shorturl_field = tag(
      'input',
      null,
      array(
        'type' => 'text',
        'maxlength' => '10',
        'name' => 'shorturl',
        'id' => 'shorturl',
        'size' => '10',
        'placeholder' => 'my_url*',
        'pattern' => '[A-Za-z0-9\-_]+',
        'style' => 'flex-grow: 12;',
        'class' => 'textinput',
      )
    );

    $submit = tag(
      'input',
      null,
      array(
        'type' => 'submit',
        'value' => 'Shorten URL',
        'style' => 'flex-grow: 1;',
        'class' => 'submit',
      )
    );

    $flex = tag(
      'div',
      array(
        $shorturl_label,
        $shorturl_field,
        $submit,
      ),
      array(
        'id' => 'flex',
        'style' => 'display: flex; width: 100%; margin-top: 20px;',
      )
    );

    $footnote = tag(
      'div',
      tag(
        'small',
        '* leave this blank for a random short URL'
      )
    );

    $abuse = tag(
      'div',
      tag(
        'small',
        'report abuse to abuse@da.gd'
      ),
      array(
        'style' => 'margin-top: 20px;',
      )
    );

    $form = tag(
      'form',
      tag(
        'table',
        array(
          $longurl_tr,
          $shorturl_tr,
          $submit_tr,
          $abuse,
        )
      ),
      array(
        'method' => 'POST',
        'action' => '/',
      )
    );

    $app = tag(
      'div',
      array(
        $branding,
        $longurl_field,
        $flex,
        $footnote,
        $abuse,
      )
    );

    return $app;
  }
}
