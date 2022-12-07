<?php

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
#app input.textinput, #app #shorturl_label {
  padding: 20px;
  box-sizing: border-box;
  font-family: "Proxima Nova Condensed", Roboto, Ubuntu, sans-serif;
  font-weight: 100;
  font-size: 2em;
}
.darkmode #app input.textinput, .darkmode #app #shorturl_label {
  background-color: #666;
}
.lightmode #app input#shorturl, .lightmode #app input#url {
  background-color: #fff;
}
#app input#url {
  border-radius: 3px;
  border: 2px solid #ddd;
  width: 100%;
}
#app input.textinput:focus, #app #flex:focus-within {
  border-color: #3a9 !important;
  outline: none;
}
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

  private function redirect($matches) {
    $query = new DaGdShortURLQuery($this);

    if ($query->isRouteMapConflict($matches[1])) {
      // If a routemap change trumped an existing shorturl, consider it invalid.
      // Needed because otherwise it could still be accessed by route aliases
      // pointing to this controller. Do this before we hit the db.
      return null;
    }

    $surl = $query->fromShort($matches[1]);

    if (!$surl) {
      return null;
    }

    // This check is best-effort to allow all older entries to continue
    // working even if they don't host-parse.
    // If it's whitelisted, don't even bother checking dnsbl
    if (!$query->isWhitelisted($surl->getLongUrl()) &&
        $query->isBlacklisted($surl->getLongUrl(), false)) {
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

  /**
   * Try to set up a DaGdRedirectResponse to the long url if it exists.
   * Otherwise set up a 404 and render it with hopefully the right renderer.
   *
   * @return DaGdRedirectResponse or more generally a DaGdResponse
   */
  private function getRedirectResponse($matches, $response) {
    $redir = $this->redirect($matches);
    if ($redir) {
      return $redir;
    }
    $phishing_info = tag(
      'p',
      array(
        'This URL might have never existed, or might have been disabled '.
        'in response to a ',
        tag(
          'a',
          'phishing attack',
          array(
            'href' => 'https://en.wikipedia.org/wiki/Phishing',
          )),
        '.',
      ));
    return $this
      ->error(404)
      ->setHtmlMessage($phishing_info)
      ->finalize();
  }

  public function hasAuthorizationHeader() {
    return $this->getRequest()->getHeader('authorization') !== null;
  }

  public function renderText(DaGdTextResponse $response) {
    $shorturl = $this->getRequest()->getRouteComponent('shorturl');
    $url = $this->getRequest()->param('url', '');

    // Allow /s?url=... to work, but all other /<foo>?url= should ignore url.
    if ($shorturl == 's' || !$shorturl) {
      if (strlen($url) == 0) {
        return $this
          ->error(400, 'Long URL cannot be empty')
          ->finalize();
      }
      return $this->storeShortUrl($response);
    }

    $matches = $this->getRequest()->getRouteMatches();
    if ($matches[1]) {
      if ($this->hasAuthorizationHeader()) {
        statsd_bump('shorturl_access_with_authorization');
        return $this
          ->error(
            403,
            'Short URL access requests cannot contain an Authorization header.')
          ->finalize();
      }
      return $this->getRedirectResponse($matches, $response);
    }
  }

  public function render(DaGdHTMLResponse $response) {
    $shorturl = $this->getRequest()->getRouteComponent('shorturl');
    $url = $this->getRequest()->param('url', '');

    // Allow /s?url=... to work, but all other /<foo>?url= should ignore url.
    if (strlen($url) > 0 && ($shorturl == 's' || !$shorturl)) {
      return $this->storeShortUrl($response);
    }

    $matches = $this->getRequest()->getRouteMatches();
    if ($matches[1]) {
      if ($this->hasAuthorizationHeader()) {
        statsd_bump('shorturl_access_with_authorization');
        return $this
          ->error(
            403,
            'Short URL access requests cannot contain an Authorization header.')
          ->finalize();
      }
      return $this->getRedirectResponse($matches, $response);
    }

    $template = $this
      ->getBaseTemplate()
      ->setBody($this->form())
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function execute(DaGdResponse $response) {
    return 'That request was a bit too fancy for us.';
  }

  /**
   * Attempts to store a ShortURL after running a plethora of validations on it.
   */
  private function storeShortUrl($response) {
    $given_shorturl = $this->getRequest()->param('shorturl', '');
    $given_shorturl = substr($given_shorturl, 0, 10);
    $given_longurl = $this->getRequest()->param('url');
    $is_custom_url = strlen($given_shorturl) > 0;

    if (!$given_longurl) {
      return $this
        ->error(
          400,
          'Cannot create something out of nothing. No long URL given.')
        ->finalize();
    }

    $query = new DaGdShortURLQuery($this);

    // Does the URL at least look somewhat valid?
    if (!preg_match('@^https?://@', $given_longurl)) {
      statsd_bump('shorturl_invalid_scheme');
      return $this
        ->error(400, 'Long URL must have http:// or https:// scheme.')
        ->finalize();
    }

    if (filter_var($given_longurl, FILTER_VALIDATE_URL) === false) {
      statsd_bump('shorturl_invalid_longurl');
      return $this
        ->error(400, 'Long URL is not a valid URL.')
        ->finalize();
    }

    // Is the user's IP banned?
    if ($query->isBannedIP($this->getRequest()->getClientIP())) {
      statsd_bump('shorturl_author_ip_banned');
      return $this->error(403)->finalize();
    }

    // Before we run the blacklist logic on the longurl (expensive), see if the
    // shorturl is in the shorturl regex blacklist.
    if ($given_shorturl) {
      $blacklist_regexes = DaGdConfig::get('shorten.shorturl_blacklist');
      foreach ($blacklist_regexes as $regex) {
        if (preg_match('#'.$regex.'#i', $given_shorturl)) {
          statsd_bump('shorturl_blacklisted');
          statsd_bump('custom_shorturl_blacklisted');
          return $this
            ->error(400)
            ->finalize();
        }
      }
    }

    // Everything is good so far, let's figure out what shorturl to use.
    if ($given_shorturl) {
      // We are given a short URL (as opposed to being asked to generate a
      // random one). First check if the shorturl is valid.
      if (!$query->isValidShortUrl($given_shorturl)) {
        statsd_bump('shorturl_custom_invalid_regex');
        return $this
          ->error(400, 'Custom short URL contained invalid characters.')
          ->finalize();
      }

      // Has the URL been taken yet?
      if (!$query->isFreeShortURL($given_shorturl)) {
        statsd_bump('shorturl_custom_not_free');
        return $this
          ->error(400, 'Short URL already taken. Pick a different one.')
          ->finalize();
      }

      // Is someone trying to be sneaky and take the URL of another app?
      if ($query->isRouteMapConflict($given_shorturl)) {
        statsd_bump('shorturl_routemap_conflict');
        return $this
          ->error(
            400,
            'Custom short URL conflicts with other da.gd URLs. '.
            'Pick a different one.')
          ->finalize();
      }
    } else {
      // We are not given a short URL. If we already have a random URL for the
      // long URL, use it. Otherwise create a new one. We query by hash to sate
      // some indexing. See 1d3974d741eab5765cf9e20ca5e7277be1747699 for
      // details.
      $existing_shorturl = $query->fromLongByHash($given_longurl);
      if ($existing_shorturl) {
        // If we have a match, we are done.
        return id(new DaGdShortenStoredController())
          ->setShortUrl($existing_shorturl)
          ->setRequest($this->getRequest())
          ->finalize();
      } else {
        // Otherwise the short URL does not exist in the database. Find a good
        // random string to use.
        $min = DaGdConfig::get('shorten.random_min_length');
        $max = DaGdConfig::get('shorten.random_max_length');
        $given_shorturl = randstr(rand($min, $max));
        while (!$query->isFreeShortURL($given_shorturl)) {
          statsd_bump('shorturl_random_hash_collision');
          $given_shorturl = randstr(rand($min, $max));
        }
      }
      statsd_bump('shorturl_new_random');
    }

    // Is the long URL whitelisted or blacklisted?
    if (!$query->isWhitelisted($given_longurl)) {
      // It's not whitelisted...
      if ($query->isBlacklisted($given_longurl, true)) {
        // ...but it *is* blacklisted.
        return $this
          ->error(400, 'Blacklisted long URL.')
          ->finalize();
      }
    }

    // TODO: This can throw. We render a "pretty" 500, but maybe we should
    // catch and do some extra stuff.
    $surl = $query->store(
      $this->getRequest()->getClientIP(),
      $given_shorturl,
      $given_longurl,
      $is_custom_url);

    return id(new DaGdShortenStoredController())
      ->setShortUrl($surl)
      ->setRequest($this->getRequest())
      ->finalize();
  }

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
        'autofocus' => DaGdTag::TAG_ATTR_BARE,
        'required' => DaGdTag::TAG_ATTR_BARE,
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
      array(
        $longurl_field,
        $flex,
        $footnote,
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
        $form,
        $abuse,
      )
    );

    return $app;
  }
}
