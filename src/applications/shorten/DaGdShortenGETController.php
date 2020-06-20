<?php

class DaGdShortenGETController extends DaGdController {
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

  private function redirect($matches) {
    $query = new DaGdShortURLQuery($this);
    $surl = $query->fromShort($matches[1]);

    if (!$surl) {
      return null;
    }

    // This check is best-effort to allow all older entries to continue
    // working even if they don't host-parse.
    // If it's whitelisted, don't even bother checking dnsbl
    if (!$query->isWhitelisted($surl->getLongUrl()) &&
        $query->isBlacklisted($surl->getLongUrl())) {
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
  private function getRedirectResponse($matches, $response) {
    $redir = $this->redirect($matches);
    if ($redir) {
      return $redir;
    }
    return $this->error(404)->finalize($response);
  }

  public function renderText(DaGdTextResponse $response) {
    $matches = $this->getRequest()->getRouteMatches();
    if ($matches[1]) {
      return $this->getRedirectResponse($matches, $response);
    }
  }

  public function render(DaGdHTMLResponse $response) {
    $matches = $this->getRequest()->getRouteMatches();
    if ($matches[1]) {
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
