<?php

require_once dirname(__FILE__).'/resources/random_string.php';
require_once dirname(__FILE__).'/coshorten.php';

final class DaGdShortenController extends DaGdBaseClass {
  public $__help__ = array(
    'title' => 'shorten',
    'summary' => 'Shorten your long URLs (/, /s, /shorten).',
    'path' => 's',
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

  protected $wrap_html = true;
  protected $wrap_pre = false;
  protected $style = 'body {
  margin: 0; padding: 0;
  border-top: 5px solid #4ab3ee;
  padding: 10px;
}
h2 { margin: 0; padding: 0; }';

  private $long_url;
  private $longurl_hash;
  private $short_url;
  private $stored_url_id;
  private $custom_url = false;
  private $store_url = true;

  private function isFreeShortURL() {
    $query = $this->db_connection->prepare(
      'SELECT COUNT(*) FROM shorturls WHERE shorturl=?');
    $query->bind_param('s', $this->short_url);
    $query->execute();
    $query->bind_result($count);
    $query->fetch();
    $query->close();

    // If we do *NOT* get one above (!), then it is free, and we return true.
    return !(bool)$count;
  }

  private function blacklisted($url) {
    $blacklist_list = DaGdConfig::get('shorten.longurl_blacklist');
    foreach ($blacklist_list as $regex) {
      if (preg_match('#'.$regex.'#i', $url)) {
        return true;
      }
    }
    return false;
  }

  public function getLongURL($shorturl) {
    $query = $this->db_connection->prepare(
      'SELECT id,longurl FROM shorturls WHERE shorturl=? AND enabled=1');
    $query->bind_param('s', $shorturl);
    $query->execute();
    $query->bind_result($this->stored_url_id, $this->long_url);
    $query->fetch();
    $query->close();
    return $this->long_url;
  }

  private function getNonCustomShortURL($longurl_hash) {
    $query = $this->db_connection->prepare(
      'SELECT id,shorturl FROM shorturls WHERE longurl_hash=? AND enabled=1 '.
      'AND custom_shorturl=0 ORDER BY id DESC LIMIT 1');
    $query->bind_param('s', $longurl_hash);
    $query->execute();
    $query->bind_result($this->stored_url_id, $this->short_url);
    $query->fetch();
    $query->close();
    return;
  }

  private function logURLAccess() {
    $query = $this->db_connection->prepare(
      'INSERT INTO shorturl_access(shorturl_id, ip, useragent) VALUES(?,?,?)');
    $stored_url_id = $this->stored_url_id;
    $client_ip = client_ip();
    $useragent = $_SERVER['HTTP_USER_AGENT'];
    $query->bind_param(
      'iss',
      $stored_url_id,
      $client_ip,
      $useragent);
    if ($query->execute()) {
      return true;
    } else {
      return false;
    }
  }

  private function redirect_from_shorturl() {
    $this->getLongURL($this->route_matches[1]);
    if ($this->long_url) {
      $this->logURLAccess();
      header('X-Original-URL: '.$this->long_url);
      $qs = build_given_querystring();
      if ($this->route_matches[2]) {
        header('Location: '.$this->long_url.'/'.$this->route_matches[2].$qs);
      } else {
        header('Location: '.$this->long_url.$qs);
      }
      return true;
    } else {
      error404();
      return false;
    }
  }

  private function store_shorturl() {
    if (!$this->store_url) {
      return true;
    }
    $query = $this->db_connection->prepare(
      'INSERT INTO shorturls(shorturl, longurl, owner_ip, custom_shorturl, '.
      'longurl_hash) VALUES(?, ?, ?, ?, ?);');
    $query->bind_param(
      'sssis',
      $this->short_url,
      $this->long_url,
      client_ip(),
      $this->custom_url,
      $this->longurl_hash);

    if ($query->execute()) {
      return true;
    } else {
      error500('Something has gone wrong! :( ... Try again? Please?');
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
    } else {
      $this->longurl_hash = hash('sha256', $this->long_url);
      $this->getNonCustomShortURL($this->longurl_hash);
      if ($this->short_url) {
        $this->store_url = false;
      } else {
        $this->short_url = randstr(rand(4, 5));
        while (!$this->isFreeShortURL()) {
          debug('Hash collision', 'Calling randstr again');
          $this->short_url = randstr(4, 5);
        }
      }
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
      if (preg_match('@^https?://@', $long_url) &&
          !$this->blacklisted($long_url)) {
        // Good enough for now...probably needs some better checks.
        $this->long_url = $long_url;
        return true;
      } else {
        error400(
          'Malformed or blacklisted original URL. Try again (http or https '.
          'protocols only, please.).');
        return false;
      }
    } else {
      return false;
    }
  }

  public function render() {
    if (array_key_exists('url', $_REQUEST)) {
      if ($this->set_longurl_or_400() && $this->set_shorturl_or_400()) {
        if ($this->store_shorturl()) {
          header('X-Short-URL: '.$this->short_url);
          $this->escape = false;
          $new_link = DaGdConfig::get('general.baseurl').'/'.$this->short_url;
          return '<a href="'.$new_link.'">'.$new_link.'</a>';
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
        return help('DaGdShortenController');
      }

      // Not a text useragent because we didn't return above.
      // Bring in the form. // TODO: html in strings = bad.
      $this->escape = false;
      $content = '<h2>da.gd</h2><form method="POST" action="/">
Long URL: <input type="text" name="url" id="url" size="35" autofocus /><br />
Custom short URL (leave blank for random): <input type="text" name="shorturl" size="20" maxlength="10" /><br />
<input type="submit" value="Shorten URL" />
</form>
[help](/help) | [open source](http://github.com/codeblock/dagd)';
      $markup = new DaGdMarkup($content);
      $markup = $markup->render();
      $markup .= '<script>window.onload = function() {document.getElementById("url").focus();}</script>';
      return $markup;
    }
  }
}
