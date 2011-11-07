<?php

require_once dirname(__FILE__).'/resources/random_string.php';

final class DaGdShortenController extends DaGdBaseClass {
  public static $__help__ = array(
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

  protected $wrap_pre = false;

  private $long_url;
  private $short_url;
  private $stored_url_id;

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

  private function getLongURL($longurl) {
    $query = $this->db_connection->prepare(
      'SELECT id,longurl FROM shorturls WHERE shorturl=?');
    $query->bind_param('s', $longurl);
    $query->execute();
    $query->bind_result($this->stored_url_id, $this->long_url);
    $query->fetch();
    $query->close();
    return;
  }

  private function logURLAccess() {
    $query = $this->db_connection->prepare(
      'INSERT INTO shorturl_access(shorturl_id, ip, useragent) VALUES(?,?,?)');
    $query->bind_param(
      'iss',
      $this->stored_url_id,
      $_SERVER['REMOTE_ADDR'],
      $_SERVER['HTTP_USER_AGENT']);
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
      header('Location: '.$this->long_url);
      return true;
    } else {
      error404();
      return false;

    }
  }

  private function store_shorturl() {
    $query = $this->db_connection->prepare(
      'INSERT INTO shorturls(shorturl, longurl, owner_ip) '.
      'VALUES(?, ?, ?);');
    $query->bind_param(
      'sss',
      $this->short_url,
      $this->long_url,
      $_SERVER['REMOTE_ADDR']);
    
    if ($query->execute()) {
      return true;
    } else {
      error400('Something has gone wrong! :( ... Try again? Please?');
      return false;
    }
  }

  
  private function set_shorturl_or_400() {
    if ($short_url = request_or_default('shorturl')) {
      $valid_char_pattern = '@^[\d\w-_]+$@i';
      if (!preg_match($valid_char_pattern, $short_url)) {
        error400('Invalid short URL entered. Alphanumeric only, please.');
        return false;
      } else {
        $this->short_url = substr($short_url, 0, 10);
        if (!$this->isFreeShortURL()) {
          error400('That custom URL was already taken, go back and try again!');
          return false;
        }
      }
    } else {
      $this->short_url = randstr(rand(4, 5));
      while (!$this->isFreeShortURL()) {
        $this->short_url = randstr(4, 5);
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
      if (preg_match('@^https?://@', $long_url)) {
        // Good enough for now...probably needs some better checks.
        $this->long_url = $long_url;
        return true;
      } else {
        error400(
          'Malformed original URL. Try again (http or https '.
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
    if (count($this->route_matches) > 1) {
      if (end($this->route_matches) == 'original') {
        $this->escape = false;
        $this->getLongURL($this->route_matches[1]);
        return '<a href="'.$this->long_url.'">'.$this->long_url.'</a>';
      } else {
        // Attempt to access a stored URL
        $this->redirect_from_shorturl();
        return;
      }
    } else {
      // We are not attempting to access a stored URL, but we also don't have
      // a 'url' - Show the form so that we can create a new short-url.
      if (is_text_useragent()) {
        // No use in showing a form for text UAs. Rather, show help text.
        return $this->help();
      }

      // Not a text useragent because we didn't return above.
      // Bring in the form. // TODO: html in strings = bad.
      $content = '***da.gd***
<form method="POST" action="">
Long URL: <input type="text" name="url" id="url" size="35" /><br />
Optional custom suffix (truncated at 10 chars): <input type="text" name="shorturl" size="20" /><br />
<input type="submit" value="Shorten URL" /><br />
[help](/help) | [open source](http://github.com/codeblock/dagd)';
      $markup = new DaGdMarkup($content);
      $markup = $markup->render();
      $markup .= '<script>window.onload = function() {document.getElementById("url").focus();}</script>';
      echo $markup;
    }
  }
}
