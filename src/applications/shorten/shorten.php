<?php

require_once dirname(__FILE__).'/resources/random_string.php';

class DaGdShortenController extends DaGdBaseClass {

  protected $wrap_pre = false;

  private $long_url;
  private $short_url;

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

  private function getLongURL() {
    $query = $this->db_connection->prepare(
      'SELECT longurl FROM shorturls WHERE shorturl=?');
    $query->bind_param('s', $this->route_matches[1]);
    $query->execute();
    $query->bind_result($longurl);
    $query->fetch();
    $query->close();

    return $longurl;
  }

  public function render() {
    if ($_SERVER['REQUEST_METHOD'] == 'GET') {
      if (count($this->route_matches) > 1) {
        // Attempt to access a stored URL
        $long_url = $this->getLongURL();
        if ($long_url) {
          header('X-Original-URL: '.$long_url);
          header('Location: '.$long_url);
        } else {
          error404();
          return;
        }
      } else {
        $content = '<form method="POST" action="">
Long URL: <input type="text" name="url" size="35" /><br />
Optional custom suffix (truncated at 10 chars): <input type="text" name="shorturl" size="20" /><br />
<input type="submit" value="Shorten URL" />';
        $markup = new DaGdMarkup($content);
        echo $markup->render();
      }
    } else { // POST

      // TODO: Break this into its own function.
      if (array_key_exists('shorturl', $_POST) &&
        strlen($_POST['shorturl']) > 0) {
        $valid_char_pattern = '@^[\d\w]+$@i';
        if (!preg_match($valid_char_pattern, $_POST['shorturl'])) {
          error400('Invalid short URL entered. Alphanumeric only, please.');
          return;
        } else {
          $this->short_url = substr($_POST['shorturl'], 0, 10);
          if (!$this->isFreeShortURL()) {
            error400('That custom URL was already taken, go back and try again!');
            return;
          }
        }
      } else {
        $this->short_url = randstr(rand(4, 5));
        while (!$this->isFreeShortURL()) {
          $this->short_url = randstr(4, 5);
        }
      }

      $this->short_url = htmlspecialchars($this->short_url);
      
      if (array_key_exists('url', $_POST) && strlen($_POST['url']) > 0) {
        // Something has at least been submitted. Is it valid?
        if (preg_match('@^https?://@', $_POST['url'])) {
          // Good enough for now...probably needs some better checks.

          $this->long_url = $_POST['url'];
          $this->long_url = htmlspecialchars($this->long_url);
          
          $query = $this->db_connection->prepare(
            'INSERT INTO shorturls(shorturl, longurl, owner_ip) '.
            'VALUES(?, ?, ?);');
          $query->bind_param(
            'sss',
            $this->short_url,
            $this->long_url,
            $_SERVER['REMOTE_ADDR']);

          if ($query->execute()) {
            header('X-Short-URL: '.$this->short_url);
            $this->escape = false;
            $new_link = DaGdConfig::get('general.baseurl').'/'.$this->short_url;
            return '<a href="'.$new_link.'">'.$new_link.'</a>';
          } else {
            error400('Something has gone wrong! :( ... Try again? Please?');
            return;
          }
          
        } else {
          error400(
            'Malformed original URL. Try again (http or https '.
              'protocols only, please.).');
          return;
        }
      } else {
        error400('Error: Cannot create something out of nothing.');
        return;
      }
    }
  }
}