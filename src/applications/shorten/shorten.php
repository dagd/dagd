<?php

require_once dirname(__FILE__).'/resources/random_string.php';

class DaGdShortenController extends DaGdBaseClass {

  protected $wrap_pre = false;

  public function render() {
    if ($_SERVER['REQUEST_METHOD'] == 'GET') {
      if (count($this->route_matches) > 1) {
        // Attempt to access a stored URL
        $query = $this->db_connection->prepare(
          'SELECT longurl FROM shorturls WHERE shorturl=?');
        $query->bind_param('s', $this->route_matches[1]);
        $query->execute();
        $query->bind_result($longurl);
        $query->fetch();
        $query->close();
        return $longurl;
      } else {
        $content = '<form method="POST" action="">
Long URL: <input type="text" name="url" size="35" /><br />
Optional custom suffix: <input type="text" name="shorturl" size="20" /><br />
<input type="submit" value="Shorten URL" />';
        $markup = new DaGdMarkup($content);
        echo $markup->render();
      }
    } else { // POST
      $shorturl = null;
      if (array_key_exists('shorturl', $_POST) &&
        strlen($_POST['shorturl']) > 0) {
        $valid_char_pattern = '@^[\d\w]+$@i';
        if (!preg_match($valid_char_pattern, $_POST['shorturl'])) {
          // TODO: Throw a 'bad request' here or something.
          return 'Invalid short URL entered. Alphanumeric only, please.';
        } else {
          $shorturl = $_POST['shorturl'];
        }
      } else {
        $shorturl = randstr(rand(4,5));
      }
      
      if (array_key_exists('url', $_POST) && strlen($_POST['url']) > 0) {
        // Something has at least been submitted. Is it valid?
        if (preg_match('@^https?://@', $_POST['url'])) {
          // Good enough for now...probably needs some better checks.
          $query = $this->db_connection->prepare(
            'INSERT INTO shorturls(shorturl, longurl, owner_ip) '.
            'VALUES(?, ?, ?);');
          $query->bind_param(
            'sss',
            $shorturl,
            $_POST['url'],
            $_SERVER['REMOTE_ADDR']);
          $query->execute();

          // Call me paranoid.....
          $shorturl = htmlspecialchars($shorturl);
          $this->escape = false;
          $new_link = DaGdConfig::get('general.baseurl').'/'.$shorturl;
          return '<a href="'.$new_link.'">'.$new_link.'</a>';
        } else {
          // TODO: 'bad request' here as well.
          return 'Malformed original URL. Try again (http or https protocols only, please.).';
        }
      } else {
        return 'Error: Cannot create something out of nothing.';
      }
    }
  }
}