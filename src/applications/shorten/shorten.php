<?php

class DaGdShortenController extends DaGdBaseClass {

  public function render() {
    if ($_SERVER['REQUEST_METHOD'] == 'GET') {
      $content = '<form method="POST" action="">
Long URL: <input type="text" name="longurl" size="35" /><br />
Optional custom suffix: <input type="text" name="customurl" size="20" /><br />
<input type="submit" value="Shorten URL" />';
      $markup = new DaGdMarkup($content);
      echo $markup->render();
    } else { // POST
      return $_POST['longurl'];
    }
  }
}