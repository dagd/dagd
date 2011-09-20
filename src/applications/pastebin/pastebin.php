<?php
class DaGdPastebinController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Paste blurbs of code.',
    'path' => 'paste',
    'examples' => array(
      array(
        'arguments' => array('4'),
        'request' => array(
          'lang' => 'php',
        ),
        'summary' => 'Fetch and show paste ID 4 and highlight it as PHP code.'),
      array(
        'arguments' => array('12'),
        'summary' => 'Fetch and show paste ID 12, with no color highlighting.'),
      array(
        'arguments' => array('7'),
        'request' => array(
          'cli' => '1',
          'lang' => 'php',
        ),
        'summary' => 'Show paste 7, highlighted as PHP with terminal colors'),
    ));

  protected $wrap_pre = false;

  private $paste_id;
  private function logPasteAccess() {
    $query = $this->db_connection->prepare(
      'INSERT INTO pastebin_access(paste_id, ip, useragent) VALUES(?,?,?)');
    $query->bind_param(
      'iss',
      $this->paste_id,
      $_SERVER['REMOTE_ADDR'],
      $_SERVER['HTTP_USER_AGENT']);
    if ($query->execute()) {
      return true;
    } else {
      return false;
    }
  }

  private function create_paste() {
    $query = $this->db_connection->prepare(
      'INSERT INTO pastebin_pastes(ip, text) VALUES(?, ?)');
    $query->bind_param(
      'ss',
      $_SERVER['REMOTE_ADDR'],
      $this->paste_text);
    if ($query->execute()) {
      $this->paste_id = $query->insert_id;
      return true;
    } else {
      return false;
    }
  }

  private function fetch_paste() {
    $query = $this->db_connection->prepare(
      'SELECT text FROM pastebin_pastes WHERE id=?');
    $query->bind_param('i', $this->paste_id);
    $query->execute();
    $query->bind_result($this->paste_text);
    $query->fetch();
    $query->close();
    return;
  }

  private function generate_link() {
    $link = DaGdConfig::get('general.baseurl').'/p/'.$this->paste_id;
    return '<a href="'.$link.'">'.$link.'</a>';
  }

  public function render() {
    if ($paste_text = request_or_default('text')) {
      // A paste is being submitted.
      $this->paste_text = $paste_text;
      $this->create_paste();
      echo $this->generate_link();
      return;
    } else {
      // Trying to access one?
      if (count($this->route_matches) > 1) {
        // Yes
        $this->paste_id = $this->route_matches[1];
        $this->fetch_paste();
        if ($this->paste_text) {
          return $this->paste_text;
        } else {
          error404();
          return;
        }
      } else {
        // No, they're accessing the front page of Pastebin.
        // This is going to need work. :D
        $content = '***Pastebin***
<form method="POST" action="">
<textarea name="text" rows="10" cols="40"></textarea>
<input type="submit" value="Pastebin it!" />
</form>';
        $markup = new DaGdMarkup($content);
        $markup = $markup->render();
        $markup .= '<script>window.onload = function() {document.getElementById("url").focus();}</script>';
        echo $markup;
        return;
      }
    }
  }
}
