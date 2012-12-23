<?php
final class DaGdPastebinController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' =>
      'Paste blurbs of code. This service will be removed in late January 2013.',
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

  private $paste_id;
  private function logPasteAccess() {
    $query = $this->db_connection->prepare(
      'INSERT INTO pastebin_access(paste_id, ip, useragent) VALUES(?,?,?)');
    $query->bind_param(
      'iss',
      $this->paste_id,
      client_ip(),
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
      client_ip(),
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
    if (server_or_default('REQUEST_METHOD') == 'POST') {
      error400(
        'This service has been deprecated, no new pastes are being accepted.');
      return;
    } else {
      // Trying to access one?
      if (count($this->route_matches) > 1) {
        // Yes
        $this->paste_id = $this->route_matches[1];
        $this->fetch_paste();
        if ($this->paste_text) {
          // NEVER EVER EVER EVER EVER EVER EVER remove this header() without
          // changing the lines below it. XSS is bad. :)
          header('Content-type: text/plain; charset=utf-8');
          header('X-Content-Type-Options: nosniff');

          $this->wrap_pre = false;
          $this->escape = false;
          $this->text_html_strip = false;
          $this->text_content_type = false;
          return $this->paste_text;
        } else {
          error404();
          return;
        }
      } else {
        if (is_text_useragent()) {
          // No use in showing a form for text UAs. Rather, show help text.
          return $this->help();
        }

        $content = '
          ***da.gd Pastebin***
          This feature is being deprecated and no new pastes are being accepted.
        ';
        $markup = new DaGdMarkup($content);
        $markup = $markup->render();
        echo $markup;
        return;
      }
    }
  }
}
