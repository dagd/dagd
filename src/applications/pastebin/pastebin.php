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

  public function render() {
    return 'Coming soon!';
  }
}
