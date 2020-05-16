<?php

final class DaGdCommanderController extends DaGdBaseClass {
  public function getHelp() {
    return array(
      'title' => 'commander',
      'summary' => 'A very simple yubnub replacement.',
      'path' => 'c',
      'examples' => array(
        array(
          'summary' => 'Add a command to the database',
          'arguments' => array(
            'store',
            'g',
            'https://www.google.com/search?q=$PARAMETERS',
          ),
        ),
        array(
          'arguments' => array(
            'g',
            'foobar',
          ),
          'summary' => 'Redirect to https://www.google.com/search?q=foobar'),
      ));
  }

  private $command;
  private $url;

  private function addCommand() {
    $query = $this->getWriteDB()->prepare(
      'INSERT INTO command_redirects(author_ip, command, url) VALUES(?, ?, ?)');
    $query->bind_param(
      'sss',
      client_ip(),
      $this->route_matches[2],
      $this->route_matches[3]);
    if ($query->execute()) {
      return true;
    } else {
      return false;
    }
  }

  private function getURL($command) {
    $query = $this->getReadDB()->prepare(
      'SELECT url FROM command_redirects WHERE command=? AND enabled=1');
    $query->bind_param('s', $command);
    $query->execute();
    $query->bind_result($this->url);
    $query->fetch();
    $query->close();
  }

  private function getAllCommands() {
    $rows = array();
    $result = $this->getReadDB()->query(
      'SELECT command, url, creation_dt FROM command_redirects WHERE '.
      'enabled=1');
    while ($row = $result->fetch_array(MYSQLI_ASSOC)) {
      $rows[] = $row;
    }
    $result->free();
    return $rows;
  }

  public function render() {
    if ($this->route_matches[1] == 'store') {
      // We are storing a command. Do some sanity checks.
      $valid_char_pattern = '@^[\d\w-_]+$@i';
      if (!preg_match($valid_char_pattern, $this->route_matches[2])) {
        error400('Invalid command. Alphanumeric only, please.');
        return false;
      }

      // TODO: might be better to use a unique constraint here, and not check
      // ourselves. That way we only make one query and just report the error
      // back.
      $this->getURL($this->route_matches[2]);
      if ($this->url !== null) {
        error400(
          'That command has already been defined. Try using a new name.');
        return false;
      }
      if ($this->addCommand()) {
        return 'Success.';
      } else {
        error400('Something failed :( ... Try again later.');
        return false;
      }

    } elseif (count($this->route_matches) <= 2) {
      $rows = $this->getAllCommands();
      if (end($this->route_matches) == 'json') {
        $commands = array();
        foreach ($rows as $row) {
          $commands[$row['command']] = $row['url'];
        }
        header('Content-Type: application/json');
        $this->escape = false;
        $this->wrap_pre = false;
        return json_encode($commands);
      } else {
        $return = "<strong>Enabled Commands</strong><dl>\n";

        foreach ($rows as $row) {
          $return .= '<dt>'.htmlspecialchars($row['command'])."</dt>\n";
          $return .= '<dd>   Redirect: '.htmlspecialchars($row['url']).
            "</dd>\n";
          $return .= '<dd>   Added: '.htmlspecialchars($row['creation_dt']).
            "</dd>\n";
        }

        $return .= '</dl>';
        $this->escape = false;
        $this->wrap_pre = false;
        return $return;
      }
    } else {
      // Accessing a command?
      $this->getURL($this->route_matches[1]);
      if ($this->url === null) {
        error400('That command was not found.');
        return false;
      } else {
        $url = str_replace(
          '$PARAMETERS',
          $this->route_matches[2],
          $this->url);
        header('Location: '.$url);
        return true;
      }
    }
  }
}
