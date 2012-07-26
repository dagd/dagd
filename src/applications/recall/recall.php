<?php

final class DaGdRecallController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Recall saved outputs from previous dagd requests.',
    'path' => 'recall',
    'examples' => array(
      array(
        'arguments' => array('myoutput123'),
        'summary' => 'An example of viewing saved output.'),
    ));

  private $output;

  function render() {
    $query = $this->db_connection->prepare(
      'SELECT response FROM dagd.saved_responses where access=?');
    $query->bind_param('s', $this->route_matches[1]);
    $query->execute();
    $query->bind_result($this->output);
    $query->fetch();
    $query->close();

    if ($this->output) {
      return $this->output;
    } else {
      error400('No matching access key was found for saved data.');
      return false;
    }
  }
}
