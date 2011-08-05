<?php

class DaGdWPEditController extends DaGdBaseClass {
  private $query;
  
  public function __construct($path, $query) {
    $this->query = $query;
  }
    
  public function render() {
    $counts = file_get_contents(
      'http://en.wikipedia.org/w/api.php?action=query&list=users'.
      '&usprop=editcount&format=json&ususers='.urlencode($this->query));
    $json_counts = json_decode($counts, true);
    $json_counts = $json_counts['query']['users'];
    $total_edits = 0;
    foreach ($json_counts as $user) {
      $total_edits += (int)$user['editcount'];
    }
    return $total_edits;
  }
}