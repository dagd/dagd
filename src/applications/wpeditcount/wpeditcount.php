<?php

class DaGdWPEditController extends DaGdBaseClass {
    
  public function render() {
    $query = $this->route_matches[1];
    $counts = file_get_contents(
      'http://en.wikipedia.org/w/api.php?action=query&list=users'.
      '&usprop=editcount&format=json&ususers='.urlencode($query));
    $json_counts = json_decode($counts, true);
    $json_counts = $json_counts['query']['users'];
    $total_edits = 0;
    foreach ($json_counts as $user) {
      $total_edits += (int)$user['editcount'];
    }
    return $total_edits;
  }
}