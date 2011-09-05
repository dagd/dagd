<?php

class DaGdEditCountController extends DaGdBaseClass {
  public static $__help__ = array(
    'summary' => 'Gives wikipedia edit count for a user.',
    'path' => 'ec',
    'examples' => array(
      array(
        'arguments' => array('Phuzion'),
        'summary' => 'en.wikipedia.org editcount for user "Phuzion"'),
      array(
        'arguments' => array('Phuzion'),
        'request' => array(
          'lang' => 'fr'),
        'summary' => 'fr.wikipedia.org editcount for user "Phuzion"'),
      array(
        'arguments' => array('Phuzion'),
        'request' => array(
          'lang' => 'en',
          'proj' => 'wikiquote'),
        'summary' => 'en.wikiquote.org editcount for user "Phuzion"'),  
    ));
    
  public function render() {
    $query = $this->route_matches[1];

    // Default to english (en).
    $language = request_or_default('lang', 'en');
    if (!preg_match('/^[/a-z]+$/i', $language)) {
      error400('`lang` should only contain letters.');
      return;
    }
    
    // Default to wikipedia.org.
    $project = request_or_default('proj', 'wikipedia.org');
    if (!in_array($project, $wmprojects)) {
        error400('`proj` needs to be a valid Wikimedia project.');
        return;
    }

    $wmprojects = array(
      "wikipedia",
      "wiktionary",
      "wikisource",
      "wikiversity",
      "wikibooks",
      "wikiquote",
      "wikinews");

   // if (!count(dns_get_record($language.'.wikipedia.org'))) {
   //     error400($language.'.wikipedia.org is not a valid hostname.');
   //     return;
   // }
    
    $counts = file_get_contents(
      'http://'.$language.'.'.$project.'.org/w/api.php?action=query&list=users'.
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
