<?php

final class DaGdEditCountController extends DaGdController {
  public function getHelp() {
    return array(
      'title' => 'editcount',
      'summary' => 'Gives wikipedia edit count for a user.',
      'path' => 'ec',
      'examples' => array(
        array(
          'arguments' => array('CodeBlock'),
          'summary' => 'en.wikipedia.org editcount for user "CodeBlock"'),
        array(
          'arguments' => array('CodeBlock'),
          'request' => array(
            'lang' => 'fr'),
          'summary' => 'fr.wikipedia.org editcount for user "CodeBlock"'),
        array(
          'arguments' => array('CodeBlock'),
          'request' => array(
            'lang' => 'en',
            'proj' => 'wikiquote'),
          'summary' => 'en.wikiquote.org editcount for user "CodeBlock"'),
      ));
  }

  public function execute(DaGdResponse $response) {
    $query = $this->getRequest()->getRouteComponent('username');

    // Default to english (en).
    $language = $this->getRequest()->getParamOrDefault('lang', 'en');
    if (!preg_match('@^[a-z]+$@i', $language)) {
      $response->setCode(400);
      return '`lang` should only contain letters.';
      return;
    }

     $wmprojects = array(
      "wikipedia",
      "wiktionary",
      "wikisource",
      "wikiversity",
      "wikibooks",
      "wikiquote",
      "wikinews",
     );

    // Default to wikipedia.org.
   $project = $this->getRequest()->getParamOrDefault('proj', 'wikipedia');
    if (!in_array($project, $wmprojects)) {
      $response->setCode(400);
      return '`proj` needs to be a valid Wikimedia project.';
    }

   if (!count(dns_get_record($language.'.'.$project.'.org'))) {
     $response->setCode(400);
     return $language.'.'.$project.'.org is not a valid wikipedia subdomain.';
   }

    $counts = file_get_contents(
      'https://'.$language.'.'.$project.'.org/w/api.php?action=query&list=users'.
      '&usprop=editcount&format=json&ususers='.urlencode($query));
    $json_counts = json_decode($counts, true);
    $json_counts = $json_counts['query']['users'];
    $total_edits = 0;
    foreach ($json_counts as $user) {
      $total_edits += (int)$user['editcount'];
    }
    return (string)$total_edits;
  }
}
