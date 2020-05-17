<?php

final class DaGdEditCountController extends DaGdController {
  public static function getHelp() {
    return id(new DaGdHelp())
      ->setTitle('editcount')
      ->setDescription('Gives wikipedia edit count for a user.')
      ->addPath(
        id(new DaGdHelpPath())
          ->setPath('ec')
          ->addPathArg('username')
          ->addGetArg('lang', 'language code for wikimedia project')
          ->addGetArg('proj', 'wikimedia project to query')
          ->setMethods(array('GET'))
          ->addExample(
            id(new DaGdHelpExample())
              ->addPathArg('CodeBlock')
              ->setCommentary(
                'en.wikipedia.org editcount for user "CodeBlock"'))
          ->addExample(
            id(new DaGdHelpExample())
              ->addPathArg('CodeBlock')
              ->addGetArg('lang', 'fr')
              ->setCommentary(
                'fr.wikipedia.org editcount for user "CodeBlock"'))
          ->addExample(
            id(new DaGdHelpExample())
              ->addPathArg('CodeBlock')
              ->addGetArg('lang', 'en')
              ->addGetArg('proj', 'wikiquote')
              ->setCommentary(
                'en.wikiquote.org editcount for user "CodeBlock"')));
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
