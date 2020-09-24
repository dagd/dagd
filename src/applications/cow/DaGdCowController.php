<?php
final class DaGdCowController extends DaGdController {
  public static function getHelp() {
    return id(new DaGdHelp())
      ->setTitle('cow')
      ->setDescription('Generate cows and other cow-like things.')
      ->addWarning('Be sure to properly URL encode your text')
      ->addPath(
        id(new DaGdHelpPath())
          ->setPath('cow')
          ->setMethods(array('GET'))
          ->addGetArg('cow', 'the cow to render')
          ->addGetArg('say', 'the text to render (mind url encoding)')
          ->addGetArg('eyes', 'substitution for eyes')
          ->addGetArg('tongue', 'substitution for tongue')
          ->addGetArg('thoughts', 'substitution for line to text bubble')
          ->addExample(
            id(new DaGdHelpExample())
              ->addGetArg('say', 'moo')
              ->setCommentary('Generate a cow that says moo'))
          ->addExample(
            id(new DaGdHelpExample())
              ->addGetArg('cow', 'moose')
              ->addGetArg('say', 'moo')
              ->addGetArg('eyes', '@@')
              ->addGetArg('thoughts', '^')
              ->addGetArg('tongue', '~')
              ->setCommentary('Generate a moose with custom attributes')));
  }

  public function renderCow($response) {
    return id(new DaGdTextResponse())
      ->setBody($this->execute($response))
      ->setTrailingNewline(true);
  }

  public function execute(DaGdResponse $response) {
    $cow = $this->getRequest()->getParamOrDefault('cow');
    $eyes = $this->getRequest()->getParamOrDefault('eyes');
    $thoughts = $this->getRequest()->getParamOrDefault('thoughts');
    $tongue = $this->getRequest()->getParamOrDefault('tongue');

    // Look for ?say, so that ?text=0/1 works. Otherwise use ?text for backwards
    // compatibility.
    $text = $this->getRequest()->getParamOrDefault('say');
    if ($text === null) {
      $text = $this->getRequest()->getParamOrDefault('text');
    }

    if (!$text) {
      $response->setCode(400);
      return 'What do you want your cow to say? (Hint: Pass ?text=)';
    }

    $cs = new DaGdCowsay();
    $cs->setMessage($text);

    if ($cow) {
      $cs->setCow($cow);
    }

    if ($eyes) {
      $cs->setEyes($eyes);
    }

    if ($thoughts) {
      $cs->setThoughts($thoughts);
    }

    if ($tongue) {
      $cs->setTongue($tongue);
    }

    return $cs->render();
  }
}
