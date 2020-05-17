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
          ->addGetArg('text', 'the text to render (mind url encoding)')
          ->addGetArg('eyes', 'substitution for eyes')
          ->addGetArg('tongue', 'substitution for tongue')
          ->addGetArg('thoughts', 'substitution for line to text bubble')
          ->addExample(
            id(new DaGdHelpExample())
              ->addGetArg('text', 'moo')
              ->setCommentary('Generate a cow that says moo'))
          ->addExample(
            id(new DaGdHelpExample())
              ->addGetArg('cow', 'moose')
              ->addGetArg('text', 'moo')
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
    $text = $this->getRequest()->getParamOrDefault('text');
    $eyes = $this->getRequest()->getParamOrDefault('eyes');
    $thoughts = $this->getRequest()->getParamOrDefault('thoughts');
    $tongue = $this->getRequest()->getParamOrDefault('tongue');

    if (!$text) {
      $response->setCode(400);
      return 'What do you want your cow to say? (Hint: Pass ?text=)';
    }

    $cs = new Cowsay();
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
