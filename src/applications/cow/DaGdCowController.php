<?php
final class DaGdCowController extends DaGdController {
  public function getHelp() {
    return array(
      'title' => 'cow',
      'summary' => 'Generate cows and other cow-like things.',
      'path' => 'cow',
      'examples' => array(
        array(
          'summary' => 'Generate a cow that says moo',
          'request' => array(
            'text' => 'moo',
          ),
        ),
        array(
          'summary' => 'Generate a moose with custom attributes',
          'request' => array(
            'cow' => 'moose',
            'text' => 'moo',
            'eyes' => '@@',
            'thoughts' => '^',
            'tongue' => '~',
          ),
        ),
      ),
    );
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
