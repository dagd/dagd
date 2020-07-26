<?php

/**
 * This controller is special because it maintains its own db handlers and
 * interacts with multiple distinct databases. It does not use the pair of
 * handlers passed in from framework entry.
 */
final class DaGdHamController extends DaGdController {
  public static function getHelp() {
    return id(new DaGdHelp())
      ->setTitle('callsign')
      ->setDescription('Lookup US ham radio callsigns.')
      ->addPath(
        id(new DaGdHelpPath())
          ->setPath('ham')
          ->addPathArg('callsign', 'the callsign to query')
          ->setMethods(array('GET')));
  }

  public function template() {
    $call = tag('h1', 'W1AW');

    $card = id(new DaGdCard())
      ->setTitle('Operator Basic Details')
      ->setBody(
        tag(
          'pre',
          "ARRL HQ OPERATORS CLUB\n225 MAIN ST\nNEWINGTON, CT 06111"))
      ->addClass('flex-1')
      ->addClass('mr5');

    $card2 = id(new DaGdCard())
      ->setTitle('Clubs Trusteed')
      ->setBody(
        tag(
          'pre',
          "FAKE DATA HERE\nFAKE NEWS LOL\nNEWINGTON, CT 06111"))
      ->addClass('flex-1')
      ->addClass('ml5');

    $card3 = id(new DaGdCard())
      ->setTitle('Administrative History')
      ->setBody(
        tag(
          'pre',
          "lol flex grids are fun and DaGdCard is funnerer"))
      ->addClass('flex-1');

    // TODO: Make this this a class, DaGdFlexGrid
    $flexgrid = tag(
      'div',
      array(
        $card->toTag(),
        $card2->toTag(),
      ),
      array(
        'style' => 'display: flex;',
        'class' => 'mb10',
      )
    );

    $flex2 = tag(
      'div',
      array(
        $card3->toTag(),
      ),
      array(
        'style' => 'display: flex;',
        'class' => 'mt10',
      )
    );

    return array(
      $call,
      $flexgrid,
      $flex2,
    );
  }

  public function render(DaGdHTMLResponse $response) {
    $callsign = $this->getRequest()->getRouteComponent('callsign');
    if (!$callsign) {
      return $this->error(404)->finalize();
    }

    //$query = new DaGdHamQuery($this);
    //$uls = $query->uls($callsign);
    $info = $this->template();

    $template = $this
      ->getBaseTemplate()
      ->setBody($info)
      ->getHtmlTag();
    return $response->setBody($template);
  }

  // TODO
  public function execute(DaGdResponse $response) {
    return 'Unimplemented';
  }
}
