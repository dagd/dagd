<?php

final class DaGdRollController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'roll',
      'summary' => 'Roll a die or multiple dice.',
      'path' => 'roll',
      'examples' => array(
        array(
          'summary' => 'Roll one 4-sided die',
          'arguments' => array(
            'd4',
          ),
        ),
        array(
          'summary' => 'Roll two 4-sided dice',
          'arguments' => array(
            '2d4',
          ),
        ),
        array(
          'summary' => 'Roll one 4-sided die and add 5 to the result',
          'arguments' => array(
            'd4+5',
          ),
        ),
        array(
          'summary' => 'Roll three 20-sided dice and subtract 1 from the result',
          'arguments' => array(
            '2d20-1',
          ),
        ),
      ));
  }

  public function execute(DaGdResponse $response) {
    $route_matches = $this->getRequest()->getRouteMatches();
    $dice = idx($route_matches, 1);
    $sides = idx($route_matches, 2);
    // NOTE: We interpret ' ' as '+' to work around URL encoding
    $operation = idx($route_matches, 3, '+');
    $op_argument = idx($route_matches, 4, 0);

    $result = 0;
    $add = intval($op_argument);

    if ($dice === '') {
      $dice = 1;
    }

    // The route will force this to be space, +, or -.
    // Space is +, and our default case, so we just handle - here.
    if ($operation === '-') {
      $add = $add * -1;
    }

    for ($i = 0; $i < $dice; $i++) {
      $result += rand(1, $sides);
    }

    $result += $add;
    return $result;
  }
}
