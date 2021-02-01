<?php

final class DaGdUnitTestAssertDaGdSessionBasics extends DaGdUnitTest {
  public function runUnits() {
    $this->path = 'DaGdUnitTestAssertDaGdSessionBasics';

    $session = new DaGdSession();
    $session->set('foo', 'bar');
    $session->set('hey', 'hi');
    $encrypted = $session->emit();
    $first = $encrypted['DaGdSession_0'];
    $new_session = id(new DaGdSession())
      ->loadFromCookies($first);

    $this->assertTrue(
      $new_session->get('foo') === 'bar',
      'set-get returns expected');

    $this->assertTrue(
      $new_session->get('hey') === 'hi',
      'set-get returns expected');

    $session->remove('foo');

    // Re-emit the session
    $encrypted = $session->emit();
    $first = $encrypted['DaGdSession_0'];
    $new_session = id(new DaGdSession())
      ->loadFromCookies($first);

    $this->assertTrue(
      $new_session->get('foo', 'does not exist') === 'does not exist',
      'get with default returns expected');
  }
}
