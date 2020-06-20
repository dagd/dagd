<?php

final class DaGdShortenStoreException extends DaGdShortenException {
  public function getPublicMessage() {
    return 'Failed to store new shorturl';
  }

  public function getPrivateMessage() {
    return 'shorturl INSERT query failed';
  }
}
