<?php

abstract class DaGdException extends Exception {
  abstract public function getPublicMessage();
  abstract public function getPrivateMessage();
}
