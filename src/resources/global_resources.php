<?php

// This is essentially stolen from Facebook's libphutil, but it allows
// for doing: id(new Foo())->bar();  ... whereas (new Foo())->bar() is invalid
// in php.
function id($obj) {
  return $obj;
}

function handle_exception(Exception $e) {
  echo 'An error has occurred within dagd! Sorry about that!';
  header('HTTP/1.0 500 Internal Server Error');
  die();
}
set_exception_handler('handle_exception');