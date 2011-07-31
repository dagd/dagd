<?php
function require_application($name) {
  require_once dirname(dirname(__FILE__)).'/applications/'.$name.'/'.$name.'.php';
  return;
}