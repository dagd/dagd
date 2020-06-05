#!/usr/bin/env php
<?php

// Activate the autoloader
require_once(dirname(dirname(__FILE__)).'/src/resources/global_resources.php');

final class TestCLI extends DaGdCLIProgram {
  function run() {
    parent::run();
  }
}

$testcli = new TestCLI();
$testcli->setName('dagd-test');
$testcli->setDescription('Experimental da.gd test suite runner - NOT USEFUL YET');
$testcli->addParameter(
  id(new DaGdCLIFlag)
    ->setName('--verbose')
    ->setShortname('-v')
    ->setDescription('Display extra debugging information during tests'));
$testcli->addParameter(
  id(new DaGdCLIFlag)
    ->setName('--no-color')
    ->setShortname('-C')
    ->setDescription('Disable ANSI terminal colors'));
$testcli->addParameter(
  id(new DaGdCLIArgument)
    ->setName('--groups')
    ->setShortname('-g')
    ->setDescription('A comma-separated list of test groups to limit to')
    ->setRequired(false));
$testcli->addParameter(
  id(new DaGdCLIFlag)
    ->setName('--debug-args')
    ->setShortname('-D')
    ->setDescription('Debug argument parsing'));
$testcli->addParameter(
  id(new DaGdCLIFlag)
    ->setName('--help')
    ->setShortname('-h')
    ->setDescription('Show program help'));
//$testcli->showUsage();
$testcli->parseArgs($argv);
$testcli->run();
