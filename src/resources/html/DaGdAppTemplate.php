<?php

class DaGdAppTemplate extends DaGdTemplate {
  public function getTitle() {
    return 'da.gd: '.parent::getTitle();
  }
}
