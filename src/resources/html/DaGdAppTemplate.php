<?php

class DaGdAppTemplate extends DaGdTemplate {
  public function getTitleTag() {
    return tag('title', 'da.gd: '.$this->getTitle());
  }
}
