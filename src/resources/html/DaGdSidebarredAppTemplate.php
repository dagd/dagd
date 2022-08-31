<?php

class DaGdSidebarredAppTemplate extends DaGdAppTemplate {
  private $sidebar;

  public function setSidebar(DaGdSidebar $sidebar) {
    $this->sidebar = $sidebar;
    return $this;
  }

  public function getSidebar() {
    return $this->sidebar;
  }

  public function getStyle() {
    $style = <<<EOD
.sitename { color: #333; float: left; font-weight: 500; }
.lightmode .sitename { color: #3a9; }
.darkmode .sitename { color: #39a; }
.appname { color: #888; }
.darkmode #bar { border-color: #555; }
body.darkmode { background-color: #333; color: #ddd; }
body.lightmode { background-color: #f6f5f4; }
body.darkmode a, body.darkmode a:active, body.darkmode a:visited { color: #ccc; }
body, .sitename { margin: 0; padding: 0; }
#app { font-family: "Proxima Nova", overpass, Ubuntu, sans-serif; clear: both; box-sizing: border-box; }
EOD;
    return array_merge(
      parent::getStyle(),
      array(
        $style,
        $this->getSidebar()->getStyle(),
      )
    );
  }

  protected function getSiteName() {
    $out = array();
    $out[] = tag(
      'a',
      tag('span', 'dagd', array('class' => 'sitename')),
      array(
        'href' => '/',
      )
    );
    $title = $this->getTitle();
    if ($title) {
      $out[] = tag('span', ':'.$title, array('class' => 'appname'));
    }
    return $out;
  }

  protected function getChrome() {
    $sitename = $this->getSiteName();
    $sidebar = tag(
      'div',
      array_merge($sitename, array($links_div)),
      array(
        'class' => 'constraint',
      )
    );

    $navbar_container = tag(
      'div',
      $navbar,
      array(
        'id' => 'bar',
      )
    );

    $constrainted_body = tag(
      'div',
      parent::getBody(),
      array(
        'id' => 'app',
        'class' => 'constraint',
      ),
      // TODO: Nix this when we can
      !$this->getEscape());

    return tag(
      'div',
      array(
        $navbar_container,
        $constrainted_body,
      )
    );
  }

  public function getBody() {
    return $this->getChrome();
  }

}
