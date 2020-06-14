<?php

class DaGdChromedAppTemplate extends DaGdAppTemplate {
  public function getStyle() {
    $style = <<<EOD
#bar {
  padding: 15px;
  height: 65px;
  line-height: 65px;
  border-bottom: 1px solid #eee;
  font-family: "Proxima Nova", overpass, Ubuntu, sans-serif !important;
  font-weight: 300;
  font-size: 1.7em;
  margin-bottom: 10px;
}
.constraint { width: 85%; margin: 0 auto; max-width: 1080px; }
#bar a, #bar a:active, #bar a:visited { color: #ccc; }
table { border-spacing: 30px; border-collapse: separate; }
td { padding: 10px 0; }
input[type=text] { border: 1px solid #ccc; }
.sitename { color: #333; float: left; font-weight: 500; }
.lightmode .sitename { color: #3a9; }
.darkmode .sitename { color: #39a; }
.appname { color: #888; }
.darkmode #bar { border-color: #555; }
body.darkmode { background-color: #333; color: #ddd; }
body.darkmode a, body.darkmode a:active, body.darkmode a:visited { color: #ccc; }
body, .sitename { margin: 0; padding: 0; }
#app { font-family: "Proxima Nova", overpass, Ubuntu, sans-serif; clear: both; }
EOD;
    return array_merge(parent::getStyle(), array($style));
  }

  protected function getLinksArray() {
    $darkmode_link = tag(
      'a',
      $this->getDarkmode() ? 'light mode' : 'dark mode',
      array(
        'href' => $this->getDarkmode() ? '?darkmode=0' : '?darkmode=1',
      )
    );

    $links = array(
      tag('a', 'help', array('href' => '/help')),
      tag(
        'a',
        'open source',
        array('href' => 'https://github.com/dagd/dagd')
      ),
      $darkmode_link,
      tag('a', 'donate', array('href' => 'https://www.patreon.com/relrod')),
    );

    return $links;
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
    $links = intersperse(' / ', $this->getLinksArray());
    $links_div = tag(
      'div',
      $links,
      array(
        'style' => 'float: right; display: inline;',
      )
    );

    $navbar = tag(
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
      !$this->getEscape() // TODO: Nix this when we can
    );

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
