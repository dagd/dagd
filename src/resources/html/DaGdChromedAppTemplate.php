<?php

class DaGdChromedAppTemplate extends DaGdAppTemplate {
  public function getStyle() {
    $style = <<<EOD
#bar {
  padding: 15px 0;
  min-height: 65px;
  font-family: "Proxima Nova", overpass, Ubuntu, sans-serif !important;
  font-weight: 300;
  font-size: 1.7em;
  margin-bottom: 10px;
}
.constraint { width: 85%; margin: 0 auto; max-width: 1080px; }
#bar .constraint {
  display: flex;
  align-items: center;
  justify-content: space-between;
}
.brand-name { line-height: 1.1; }
#bar a, #bar a:active, #bar a:visited { color: #3a9; text-decoration: none; }
#bar a:hover { color: #9C89B8; }
input[type=text] { border: 1px solid #ccc; }
.sitename { color: #333; font-weight: 500; }
.lightmode .sitename { color: #3a9; }
.darkmode .sitename { color: #39a; }
.appname {
  font-family: "SF Mono", "JetBrains Mono", "Fira Code", Menlo, Consolas, monospace;
  font-size: 0.9em;
  font-weight: 400;
  color: #888;
  margin-left: 6px;
  letter-spacing: -0.02em;
}
.appname-dot { color: #9C89B8; margin-right: 6px; font-weight: 700; }
.darkmode #bar { border-color: #555; }
.navlinks { display: flex; align-items: center; }
#bar a.navlink, #bar a.navlink:active, #bar a.navlink:visited {
  display: inline-flex;
  flex-direction: column;
  align-items: center;
  text-align: center;
  vertical-align: middle;
  line-height: 1.2;
  font-size: 14px;
  margin-left: 20px;
}
.navlink svg { display: block; }
.navlabel { margin-top: 3px; }
.tagline {
  color: #888;
  font-weight: 300;
  font-style: italic;
  font-size: 0.62em;
  margin-top: 3px;
  line-height: 1.2;
}
.flex-1 { flex: 1; }
.ml5 { margin-left: 5px; }
.mr5 { margin-right: 5px; }
.mt10 { margin-top: 10px; }
.mb10 { margin-bottom: 10px; }
.ow-bw { overflow-wrap: break-word; }
.b { font-weight: bold; }
body.darkmode { background-color: #1a1825; color: #aaa; }
body.lightmode { background-color: #f4f3f8; }
body.darkmode a, body.darkmode a:active, body.darkmode a:visited { color: #ccc; }
body, .sitename { margin: 0; padding: 0; }
#app { font-family: "Proxima Nova", overpass, Ubuntu, sans-serif; clear: both; box-sizing: border-box; }
.card {
  box-shadow: 1px 1px 2px #555;
}
.card .card-title, .card .card-body, .card .card-footer { padding: 10px; }
body.lightmode .card { background-color: #fff; }
.card .card-title { background-color: #9C89B8; color: #422040; }
body.darkmode .card { background-color: #888; }
.alert { width: 100%; padding: 1px 10px; margin: 5px 0; box-sizing: border-box; border-radius: 3px; }
.lightmode .alert-warning { background-color: #ffd27f; }
.darkmode .alert-warning { background-color: #aaa27f; }
.lightmode .alert-success { background-color: #88d27f; }
.darkmode .alert-success { background-color: #77a27f; }
.lightmode .alert-failure { background-color: #ff828f; }
.darkmode .alert-failure { background-color: #a7727f; }
.lightmode .alert-fatal { background-color: #ff4344; }
.darkmode .alert-fatal { background-color: #a7323f; }
.lightmode .alert-info { background-color: #88d2ff; }
.darkmode .alert-info { background-color: #628aaf; }
EOD;
    return array_merge(parent::getStyle(), array($style));
  }

  /**
   * Icons in this file are from Feather (https://feathericons.com/), taken
   * from a revision predating commit
   * 044aff80341bef1a228053b9e7810ef45c7acb4b, at which point the project still
   * granted use without an attribution/license requirement. We attribute them
   * here voluntarily. Each icon is the SVG body of the like-named Feather icon.
   */
  private function featherIcon($name) {
    switch ($name) {
      case 'help-circle':
        $body =
          '<circle cx="12" cy="12" r="10"></circle>'.
          '<path d="M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3"></path>'.
          '<line x1="12" y1="17" x2="12.01" y2="17"></line>';
        break;
      case 'code':
        $body =
          '<polyline points="16 18 22 12 16 6"></polyline>'.
          '<polyline points="8 6 2 12 8 18"></polyline>';
        break;
      case 'moon':
        $body =
          '<path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"></path>';
        break;
      case 'sun':
        $body =
          '<circle cx="12" cy="12" r="5"></circle>'.
          '<line x1="12" y1="1" x2="12" y2="3"></line>'.
          '<line x1="12" y1="21" x2="12" y2="23"></line>'.
          '<line x1="4.22" y1="4.22" x2="5.64" y2="5.64"></line>'.
          '<line x1="18.36" y1="18.36" x2="19.78" y2="19.78"></line>'.
          '<line x1="1" y1="12" x2="3" y2="12"></line>'.
          '<line x1="21" y1="12" x2="23" y2="12"></line>'.
          '<line x1="4.22" y1="19.78" x2="5.64" y2="18.36"></line>'.
          '<line x1="18.36" y1="5.64" x2="19.78" y2="4.22"></line>';
        break;
      case 'coffee':
        $body =
          '<path d="M18 8h1a4 4 0 0 1 0 8h-1"></path>'.
          '<path d="M2 8h16v9a4 4 0 0 1-4 4H6a4 4 0 0 1-4-4V8z"></path>'.
          '<line x1="6" y1="1" x2="6" y2="4"></line>'.
          '<line x1="10" y1="1" x2="10" y2="4"></line>'.
          '<line x1="14" y1="1" x2="14" y2="4"></line>';
        break;
      default:
        throw new Exception('Unknown feather icon: '.$name);
    }

    return
      '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" '.
      'viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" '.
      'stroke-linecap="round" stroke-linejoin="round">'.$body.'</svg>';
  }

  private function navLink($href, $label, $icon_name) {
    // The icon is a raw SVG string, so render the anchor as CDATA (unescaped).
    $body =
      $this->featherIcon($icon_name).
      tag('span', $label, array('class' => 'navlabel'))->renderSafe();

    return tag(
      'a',
      $body,
      array(
        'href' => $href,
        'class' => 'navlink',
      ),
      true
    );
  }

  protected function getLinksArray() {
    $darkmode = $this->getDarkmode();

    $links = array(
      $this->navLink('/help', 'help', 'help-circle'),
      $this->navLink(
        'https://codeberg.org/dagd/dagd',
        'open source',
        'code'
      ),
      $this->navLink(
        $darkmode ? '?darkmode=0' : '?darkmode=1',
        $darkmode ? 'light mode' : 'dark mode',
        $darkmode ? 'sun' : 'moon'
      ),
      $this->navLink('https://buymeacoffee.com/relrod', 'donate', 'coffee'),
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
      $out[] = tag(
        'span',
        array(
          tag('span', '·', array('class' => 'appname-dot')),
          $title,
        ),
        array(
          'class' => 'appname',
        )
      );
    }
    return $out;
  }

  protected function getChrome() {
    // The brand is a left-hand stack: name + appname on top, tagline beneath.
    $brand_rows = array(
      tag(
        'div',
        $this->getSiteName(),
        array(
          'class' => 'brand-name',
        )
      ),
    );

    $tagline = $this->getTagline();
    if ($tagline) {
      $brand_rows[] = tag(
        'div',
        $tagline,
        array(
          'class' => 'tagline',
        )
      );
    }

    $brand = tag(
      'div',
      $brand_rows,
      array(
        'class' => 'brand',
      )
    );

    $links_div = tag(
      'div',
      $this->getLinksArray(),
      array(
        'class' => 'navlinks',
      )
    );

    $navbar = tag(
      'div',
      array(
        $brand,
        $links_div,
      ),
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
      array(
        $this->getalerts(),
        parent::getBody(),
      ),
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

  public function getFaviconTag() {
    // Completely abuse DaGdTag to make an SVG that we can inline.
    $svg = tag(
      'svg',
      array(
        tag(
          'circle',
          null,
          array(
            'cx' => '32',
            'cy' => '32',
            'r' => '31',
            'fill' => '#333333',
          )
        ),
        tag(
          'text',
          'da.',
          array(
            'x' => '16',
            'y' => '26',
            'font-family' => 'Proxima Nova, sans-serif',
            'font-size' => '24',
            'fill' => '#3399aa',
          )
        ),
        tag(
          'text',
          'gd',
          array(
            'x' => '16',
            'y' => '50',
            'font-family' => 'Proxima Nova, sans-serif',
            'font-size' => '24',
            'fill' => '#3399aa',
          )
        ),
      ),
      array(
        'xmlns' => 'http://www.w3.org/2000/svg',
        'width' => '64',
        'height' => '64',
      )
    )->renderSafe();

    // And then inline it.
    return tag(
      'link',
      null,
      array(
        'rel' => 'icon',
        'href' => 'data:image/svg+xml,'.rawurlencode($svg),
      )
    );
  }
}
