<?php
class DaGdAboutUsController extends DaGdBaseClass {
  public function render() {

    // We use html below.
    $this->escape = false;
    $this->auto_adapt = false;
    
    $content = <<<HTML
      da.gd is an open source collection of PHP applications which
      give information about networking, IPs and domains.<br /><br />

      The source of dagd is located on
      <a href="http://github.com/codeblock/dagd">Github</a>, and is fairly
      easy to hack on, if you read a bit.<br /><br />

      Current features include:<br />
      Whois (<a href="/w/google.com">/w/google.com</a>,
      <a href="/w/127.0.0.1">/w/127.0.0.1</a>)<br />
      Show your current IP (<a href="/ip">/ip</a>)<br />
      Show your useragent (<a href="/ua">/ua</a>)<br />
      
HTML;

    return $content;
  }
}