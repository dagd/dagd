<?php
class DaGdAboutUsController extends DaGdBaseClass {

  protected $wrap_pre = false;
  protected $escape = false;
  
  public function render() {
    
    $content = "da.gd is an open source collection of PHP applications which
give information about networking, IPs and domains.

The source of dagd is located on
[Github](http://github.com/codeblock/dagd), and is fairly easy to hack on,
if you don't mind reading a bit.

Current features include:
***Whois*** ([/w/google.com](/w/google.com), [/w/127.0.0.1](/w/127.0.0.1))
Show your ***current IP*** ([/ip](/ip))
Show your ***useragent*** ([/ua](/ua))";

    $markup = new DaGdMarkup($content);
    return $markup->render();
  }
}