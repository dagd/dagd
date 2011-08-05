<?php
class DaGdAboutUsController extends DaGdBaseClass {

  protected $wrap_pre = false;
  protected $escape = false;
  
  public function render() {
    
    $content = "da.gd is a collection of PHP applications released as
[open source software](http://github.com/codeblock/dagd),  which aim to provide
information about networking, IPs, and domains.

Current commands:
Whois: ([/w/google.com](/w/google.com), [/w/127.0.0.1](/w/127.0.0.1))
Your current IP: ([/ip](/ip))
Your current useragent: ([/ua](/ua))
[Wikipedia](http://en.wikipedia.org/) edit count: ([/wp/Phuzion](/wp/Phuzion))

Add \"```?strip```\" to the end of any URL to strip the newline which otherwise
ends the response. (e.g. [/ip?strip](/ip?strip)). This is useful for scripts
which need to poll your current IP, which break with a newline character.";

    $markup = new DaGdMarkup($content);
    return $markup->render();
  }
}