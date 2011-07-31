What is da.gd?
==============

da.gd is a collection of quick-info tools written in PHP. It allows you to use `curl` (or any http client) to quickly retrieve information from an easy-to-remember url.

What currently works?
=====================

Currently /ip and /ua work.
 
    [ricky@rhelpad01 src]$ curl da.gd/ip
    174.100.171.46
    [ricky@rhelpad01 src]$ curl da.gd/ua
    curl/7.19.7 (x86_64-redhat-linux-gnu) libcurl/7.19.7 NSS/3.12.9.0 zlib/1.2.3 libidn/1.18 libssh2/1.2.2


What is planned?
================

Eventually the following will work.

- /w/google.com - whois google.com and show the result.
- /up/google.com - return 'up' if google.com is up, or 'down' if it's not.
- /i/google.com - return info about google.com (including up/down), such as the title of the page.
- / - eventually da.gd itself will be a URL shortener.
- /port/google.com/80 - check if port 80 is open on google.com - this will be limited if implemented.
- Any other ideas you would like to see.