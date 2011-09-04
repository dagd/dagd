What is da.gd?
==============

da.gd is a collection of quick-info tools written in PHP. It allows you to use `curl` (or any http client) to quickly retrieve information from an easy-to-remember url.

The goal of this project is to just make an easy to use, little-of-everything tool that works on any device or in any script in any situation. So far, some interesting ideas have been requested (and implemented). If you have an idea for a feature you would like to see, please either (order of preference):

- Fork, Add feature, Send pull request for review/merge.
- Ask CodeBlock on irc (freenode). We also now have #dagd on irc.freenode.net.
- File it in the issue tracker

Because a goal of this project is to have it work for many purposes/situations, I encourage feedback, ideas, participation, and interaction with this project. Have some fun with it :)

What currently works?
=====================

`curl da.gd/help` will give you a list of what is currenly available
on the live site.

What is planned?
================

Eventually the following will work.

- /up/google.com - return 'up' if google.com is up, or 'down' if it's not.
- /i/google.com - return info about google.com (including up/down), such as the title of the page.
- / - eventually da.gd itself will be a URL shortener.
- /port/google.com/80 - check if port 80 is open on google.com - this will be limited if implemented.
- /bl/127.0.0.1 - check to see if an IP is on a dnsbl. (Thanks for the idea @Ttech!)
- /host/google.com - return the IP address of google.com.
- /host/127.0.0.1 - return the hostname/reverse dns of 127.0.0.1.
- Any other ideas you would like to see.