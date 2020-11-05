What is da.gd?
==============

![dagd-test](https://github.com/dagd/dagd/workflows/dagd-test/badge.svg?branch=master)

da.gd is both a URL shortener and a collection of quick-info tools written in PHP. It allows you to use `curl` (or any http client) to quickly retrieve various kinds of information such as your IP, useragent, whois for a given domain or IP, DNS lookups, etc., from an easy-to-remember url.

The goal of this project is to just make an easy to use, little-of-everything tool that works on any device or in any script in any situation. So far, some interesting ideas have been requested (and implemented). If you have an idea for a feature you would like to see, please either (order of preference):

- Fork, Add feature, Send pull request for review/merge.
- Ask relrod to implement it on irc (freenode) via PM.
- File it in the issue tracker.

Because a goal of this project is to have it work for many purposes/situations, I encourage feedback, ideas, participation, and interaction with this project. Have some fun with it :)

What currently works?
=====================

`curl da.gd/help` will give you a list of what is currenly available
on the live site.

Getting a dev environment up
============================

da.gd is slightly annoying to set up, but there are now docker-compose files in place to make it easier.
You can get a quick development environment set up by doing this:

* Ensure you have a working `docker-compose` or `podman-compose` setup.
* Clone the dagd repository
* cd container
* For docker-compose: `docker-compose up`
* For podman-compose: `podman-compose up`
* Go to http://localhost:8080/ in your regular browser.

Note that the development environment is entirely ephemeral. Anything stored in the database will be lost when you `docker-compose down` or `podman-compose down`.
That is, **the files in `./container/` are not meant for production use**.

License
=======

ASL 2.0. See `LICENSE` for more details.
