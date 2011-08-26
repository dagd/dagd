Running the test suite before each commit
=========================================

- Make sure your config.php has a general.baseurl, and that it is a URL that
  points to your working copy (aka master).
- In the dagd root directory, run the following:
  `ln -s \`pwd\`/tests/dagd_test_suite.php ./.git/hooks/pre-commit`

Next time you make a change and go to `git commit` you should see the tests
run.

If a test fails and you need to bypass it (this is near-never recommended),
you can add `--no-verify` to your `git commit` command.


da.gd Config File
=================

As of commit 993a401bb728fb1bf59233b697d0711c1c71973b, da.gd knows where to
to find its configuration file through the use of Environment Variables.

The test suite, being a part of da.gd, needs to know where this file is too.
To define where the configuration file is, add the following to your .bashrc
or .bash_profile: `export DaGdConfigFile=config.dev.php` - replacing the name
of the config file to one suitable for you.

For this to take effect immediately, you can run that command at your shell
just as well.
