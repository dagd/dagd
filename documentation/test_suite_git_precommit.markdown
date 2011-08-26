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
