# Making a Malli release

1. Make sure `CHANGELOG.md` mentions all relevant unreleased changes
2. Recommended: update dependencies `clj -M:outdated --upgrade`
   * Make a PR out of this to get the CI to run all the tests!
3. Pick a new version number. Remember: we use [BreakVer](https://www.taoensso.com/break-versioning)
4. Set the version number
   * Add a title to `CHANGELOG.md`
   * Change the `<version>` and `<tag>` fields in `pom.xml`
5. Push to `master`
6. Create a release via the [GitHub UI](https://github.com/metosin/malli/releases/new)
   * Use the version number as the tag name, eg. `0.22.33`
7. Once the release is published, the `release` GitHub Action will build a release and deploy it to Clojars.
