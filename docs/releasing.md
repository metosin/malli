# Making a Malli release

1. Make sure `CHANGELOG.md` mentions all relevant unreleased changes
2. Pick a new version number. Remember: we use [BreakVer](https://www.taoensso.com/break-versioning)
3. Set the version number
   * Add a title to `CHANGELOG.md`
   * Change the `<version>` and `<tag>` fields in `pom.xml`
3. Push to `master`
4. Create a release via the [GitHub UI](https://github.com/metosin/malli/releases/new)
   * Use the version number as the tag name, eg. `0.22.33`
5. Once the release is published, the `release` GitHub Action will build a release and deploy it to Clojars.
