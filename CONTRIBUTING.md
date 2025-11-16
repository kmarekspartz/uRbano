# Contributing to uRbano

This outlines how to propose a change to uRbano.

When contributing to this repository, please first discuss the change you wish to make via issue,
email, or any other method with the owners of this repository before making a change.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file.

This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file.
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed.

### Pull request process
*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("MSP-LTER/uRbano", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`.
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.
*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

## Pull Request Process

1. Ensure any install or build dependencies are removed before the end of the layer when doing a
   build.
2. Update the README.md with details of changes that may affect consumers of the package.
3. Increase the version numbers in any examples files and the README.md to the new version that this
   Pull Request would represent. The versioning scheme we use is [SemVer](http://semver.org/) with the date appended.
4. You may merge the Pull Request in once you have the sign-off of two other developers, or if you
   do not have permission to do that, you may request the second reviewer to merge it for you.

### Code style

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests.
   Contributions with test cases included are easier to accept.

## Code of Conduct

Please note that the uRbano project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
