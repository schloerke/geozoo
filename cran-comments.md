## Version
0.5.1

## Test environments and R CMD check results
* local OS X install (x86_64-apple-darwin13.4.0), R 3.3.0
  * There were no ERRORs, WARNINGs, or NOTEs.

* ubuntu 12.04 (on travis-ci, x86_64-pc-linux-gnu), R 3.3.0
  * There were no ERRORs, WARNINGs or NOTEs.

* win-builder (release only (no devel available))
  * There were no ERRORs, WARNINGs.  

  * There is one NOTE while checking CRAN incoming feasibility

    * False positive spelling
      Possibly mis-spelled words in DESCRIPTION:
        geozoo (5:43)
        tourr (5:97)

    * This url is from a download count badge (that displays in github) which are counts provided by rstudio.  My thought is it should point to their servers for this reason.  I'd be happy to change if it makes things easier.

      Found the following (possibly) invalid URLs:
        URL: http://cran.rstudio.com/web/packages/geozoo/index.html (moved to https://cran.rstudio.com/web/packages/geozoo/index.html)
          From: README.md
          Status: 200
          Message: OK
          CRAN URL not in canonical form
        A canonical CRAN URL starts with https://cran.r-project.org/

## Downstream dependencies
There are 0 downstream dependencies. (Yay!)
