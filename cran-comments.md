## Test environments and R CMD check results
* local OS X install (x86_64-apple-darwin13.4.0), R 3.2.3
  * There were no ERRORs, WARNINGs, or NOTEs.

* ubuntu 12.04 (on travis-ci, x86_64-pc-linux-gnu), R 3.2.3
  * There were no ERRORs, WARNINGs.  
  * There are two NOTEs, but they are false positives.

    * checking CRAN incoming feasibility ... NOTE
    Checking URLs requires 'libcurl' support in the R build

    * checking package dependencies ... NOTE
      No repository set, so cyclic dependency check skipped

* win-builder (devel and release)
  * There were no ERRORs, WARNINGs.  
  * There is one NOTE.
    * The author was changed, as I made a silly spelling mistake of my name previously.  The possibly mis-spelled words are correctly spelled.

      * checking CRAN incoming feasibility ... NOTE
      Maintainer: 'Barret Schloerke <schloerke@gmail.com>'

      New maintainer:
        Barret Schloerke <schloerke@gmail.com>
      Old maintainer(s):
        Barret Scloerke <schloerke@gmail.com>

      Possibly mis-spelled words in DESCRIPTION:
        geozoo (5:43)
        tourr (5:98)


## Downstream dependencies
There are 0 downstream dependencies. (Yay!)
