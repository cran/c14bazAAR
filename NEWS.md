# c14bazAAR 1.2.0

## general changes
- unified database names in all functions, tables, variables and documentation (#86)
- new logo and some layout changes in the README (#81)

## new features
- added a basic plot function for c14_date_lists (#82)
- added a basic write function for c14_date_lists: `write_c14()` (#84)
- added a version column that documents from which database version a certain date is pulled (#85)

# c14bazAAR 1.1.0

## general changes
- [ROpenSci review](https://github.com/ropensci/software-review/issues/333)
- moved main development repository to github/ropensci/c14bazAAR (e0e6827f0381be04c50380eec277c01cad44ac7d)
- created [c14bazAAR project](https://doi.org/10.17605/OSF.IO/3DS6A) on the OSF platform with a DOI
- more work on an article for the Journal of Open Source Software (paper.md + paper.bib)
- changed citation to JOSS article after its publication

## new features
- new download interface as suggested by Enrico Crema in the ROpenSci review: `get_c14data()` (#76)
- replaced hard coded URLs with arguments to get helper functions (caabcb7b)

## new getter functions
- added getter function for irdd database: `get_irdd` (#79)

# c14bazAAR 1.0.3

## general changes
- reformatted authors in DESCRIPTION and added ORCIDs (#72)
- added a [citation](https://github.com/ropensci/c14bazAAR#citation) section to the README
- added a [checklist](https://github.com/ropensci/c14bazAAR#adding-database-getter-functions) to the README on how to add new getter functions
- work on an article for the Journal of Open Source Software (paper.md + paper.bib)
- added a vignette with some plotting workflows
- created a completely artificial example dataset that replaces the sampled version

## new getter functions
- added getter function for Palmisano et al. database: `get_palmisano` (#59)
- added getter function for eubar database: `get_eubar` (#64)

## new features
- added new options for the deduplication function (see `?duplicates`) (#63)
- added an internal function `clean_labnr` to the `as.c14_date_list` workflow to fix certain syntactically wrong representations of lab numbers in several input databases as part of the downloading process (#61)
- better implementation of the `c14_date_list` as a subclass of tibble for a better integration of the subclass into the tidyverse (#67)

## bugfixes
- small file path construction fix in context getter function
- replaced some deprecated functions by other packages (dplyr::funs & tibble::as.tibble)
- replaced `RCurl::url.exists` with `httr::http_error` in `check_connection_to_url` (#68)
- fixed `as.sf` error that occurred when date lists with dates without coordinates were transformed

## removed objects
- data objects `c14bazAAR::country_thesaurus`, `c14bazAAR::material_thesaurus`, `c14bazAAR::variable-reference` have been removed from the package -- they are queried from [here](https://github.com/ropensci/c14bazAAR/tree/master/data-raw) anyway and it's not necessary to put them into the package
- some helper functions have been made internal
- .Rd files for unexported, internal objects have been removed (@noRd)

# c14bazAAR 1.0.2

Release version
