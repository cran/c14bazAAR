[![Travis-CI Build Status](https://travis-ci.org/ISAAKiel/c14bazAAR.svg?branch=master)](https://travis-ci.org/ISAAKiel/c14bazAAR) [![Coverage Status](https://img.shields.io/codecov/c/github/ISAAKiel/c14bazAAR/master.svg)](https://codecov.io/github/ISAAKiel/c14bazAAR?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/c14bazAAR)](http://cran.r-project.org/package=c14bazAAR)
[![license](https://img.shields.io/badge/license-GPL%202-B50B82.svg)](https://www.r-project.org/Licenses/GPL-2)

# c14bazAAR

<img align="right" src="inst/image/khajiit.jpg" width = 350>

c14bazAAR is a R package to query different openly accessible radiocarbon date databases. It allows basic data cleaning, calibration and merging. It serves as back end of our [neolithicRC WebApp](https://github.com/nevrome/neolithicR). If you're not familiar with R the [WebApp](http://www.neolithicRC.de) or other tools (such as [GoGet](http://www.ibercrono.org/goget/index.php)) to search for radiocarbon dates might be better suited for your needs. 

- [Installation](#installation)
- [How to use](#how-to-use)
  - [Download](#download)
  - [Calibration](#calibration)
  - [Material classification](#material-classification)
  - [Country attribution](#country-attribution)
  - [Duplicates](#duplicates)
  - [Coordinate precision](#coordinate-precision)
  - [Conversion](#conversion)
  - [Technical functions](#technical-functions)
- [Databases](#databases)
- [Contributing](#contributing)
- [License](#license)

If you want to use data downloaded with c14bazAAR or neolithicRC for your research, you have to quote the source databases. Most databases have a preferred way of citation that also may change over time with new versions and publications. Please check the [respective homepages](#databases) to find out more. The output of c14bazAAR does not contain the full citations of the individual dates, but only a short reference tag. For further information you have to consult the source databases.

### Installation

c14bazAAR is not on [CRAN](https://cran.r-project.org/) yet, but you can install it from github. To do so, run the following lines in your R console:

```
if(!require('devtools')) install.packages('devtools')
devtools::install_github("ISAAKiel/c14bazAAR")
```

The package needs a lot of other packages -- many of them only necessary for specific tasks. Functions that require certain packages you don't have installed yet will stop and ask you to install them. Please do so with [`install.packages()`](https://www.r-bloggers.com/installing-r-packages/) to download and install the respective packages from CRAN.

### How to use

The package contains a set of getter functions (see below) to query the databases. Thereby not every available variable from every archive is downloaded. Instead c14bazAAR focuses on a [selection](https://github.com/ISAAKiel/c14bazAAR/blob/master/data-raw/variable_reference.csv) of the most important and most common variables to achieve a certain degree of standardization. The downloaded dates are stored in the custom S3 class `c14_date_list` which acts as a wrapper around the [tibble](http://tibble.tidyverse.org/) class and provides specific class methods.

One (almost) complete workflow to download and prepare all dates can be triggered like this:

```
library(c14bazAAR)
library(magrittr)

get_all_dates() %>%
  calibrate() %>%
  mark_duplicates() %>%
  classify_material() %>%
  finalize_country_name() %>%
  coordinate_precision()
```

It takes quite some time to run all of this and it's probably not necessary for your use case. Here's a list of the main tasks c14bazAAR can handle. That allows you to pick what you need:

#### Download

c14bazAAR contains a growing selection of getter functions to download radiocarbon date databases. [Here's](#databases) a list of all available getters. You can download all dates at once with [`get_all_dates()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/get_all_dates.R). The getters download the data, adjust the variable selection according to a defined [variable key](https://github.com/ISAAKiel/c14bazAAR/blob/master/data-raw/variable_reference.csv) and transform the resulting list into a `c14_date_list`. 

See `?get_dates` for more information.

```
x <- get_all_dates()
```

#### Calibration

The [`calibrate()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_calibrate.R) function calibrates all valid dates in a `c14_date_list` individually with [`Bchron::BchronCalibrate()`](https://github.com/andrewcparnell/Bchron/blob/master/R/BchronCalibrate.R). It provides two different types of output: calprobdistr and calrange. 

See `?calibrate` for more information.

```
x %>% calibrate()
```

#### Material classification

Most 14C databases provide some information about the material sampled for the individual date. Unfortunately this information is often very specific and makes filtering operations difficult for large datasets. The function [`classify_material()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_classify_material.R) relies on a [custom made classification](https://github.com/ISAAKiel/c14bazAAR/blob/master/data-raw/material_thesaurus.csv) to simplify this data. 

See `?classify_material` for more information and look [here](https://github.com/ISAAKiel/c14bazAAR/blob/master/data-raw/material_thesaurus_comments.md) for a change log of the thesaurus.

```
x %>% classify_material()
```

#### Country attribution

Filtering 14C dates by country is useful for a first spatial limitation and especially important, if no coordinates are documented. Most databases provide the variable country, but they don't rely on a unified naming convention and therefore use various terms to represent the same entity. The function [`standardize_country_name()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_spatial_standardize_country_name.R) tries to unify the semantically equal terms by string comparison with the curated country name list [`countrycode::codelist`](https://github.com/vincentarelbundock/countrycode) and a [custom made thesaurus](https://github.com/ISAAKiel/c14bazAAR/blob/master/data-raw/country_thesaurus.csv). Beyond that it turned out to be much more reliable to look at the coordinates to determine the country. 

That's what the function [`determine_country_by_coordinate()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_spatial_determine_country_by_coordinate.R) does. It joins the position with country polygons from [`rworldxtra::countriesHigh`](https://github.com/AndySouth/rworldxtra) to get reliable country attribution. 

The function [`finalize_country_name()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_spatial_finalize_country_name.R) finally combines the initial country information in the database and the results of the two previous functions to forge a single column country_final. If the necessary columns are missing, it calls the previous functions automatically. 

See `?country_attribution` for more information.

```
x %>%
  standardize_country_name() %>%
  determine_country_by_coordinate() %>%
  finalize_country_name()
```

#### Duplicates

Some of the source databases already contain duplicated dates and for sure you'll have some if you combine different databases. As a result of the long history of these archives, which includes even mutual absorption, duplicates make up a significant proportion of combined datasets. The function [`mark_duplicates()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_duplicates_mark.R) adds a column duplicate group to the c14_date_list, that assigns duplicates found by lab code comparison a common group number. This should allow you to make an educated decision, which dates to discard. 

For an automatic removal there's the function [`remove_duplicates()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_duplicates_remove.R). It boils down all dates in a duplicate_group to one entry. Unequal values become NA. All variants for all columns are documented within a string in the column duplicate_remove_log. 

See `?duplicates` for more information.

```
x %>%
  mark_duplicates() %>%
  remove_duplicates()
```

#### Coordinate precision

The function [`coordinate_precision()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_spatial_coordinate_precision.R) allows to calculate the precision of the coordinate information. It relies on the number of digits in the columns lat and lon. The mean of the inaccuracy on the x and y axis in meters is stored in the additional column coord_precision. 

See `?coordinate_precision` for more information.

```
x %>% coordinate_precision()
```

#### Conversion

A c14_date_list can be directly converted to other R data structures. So far only [`as.sf()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_convert.R) is implemented. The sf package provides great tools to manipulate and plot spatial vector data. This simplifies certain spatial operations with the date point cloud.

See `?as.sf` for more information.

```
x %>% as.sf()
```

#### Technical functions

c14_date_lists are constructed with [`as.c14_date_list`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_basic.R). This function takes data.frames or tibbles and adds the c14_date_list class tag. It also calls [`order_variables()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_order_variables.R) to establish a certain variable order and [`enforce_types()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_enforce_types.R) which converts all variables to the correct data type. There are custom `print()` and `format()` methods for c14_date_lists. 

The [`fuse()`](https://github.com/ISAAKiel/c14bazAAR/blob/master/R/c14_date_list_fuse.R) function allows to rowbind multiple c14_date_lists.

See `?as.c14_date_list` and `?fuse`.

```
x1 <- data.frame(
  c14age = 2000,
  c14std = 30
) %>% as.c14_date_list()

x2 <- fuse(x1, x1)
```

### Databases

To suggest other archives to be queried you can join the discussion [here](https://github.com/ISAAKiel/c14bazAAR/issues/2).

* [`get_14SEA()`](R/get_14sea.R) [**14SEA**](http://www.14sea.org/) 14C database for Southeast Europe and Anatolia (10,000–3000 calBC).
* [`get_aDRAC()`](R/get_adrac.R) [**aDRAC**](https://github.com/dirkseidensticker/aDRAC): Archives des datations radiocarbone d'Afrique centrale by Dirk Seidensticker.
* [`get_AustArch()`](R/get_austarch.R) [**AustArch**](http://archaeologydataservice.ac.uk/archives/view/austarch_na_2014/): A Database of 14C and Luminescence Ages from Archaeological Sites in Australia by [Alan N. Williams, Sean Ulm, Mike Smith, Jill Reid](http://intarch.ac.uk/journal/issue36/6/williams.html)
* [`get_CalPal()`](R/get_calpal.R) [**CALPAL**](https://uni-koeln.academia.edu/BernhardWeninger/CalPal): Radiocarbon Database of the [CalPal software package](http://monrepos-rgzm.de/forschung/ausstattung.html#calpal) by Bernhard Weninger. See [nevrome/CalPal-Database](https://github.com/nevrome/CalPal-Database) for an interface.
* [`get_CONTEXT()`](R/get_context.R) [**CONTEXT**](http://context-database.uni-koeln.de/): Collection of radiocarbon dates from sites in the Near East and neighboring regions (20.000 - 5.000 calBC) by Utz Böhner and Daniel Schyle.
* [`get_EUROEVOL()`](R/get_euroevol.R) [**EUROEVOL**](http://discovery.ucl.ac.uk/1469811/): Cultural Evolution of Neolithic Europe Dataset by [Katie Manning, Sue Colledge, Enrico Crema, Stephen Shennan and Adrian Timpson](http://openarchaeologydata.metajnl.com/articles/10.5334/joad.40/).
* [`get_KITEeastAfrica()`](R/get_eastafrica.R) [**CARD Upload Template - KITE East Africa v2.1**](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NJLNRJ): Radiocarbon dates from eastern Africa in the CARD2.0 format by [Colin Courtney Mustaphi, Rob Marchant](https://www.openquaternary.com/articles/10.5334/oq.22/)
* [`get_RADON()`](R/get_radon.R) [**RADON**](http://radon.ufg.uni-kiel.de/): Central European and Scandinavian database of 14C dates for the Neolithic and Early Bronze Age by [Dirk Raetzel-Fabian, Martin Furholt, Martin Hinz, Johannes Müller, Christoph Rinne, Karl-Göran Sjögren und Hans-Peter Wotzka](http://www.jna.uni-kiel.de/index.php/jna/article/view/65).
* [`get_RADONB()`](R/get_radonb.R) [**RADON-B**](http://radon-b.ufg.uni-kiel.de/): Database for European 14C dates for the Bronze and Early Iron Age by Jutta Kneisel, Martin Hinz, Christoph Rinne.

### Contributing

If you would like to contribute to this project, please start by reading our [Guide to Contributing](CONTRIBUTING.md). Please note that this project is released with a Contributor [Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

For the code in this project apply the terms and conditions of GNU GENERAL PUBLIC LICENSE Version 2. The source databases are published under different licences.
