auspol
================
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/carlosyanez/auspol/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/carlosyanez/auspol/actions/workflows/R-CMD-check.yaml)
  [![auspol status badge](https://carlosyanez.r-universe.dev/badges/auspol)](https://carlosyanez.r-universe.dev)
  <!-- badges: end -->
  
<img src="https://github.com/carlosyanez/auspol/raw/main/img/hexSticker.png" width = "175" height = "200" align="right" />

**auspol** provides result data for the Australian Federal Elections, from 2004 onwards, direclty from R.
Along with functions to access the data, this package also provides functions to provide summaries tables and plots.
The current version provides election data for the House of Representatives.

All the data is contained as a release ([here](https://github.com/carlosyanez/auspol/releases/tag/data)) in [Apache Parquet format](https://arrow.apache.org/docs/r/index.html), so it can be used directly in any programming language/tool/platform that supports Parquet.


## Installation and pre-requisites

The current version of this packages (0.0.1.0000) is not yet in CRAN. To  install the package, you can download it from Github:

```
devtools::install_github("carlosyanez/auspol")
```
Alternatively, install from r-universe:

```

# Enable this universe
options(repos = c(
    carlosyanez = 'https://carlosyanez.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('auspol')
```

## How to use

This package provides a series of functions to access the data as published by the AEC, as well as convinience functions to retrieve summaries and produce standard plots. For reference on how to check the [articles](articles/index.html).

## Disclaimer

This package's data has been sourced from the Australian  Electoral Commission's website. However, data is provided here with no guarantees of accuracy.

**The Australian Electoral Commission is the authorative source of election data**. Should you need to certify the authenticity or correctness of the data, you should  retrieve the results directly from the AEC's website.

The GitHub repository contains the raw data processing scripts in the *data-raw* folder.

## Issues? bugs? Ideas?

If you find something that is not quite right, please post an issue. If
you have any ideas,requests, or if you want to contribute, [please let me know](https://twitter.com/messages/25712933-3805104374?recipient_id=25712933&text=Hello%20world)!

## To Do

This package is still under development. At the moment, it only contains data for the House of Representatives.

## Credits

-   Data has been sourced directly from the [Australian Electoral Commission](https://www.aec.gov.au/). Historical results for all Commonwealth elections are published on the AEC's [Tally Room archive](https://results.aec.gov.au/).
-  Some functions use code written for the [{tigris} package](https://github.com/walkerke/tigris), maintained by [Kyle Walker](https://github.com/walkerke).
-  <div> Icons made by <a href="https://www.flaticon.com/authors/pixel-perfect" title="Pixel perfect"> Pixel perfect </a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com'</a></div>
- <a href="https://www.flaticon.com/free-icons/work-in-progress" title="work in progress icons">Work in progress icons created by Freepik - Flaticon</a>

## Acknowledgment of Country

The author of this package acknowledges the Boonwurrung/Bunurong and Wurrundjeri Woi Wurrung peoples of the Eastern Kulin Nation as Traditional Owners and Custodians of the land where this package has been created, and pays respect to their Elders past, present and emerging.


