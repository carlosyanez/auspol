auspol
================
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/carlosyanez/auspol/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/carlosyanez/auspol/actions/workflows/R-CMD-check.yaml)
  [![auspol status badge](https://carlosyanez.r-universe.dev/badges/auspol)](https://carlosyanez.r-universe.dev)
  <!-- badges: end -->
  
<img src="https://github.com/carlosyanez/auspol/raw/main/img/work-in-progress.png"  width = "175" align="center"/>
<br/>
<br/>

<img src="https://github.com/carlosyanez/auspol/raw/main/img/hexSticker.png" width = "175" height = "200" align="right" />

**auspol** provides result data for the Australian Federal Elections, from 2004 onwards, direclty from R.
Along with functions to access the data, this package also provides functions to provide summaries tables and plots.

All the data is contained as a release ([here](https://github.com/carlosyanez/auspol/releases/tag/data)) in [Apache Parquet format](https://arrow.apache.org/docs/r/index.html), so it can be used directly in any programming language/tool/platform that supports Parquet.


## Installation and pre-requisites

First install required packages,then, install this package from github

```
devtools::install_github("carlosyanez/auspol")
```
Alternatively, install from r-universe

```

# Enable this universe
options(repos = c(
    carlosyanez = 'https://carlosyanez.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('auspol')
```

## Issues? bugs? Ideas?

If you find something that is not quite right, please post an issue. If
you have any ideas, or if you want to contribute, [please let me know](https://twitter.com/messages/25712933-3805104374?recipient_id=25712933&text=Hello%20world)!

## To Do

This package is still under development. At the moment, it only contains data for the House of Representatives.

## Credits

-   Data has been sourced directly from the [Australian Electoral Commission](https://www.aec.gov.au/). Historical results for all Commonwealth elections are published on the AEC's [Tally Room archive](https://results.aec.gov.au/).
-  Some functions use code written for the [{tigris} package](https://github.com/walkerke/tigris), maintained by [Kyle Walker](https://github.com/walkerke).
-  <div> Icons made by <a href="https://www.flaticon.com/authors/pixel-perfect" title="Pixel perfect"> Pixel perfect </a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com'</a></div>
- <a href="https://www.flaticon.com/free-icons/work-in-progress" title="work in progress icons">Work in progress icons created by Freepik - Flaticon</a>
