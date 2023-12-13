> This repository stores the code and data used to perform the analysis presented in the manuscript *In-season monitoring of harvest and effort form a large-scale subsistence salmon fishery in western Alaska* by authors B. Staton, W. Bechtol, L. Coggins, G. Decossas, and J. Esquible. The manuscript is under review.

*Insert Article DOI if accepted*

*Insert GitHub Repo Archive DOI once minted*

## Repository Structure

| File/Subdirectory          | Purpose                                                                                                          |
|------------------------------|------------------------------------------|
| `effort-simulation/`       | Contains code to perform all simulation evaluations of the effort estimator and plot the results                 |
| `validation/`              | Contains code and data to perform the comparison of in-season harvest estimates to post-season harvest estimates |
| `session-setup.R`          | Loads packages, sets directories, creates miscellaneous objects used throughout several scripts                  |
| `make-figures.R`           | When executed, recreates all figures, except those for the effort simulation                                     |
| `make-tables.R`            | When executed, creates all numerical table output                                                                |
| `make-in-text-summaries.R` | When executed, calculates a variety of summary statistics that are presented the methods and results text        |

## Dependencies

Users will need to install the R package '[KuskoHarvData](https://github.com/bstaton1/KuskoHarvData)', which stores meta data, interview data, flight data, and the compiled harvest and effort estimates from the in-season monitoring program:

``` r
install.packages("remotes")
remotes::install_github("bstaton1/KuskoHarvData")
```

Two other packages will simultaneously be installed:

-   '[KuskoHarvEst](https://github.com/bstaton1/KuskoHarvEst)': contains the workflow to produce harvest estimates from interview and flight data

-   '[KuskoHarvUtils](https://github.com/bstaton1/KuskoHarvUtils)': contains several helper functions used throughout the 'KuskoHarv\*' family of packages

There are several other packages (all available from CRAN) loaded by the scripts or used with `pkg::fun()` syntax. The user can install them on the fly after discovering that they don't have them -- see Session Info below for the complete list and versions used.

## Reproducing Article Content

To reproduce all content presented in the manuscript, open [RStudio](https://posit.co/download/rstudio-desktop/) to the `KuskoHarvEst-ms-analysis.Rproj` location and execute:

``` r
source("make-figures.R")                   # exports PNG files for manuscript
source("make-tables.R")                    # exports CSV files for manuscript
source("make-in-text-summaries.R")         # examine output line-by-line, output saved
source("effort-simulation/simple-sim.R")   # exports a figure
source("effort-simulation/complex-sim.R")  # may take an hour or more; exports a figure
```

After executing this code, the there will be two new subdirectories: `figures` and `tables`, which store the PNG and CSV output files used in building the manuscript.

## Session Info

For reproducibility purposes, all analyses were conducted using this configuration:

```         
─ Session info ──────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.3.0 (2023-04-21 ucrt)
 os       Windows 10 x64 (build 19045)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  English_United States.utf8
 ctype    English_United States.utf8
 tz       America/Los_Angeles
 date     2023-12-12
 rstudio  2023.03.0+386 Cherry Blossom (desktop)
 pandoc   NA

─ Packages ──────────────────────────────────────────────────────────────────────────────
 package        * version date (UTC) lib source
 cli              3.6.1   2023-03-23 [1] CRAN (R 4.3.0)
 colorspace       2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
 dplyr          * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
 fansi            1.0.4   2023-01-22 [1] CRAN (R 4.3.0)
 farver           2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
 generics         0.1.3   2022-07-05 [1] CRAN (R 4.3.0)
 glue             1.6.2   2022-02-24 [1] CRAN (R 4.3.0)
 gridExtra        2.3     2017-09-09 [1] CRAN (R 4.3.0)
 gtable           0.3.3   2023-03-21 [1] CRAN (R 4.3.0)
 knitr            1.42    2023-01-25 [1] CRAN (R 4.3.0)
 KuskoHarvEst     1.2.4   2023-10-24 [1] Github (bstaton1/KuskoHarvEst@e8e252f)
 KuskoHarvUtils   0.1.1   2023-10-30 [1] Github (bstaton1/KuskoHarvUtils@02a01d1)
 lifecycle        1.0.3   2022-10-07 [1] CRAN (R 4.3.0)
 lubridate      * 1.9.2   2023-02-10 [1] CRAN (R 4.3.0)
 magrittr         2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
 munsell          0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
 pillar           1.9.0   2023-03-22 [1] CRAN (R 4.3.0)
 pkgconfig        2.0.3   2019-09-22 [1] CRAN (R 4.3.0)
 plyr             1.8.8   2022-11-11 [1] CRAN (R 4.3.0)
 png              0.1-8   2022-11-29 [1] CRAN (R 4.3.0)
 R6               2.5.1   2021-08-19 [1] CRAN (R 4.3.0)
 Rcpp             1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
 reshape2       * 1.4.4   2020-04-09 [1] CRAN (R 4.3.0)
 rlang            1.1.0   2023-03-14 [1] CRAN (R 4.3.0)
 rprojroot        2.0.3   2022-04-02 [1] CRAN (R 4.3.0)
 rstudioapi       0.14    2022-08-22 [1] CRAN (R 4.3.0)
 scales           1.2.1   2022-08-20 [1] CRAN (R 4.3.0)
 sessioninfo      1.2.2   2021-12-06 [1] CRAN (R 4.3.0)
 stringi          1.7.12  2023-01-11 [1] CRAN (R 4.3.0)
 stringr        * 1.5.0   2022-12-02 [1] CRAN (R 4.3.0)
 this.path        2.3.1   2023-12-11 [1] CRAN (R 4.3.2)
 tibble           3.2.1   2023-03-20 [1] CRAN (R 4.3.0)
 tidyselect       1.2.0   2022-10-10 [1] CRAN (R 4.3.0)
 timechange       0.2.0   2023-01-11 [1] CRAN (R 4.3.0)
 utf8             1.2.3   2023-01-31 [1] CRAN (R 4.3.0)
 vctrs            0.6.2   2023-04-19 [1] CRAN (R 4.3.0)
 withr            2.5.0   2022-03-03 [1] CRAN (R 4.3.0)
 xfun             0.39    2023-04-20 [1] CRAN (R 4.3.0)

 [1] C:/Users/bstaton/AppData/Local/R/win-library/4.3
 [2] C:/Program Files/R/R-4.3.0/library
─────────────────────────────────────────────────────────────────────────────────────────
```
