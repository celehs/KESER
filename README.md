Large Scale Code Embedding with Applications to Feature Selection and
Knowledge Discovery in Electronic Health Records
================

## Overview

The increasing availability of Electronic Health Record (EHR) systems
has created enormous potential for translational research. While the
huge amount of information in EHR can be used in clinical informatics
tasks, data mining for EHR data with complex structure is challenging.
Traditional data mining approaches require domain knowledge to select
relevant codes, which hampers the efficiency for translational research.
In this project, we aim to using large scale code embeddings to
facilitate effective feature selection and knowledge discovery with EHR
data.

## Installation

Install development version from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("celehs/embedFS")
```

Load the package into
R.

``` r
library(embedFS)
```

``` r
data <- readRDS(url("https://github.com/celehs/embedFS/raw/master/embed/embed.rds", "rb"))
str(data)
```

    ## List of 7
    ##  $ cos.sim:'data.frame': 203 obs. of  2 variables:
    ##   ..$ feature.list1: chr [1:203] "CCS:142" "CCS:143" "CCS:152" "CCS:153" ...
    ##   ..$ cos.sims     : num [1:203] 0.113 0.03 0.116 0.105 0.131 ...
    ##  $ x.full : num [1:1400, 1:203] -0.5279 0.0589 0.0555 -0.1405 0.1265 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : NULL
    ##   .. ..$ : chr [1:203] "CCS:142" "CCS:143" "CCS:152" "CCS:153" ...
    ##  $ x.train: num [1:1400, 1:203] 4.64e-01 -1.13e-01 -2.53e-02 -1.11e-15 -1.27e-01 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : NULL
    ##   .. ..$ : chr [1:203] "CCS:142" "CCS:143" "CCS:152" "CCS:153" ...
    ##  $ x.valid: num [1:1400, 1:203] -4.86e-01 -1.05e-01 -1.89e-02 -4.27e-16 -1.12e-01 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : NULL
    ##   .. ..$ : chr [1:203] "CCS:142" "CCS:143" "CCS:152" "CCS:153" ...
    ##  $ y.full : num [1:1400] -0.6533 0.0791 0.1126 -0.0566 -0.1325 ...
    ##  $ y.train: num [1:1400] 5.83e-01 -1.60e-01 -3.59e-02 6.18e-17 2.08e-02 ...
    ##  $ y.valid: num [1:1400] -6.35e-01 -1.46e-01 -2.73e-02 6.35e-16 1.48e-01 ...
