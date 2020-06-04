Knowledge Extraction via Sparse Embedding Regression (KESER)
================

## Overview

The increasing availability of Electronic Health Record (EHR) systems
has created enormous potential for translational research. While the
huge amount of information in EHR can be used in clinical informatics
tasks, data mining for EHR data with complex structure is challenging.
Traditional data mining approaches require domain knowledge to select
relevant codes, which hampers the efficiency for translational research.

This packages implements the Knowledge Extraction via Sparse Embedding
Regression (KESER) algorithm from our working paper, which is currently
available upon request. We provide functions to use large scale code
embeddings to facilitate effective feature selection and knowledge
discovery with EHR data. A main advantage of the proposed
embedding-based method over the existing feature selection algorithms is
that it can be performed based on only summary data that can be shared
across research
groups.

<img src="https://github.com/celehs/KESER/raw/master/img/workflow.png" width="800" />

## Getting Started

Install development version from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("celehs/KESER")
```

Load the package into R.

``` r
library(KESER)
```

``` r
dir <- "https://github.com/celehs/KESER/raw/master/rdata/"
data <- readRDS(url(paste0(dir, "depression.rds"), "rb"))
```

``` r
str(data)
```

    ## List of 6
    ##  $ X_full_lst :List of 2
    ##   ..$ : num [1:3700, 1:387] -1.426 0.191 0.403 -0.141 -0.572 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:387] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##   ..$ : num [1:5400, 1:552] -1.055 -0.919 0.183 -0.657 -0.427 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:552] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##  $ Y_full_lst :List of 2
    ##   ..$ : num [1:3700] -1.298 0.097 -0.41 -0.118 -0.228 ...
    ##   ..$ : num [1:5400] -0.698 -0.702 -0.7 -0.259 -0.126 ...
    ##  $ X_train_lst:List of 2
    ##   ..$ : num [1:3700, 1:387] 1.23 -3.84e-01 -8.46e-02 1.28e-15 4.63e-01 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:387] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##   ..$ : num [1:5400, 1:552] -0.4631 -0.0193 0.461 -0.1689 0.2052 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:552] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##  $ Y_train_lst:List of 2
    ##   ..$ : num [1:3700] 1.36 2.85e-01 -6.31e-02 -7.11e-16 9.94e-01 ...
    ##   ..$ : num [1:5400] -0.8208 -0.6993 0.7399 0.4333 0.0566 ...
    ##  $ X_valid_lst:List of 2
    ##   ..$ : num [1:3700, 1:387] -1.32 -4.20e-01 -5.76e-02 -1.07e-15 8.26e-01 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:387] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##   ..$ : num [1:5400, 1:552] -1.051 -0.901 0.101 -0.688 -0.45 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:552] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##  $ Y_valid_lst:List of 2
    ##   ..$ : num [1:3700] -1.36 3.17e-01 -8.34e-02 2.64e-16 3.40e-01 ...
    ##   ..$ : num [1:5400] -0.707 -0.707 -0.716 -0.157 -0.215 ...

<img src="https://github.com/celehs/KESER/raw/master/img/interface.png" width="800" />

## References

  - **Large Scale Code Embedding with Applications to Feature Selection
    and Knowledge Discovery in Electronic Health Records**. *Working
    Paper*.
