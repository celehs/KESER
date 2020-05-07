Knowledge Extraction via Sparse Embedding Regression (KESER)
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
remotes::install_github("celehs/KESER")
```

Load the package into R.

``` r
library(KESER)
library(MASS)
library(glmnet)
library(gglasso)
```

## Example

``` r
file <- "https://github.com/celehs/KESER/raw/master/rdata/depression.rds"
data <- readRDS(url(file, "rb"))
str(data)
```

    ## List of 7
    ##  $ cos_lst    :List of 2
    ##   ..$ : num [1:390] 0.1109 0.0419 0.0971 0.0495 0.1222 ...
    ##   ..$ : num [1:553] 0.183 0.113 0.112 0.106 0.138 ...
    ##  $ X_full_lst :List of 4
    ##   ..$ : num [1:3700, 1:390] -1.426 0.191 0.403 -0.141 -0.572 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:390] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##   ..$ : num [1:5400, 1:553] -1.055 -0.919 0.183 -0.657 -0.427 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:553] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##   ..$ : num [1:3700, 1:312] -1.426 0.191 0.403 -0.141 -0.572 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:312] "CCS:130" "CCS:182" "CCS:200" "CCS:218" ...
    ##   ..$ : num [1:5400, 1:469] -1.055 -0.919 0.183 -0.657 -0.427 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:469] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##  $ Y_full_lst :List of 4
    ##   ..$ : num [1:3700] -1.298 0.097 -0.41 -0.118 -0.228 ...
    ##   ..$ : num [1:5400] -0.698 -0.702 -0.7 -0.259 -0.126 ...
    ##   ..$ : num [1:3700] -1.298 0.097 -0.41 -0.118 -0.228 ...
    ##   ..$ : num [1:5400] -0.698 -0.702 -0.7 -0.259 -0.126 ...
    ##  $ X_train_lst:List of 4
    ##   ..$ : num [1:3700, 1:390] 1.23 -3.84e-01 -8.46e-02 1.28e-15 4.63e-01 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:390] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##   ..$ : num [1:5400, 1:553] -0.4631 -0.0193 0.461 -0.1689 0.2052 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:553] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##   ..$ : num [1:3700, 1:312] 1.23 -3.84e-01 -8.46e-02 1.28e-15 4.63e-01 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:312] "CCS:130" "CCS:182" "CCS:200" "CCS:218" ...
    ##   ..$ : num [1:5400, 1:469] -0.4631 -0.0193 0.461 -0.1689 0.2052 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:469] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##  $ Y_train_lst:List of 4
    ##   ..$ : num [1:3700] 1.36 2.85e-01 -6.31e-02 -7.11e-16 9.94e-01 ...
    ##   ..$ : num [1:5400] -0.8208 -0.6993 0.7399 0.4333 0.0566 ...
    ##   ..$ : num [1:3700] 1.36 2.85e-01 -6.31e-02 -7.11e-16 9.94e-01 ...
    ##   ..$ : num [1:5400] -0.8208 -0.6993 0.7399 0.4333 0.0566 ...
    ##  $ X_valid_lst:List of 4
    ##   ..$ : num [1:3700, 1:390] -1.32 -4.20e-01 -5.76e-02 -1.07e-15 8.26e-01 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:390] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##   ..$ : num [1:5400, 1:553] -1.051 -0.901 0.101 -0.688 -0.45 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:553] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##   ..$ : num [1:3700, 1:312] -1.32 -4.20e-01 -5.76e-02 -1.07e-15 8.26e-01 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:312] "CCS:130" "CCS:182" "CCS:200" "CCS:218" ...
    ##   ..$ : num [1:5400, 1:469] -1.051 -0.901 0.101 -0.688 -0.45 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:469] "CCS:130" "CCS:131" "CCS:156" "CCS:163" ...
    ##  $ Y_valid_lst:List of 4
    ##   ..$ : num [1:3700] -1.36 3.17e-01 -8.34e-02 2.64e-16 3.40e-01 ...
    ##   ..$ : num [1:5400] -0.707 -0.707 -0.716 -0.157 -0.215 ...
    ##   ..$ : num [1:3700] -1.36 3.17e-01 -8.34e-02 2.64e-16 3.40e-01 ...
    ##   ..$ : num [1:5400] -0.707 -0.707 -0.716 -0.157 -0.215 ...

``` r
loc.fit.RPDR <- loc.feature.selection(
  data$X_full_lst[[1]], 
  data$Y_full_lst[[1]],
  data$X_train_lst[[1]], 
  data$Y_train_lst[[1]], 
  data$X_valid_lst[[1]], 
  data$Y_valid_lst[[1]],
  alpha = 0.5, 
  lambda_lst = NULL, 
  up_rate = 10, 
  drop_rate = 0.5, 
  cos_cut = 0.1)
```

``` r
loc.fit.VA <- loc.feature.selection(
  data$X_full_lst[[2]], 
  data$Y_full_lst[[2]],
  data$X_train_lst[[2]], 
  data$Y_train_lst[[2]], 
  data$X_valid_lst[[2]], 
  data$Y_valid_lst[[2]],
  alpha = 0.5, 
  lambda_lst = NULL, 
  up_rate = 10, 
  drop_rate = 0.5, 
  cos_cut = 0.1)
```

``` r
int.fit.results <- int.feature.selection(
  data$X_full_lst[c(1, 2)], 
  data$Y_full_lst[c(1, 2)], 
  data$X_train_lst[c(1, 2)],
  data$Y_train_lst[c(1, 2)], 
  data$X_valid_lst[c(1, 2)], 
  data$Y_valid_lst[c(1, 2)],
  alpha = 0.5, 
  lambda_single_lst = NULL, 
  lambda_group_lst = NULL, 
  up.rate = 10, 
  drop.rate = 0.5, 
  cos.cut = 0.1)
```
