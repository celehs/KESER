Knowledge Extraction via Sparse Embedding Regression (KESER)
================

## Overview

The increasing availability of Electronic Health Record (EHR) systems has created enormous potential for translational research. While the huge amount of information in EHR can be used in clinical informatics
tasks, data mining for EHR data with complex structure is challenging. Traditional data mining approaches require domain knowledge to select relevant codes, which hampers the efficiency for translational research.

This packages implements the Knowledge Extraction via Sparse Embedding Regression (KESER) algorithm from our working paper, which is currently available upon request. We provide functions to use large scale code embeddings to facilitate effective feature selection and knowledge discovery with EHR data.

## Installation

Install development version from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("celehs/KESER")
```

Load the package into R.

``` r
library(KESER)
```

## Documentation

- [Example 1: Local Feature Selection with Data from Partners Healthcare](https://celehs.github.io/KESER/articles/example1.html)

- [Example 2: Local Feature Selection with Data from Veteran Affairs (VA)](https://celehs.github.io/KESER/articles/example2.html)

- [Example 3: Integrative Feature Selection with Data from Two Sources](https://celehs.github.io/KESER/articles/example3.html)

## References

- __Large Scale Code Embedding with Applications to Feature Selection and Knowledge Discovery in Electronic Health Records__. _Working Paper_. 
