Knowledge Extraction via Sparse Embedding Regression (KESER)
================

## Overview

The increasing availability of Electronic Health Record (EHR) systems has created enormous potential for translational research. While the huge amount of information in EHR can be used in clinical informatics tasks, data mining for EHR data with complex structure is challenging. Traditional data mining approaches require domain knowledge to select relevant codes, which hampers the efficiency for translational research.

This packages implements the Knowledge Extraction via Sparse Embedding Regression (KESER) algorithm from our working paper, which is currently available upon request. We provide functions to use large scale code embeddings to facilitate effective feature selection and knowledge discovery with EHR data. A main advantage of the proposed embedding-based method over the existing feature selection algorithms is that it can be performed based on only summary data that can be shared across research groups.

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

- [Efficient Calculation of Coocurrence and Embeddings](https://code.ornl.gov/mvp-champion/research-support/cooccurrence-matrix)

- [Local Feature Selection with Embeddings from a Single Site](https://celehs.github.io/KESER/articles/single.html)

- [Integrative Feature Selection with Embeddings from Two Sites](https://celehs.github.io/KESER/articles/multiple.html)

- Network and Interactive Visualization (Coming Soon)

## References

- __Large Scale Code Embedding with Applications to Feature Selection and Knowledge Discovery in Electronic Health Records__. _Working Paper_. 
