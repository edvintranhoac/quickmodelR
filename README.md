
# quickmodelR

<img src="multi.png" width="200"/>

<!-- badges: start -->
<!-- badges: end -->

The goal of quickmodelR is to build multiple machine learning models and pick the best one  all in one package

## Installation

You can install the development version of quickmodelR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edvintranhoac/quickmodelR")
```

## Example

``` r
library(quickmodelR)

# Load in data to use for creating models
data(PIMA)

# Run quickmodel function to train default models 
result <- quickmodel(Diabetes ~ ., data = PIMA)

# Run bestmodel function to retrieve the best model of the models created, based on the evaluation metric
bestmodel <- bestmodel(result)
```

