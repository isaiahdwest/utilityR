# utilityR :construction_worker:
 <!-- badges: start -->
  [![R-CMD-check](https://github.com/isaiahdwest/utilityR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/isaiahdwest/utilityR/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
General utility functions for working in R. 

For installation:

```
install.packages("remotes")
remotes::install_github("isaiahdwest/utilityR")
```

## Data Manipulation/Cleaning
- Join datasets together and coalesce duplicate columns with `joinCoalesce`
- Create a pipeline of cleaning functions with `map_fns`
- Cleanse bad data with `na_if_func`
- Clean `data.frame` names from messy excel or csv files with `guess_names`
- Multiple variable assignment with `g()` and `%=%`
- Easily pivot multiple columns into long format in parallel with `parallel_longer`

## Stats
- Calculate standard error from the mean of a variable with `se()`
- Calculate a confidence interval around the mean of a variable using `cf.int()`, using t-test or z-score critical values.

## Package Dependency
- Check what dependencies that an available package has with `package.dependencies("package")`
- install the dependencies for a package with `install.dependencies("package")`

## Extensions
- Assign names to split data with `split2(data)`
- Sting extraction with `grext` and `grextall` that use only base R functions under the hood - equivalents of `stringr::extract` and `stringr::extract_all`

