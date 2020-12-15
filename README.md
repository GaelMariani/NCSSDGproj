
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Link bewteen Natural Climate Solutions and Sustainable Development Goals

This repository contains the data and codes used to obtain results of
the paper by Mariani et al. (202?) - Hit two targets with one stone with
Natural Climate Solutions.

## Contents

[:file\_folder: **rawdata**](rawdata/) directory contains the 0/1 matrix
used to perform all analysis. See Material & Methods section of the
paper for more details. This directory also contains SDG’s and NCS’s
icons use in the figures of the paper.

[:file\_folder: **results**](results/) directory contains all `.RData`
files generated during the analyses.

[:file\_folder: **R**](R/) directory contains the functions used to
perform all the analyses. It contains 4 `.R` files:

1.  [:open\_book: read\_data.R](R/read_data.R) contains functions to
    read data.
2.  [:package: format\_data.R](R/format_data.R) contains functions to
    format the data in the desired format to make the analyses and
    figures.
3.  [:microscope: analysis.R](R/analysis.R) contains functions to
    perform analysis, mainly null models and correspondance analysis.
4.  [:bar\_chart: plot\_data.R](R/plot_data.R) contains all functions to
    obtained plots in the paper.

[:file\_folder: **figures**](figures/) directory contains all figures in
the paper.

The [:hammer: dev\_history.R](dev_history.R) file allow to rebuild our
research compendium from scratch.

The [:briefcase: make.R](make.R) file allow to produce all the analyses
and render the corresponding plots.

## How to run it?

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium by cloning this
[repository](https://github.com/GaelMariani/NCSSDGproj). You can follow
this
[tutorial](https://docs.github.com/en/free-pro-team@latest/github/creating-cloning-and-archiving-repositories/cloning-a-repository)
to clone the repository. <br>

Once the compendium is cloned, follow these steps:

1.  open the `.Rproj` file in RStudio.
2.  open [make.R](make.R) and run it to produce all the analyses.

### Session Info

``` r
utils::sessionInfo()
#> R version 3.6.2 (2019-12-12)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19041)
#> 
#> Matrix products: default
#> 
#> locale:
#> [1] LC_COLLATE=French_France.1252  LC_CTYPE=French_France.1252   
#> [3] LC_MONETARY=French_France.1252 LC_NUMERIC=C                  
#> [5] LC_TIME=French_France.1252    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_3.6.2  magrittr_1.5    tools_3.6.2     htmltools_0.4.0
#>  [5] yaml_2.2.1      Rcpp_1.0.4.6    stringi_1.4.6   rmarkdown_2.5  
#>  [9] knitr_1.30      stringr_1.4.0   xfun_0.19       digest_0.6.25  
#> [13] rlang_0.4.6     evaluate_0.14
```

### Licenses

**Text and figures:**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code:** See the [DESCRIPTION](DESCRIPTION) file

**Data:** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse
