
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Co-benefits and trade-offs between Natural Climate Solutions and Sustainable Development Goals

This repository contains data and code used to obtain results of the
paper Mariani et al. (202?) - Co-benefits and trade\_offs between
Natural Climate Solutions and Sustainable Development Goals in ???.

## Contents

[:file\_folder: **rawdata**](rawdata/) directory contains the table of
positive and negative links used to perform all analysis. See Material &
Methods section of the paper for more details. This directory also
contains SDG’s and NCS’s icons use in the figures of the paper.

[:file\_folder: **results**](results/) directory contains all `.RData`
files generated during the analyses.

[:file\_folder: **R**](R/) directory contains the functions used to
perform all the analyses. The 4 main importants `.R` files are:

1.  [:open\_book: read\_data.R](R/read_data.R) contains functions to
    read data.
2.  [:package: format\_data.R](R/format_data.R) contains functions to
    format the data in the desired format to make the analyses and
    figures.
3.  [:microscope: analysis.R](R/analysis.R) contains functions to
    perform statistical analyses, mainly null models and correspondance
    analysis.
4.  [:bar\_chart: plot\_data.R](R/plot_data.R) contains all functions to
    obtain plots in the paper.

[:file\_folder: **figures**](figures/) directory contains all figures
(format .png) in the paper.

[:file\_folder: **make\_results**](make_results/) contains detailled R
scripts to produce figures and run analysis using all functions in
[**R**](R/).

The [:briefcase: **make.R**](make.R) file allows to produce all analyses
and render the corresponding plots by running R scripts in
[**make\_results**](make_results/).

The [:hammer: dev\_history.R](dev_history.R) file allows to rebuild our
research compendium from scratch.

## How to run it?

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the following softwares:

-   R: <https://cloud.r-project.org/>
-   RStudio: <https://rstudio.com/products/rstudio/download/>
-   Git: <https://git-scm.com/>
-   Create a GitHub account: <https://github.com/>

You can download the compendium by cloning this
[repository](https://github.com/GaelMariani/NCSSDGproj). You can follow
this
[tutorial](https://resources.github.com/whitepapers/github-and-rstudio/#:~:text=Clone%20the%20repository%20with%20RStudio&text=Click%20the%20Copy%20to%20clipboard,the%20Project%20directory%20name%20field.),
section **“Clone the repository with RStudio”** to clone the repository.
<br>

Once the compendium is cloned, follow these steps:

1.  open the `.Rproj` file in RStudio.
2.  open [make.R](make.R) and run it to produce all analyses.

Note that R version 3.6.2 or later is required. Run this command
`utils::sessionInfo()` to check your R version. <br> You can uptade your
R version [here](https://cran.r-project.org).

### Session Info

``` r
utils::sessionInfo()
#> R version 4.1.0 (2021-05-18)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19042)
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
#>  [1] compiler_4.1.0    magrittr_2.0.1    tools_4.1.0       htmltools_0.5.1.1
#>  [5] yaml_2.2.1        stringi_1.7.3     rmarkdown_2.9     knitr_1.33       
#>  [9] stringr_1.4.0     xfun_0.24         digest_0.6.27     rlang_0.4.11     
#> [13] evaluate_0.14
```

### Licenses

**Text and figures:**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code:** See the [DESCRIPTION](DESCRIPTION) file

**Data:** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse
