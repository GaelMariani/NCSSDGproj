
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Link bewteen Natural Climate Solutions and Sustainable Development Goals

This repository contains the data and codes used to obtain results of
the paper by Mariani et al. (202?) - Hit two targets with one stone with
Natural Climate Solutions.

## Contents

[:file\_folder: **rawdata**](rawdata/) directory contains the 0/1 matrix
used to perform all analysis. See Material & Methods section of the
paper for more details. This directory also contains SDG and NCS icons
use in figure of the paper.

[:file\_folder: **results**](results/) directory contains all .RData
files generated during the analyses.

[:file\_folder: **R**](R/) directory contains the functions used to
perform all the analyses. It contains 4 .R files:

1.  [:open\_book: read\_data.R](R/read_data.R) contains functions to
    read data.
2.  [:package: format\_data.R](R/format_data.R) contains functions to
    format the data in the desired format to make the analyses and
    figures.
3.  [:microscope: analysis\_data.R](R/format_data.R) contains functions
    to perform analysis, mainly null models and correspondance analysis.
4.  [:bar\_chart: plot\_data.R](R/plot_data.R) contains all functions to
    obtained plots in the paper.

[:file\_folder: **figures**](figures/) directory contains all figures in
the paper.

The [:hammer: dev\_history.R](dev_history.R) file allow to rebuild our
research compendium from scratch for maximum reproducibility.

The [:briefcase: make.R](make.R) file allow to produce all the analyses
and render the corresponding plots.

## How to run it ?

This research compendium has been developed using the statistical
programminglanguage R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium by cloning this
[repository](https://github.com/GaelMariani/NCSSDGproj). You can follow
this
[tutorial](https://docs.github.com/en/free-pro-team@latest/github/creating-cloning-and-archiving-repositories/cloning-a-repository)
to clone the R project. <br>

Once the compendium is cloned, follow these steps: 1. open the `.Rproj`
file in RStudio

2.  open [make.R](make.R) and run it to produce all the analyses.
