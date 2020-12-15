
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
