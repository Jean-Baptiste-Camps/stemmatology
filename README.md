# stemmatology : an R stemmatology package

[![Travis-CI Build Status](https://travis-ci.org/Jean-Baptiste-Camps/stemmatology.svg?branch=master)](https://travis-ci.org/Jean-Baptiste-Camps/stemmatology) 
[![Coverage Status](https://img.shields.io/codecov/c/github/Jean-Baptiste-Camps/stemmatology/master.svg)](https://codecov.io/github/Jean-Baptiste-Camps/stemmatology?branch=master) 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/stemmatology)](https://cran.r-project.org/package=stemmatology)
[![DOI](https://zenodo.org/badge/21941228.svg)](https://zenodo.org/badge/latestdoi/21941228)


    Version: 0.2.2 (pre-alpha)
    Date: 2014-07-16
    Author: Jean-Baptiste Camps ; Florian Cafiero
    Maintainer: complain to <jbcamps@hotmail.com>, <florian.cafiero@polytechnique.edu>
    Description: This package helps building and analysing the genealogy of textual traditions.
    License: GPL-3

This repository contains the source file of the development version for the project of a stemmatology package for R. This package will contain functions for the PCC method as described in Jean-Baptiste Camps And Florian Cafiero, « Genealogical Variant Locations And Simplified Stemma: A Test Case », 2014, as well as other stemmatological methods (see roadmap at https://graal.hypotheses.org/925).

## Installation

You can install stemmatology from Github with:

```r
# install.packages("devtools")
devtools::install_github("Jean-Baptiste-Camps/stemmatology")
```

## Utils

Conversion utils, including a TEI_app to csv conversion stylesheet, are 
available on another repository: 
[https://github.com/Jean-Baptiste-Camps/stemmatology-utils](stemmatology-utils).

