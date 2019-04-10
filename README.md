# stemmatology : an R Stemmatology Package

[![Travis-CI Build Status](https://travis-ci.org/Jean-Baptiste-Camps/stemmatology.svg?branch=master)](https://travis-ci.org/Jean-Baptiste-Camps/stemmatology) 
[![Coverage Status](https://img.shields.io/codecov/c/github/Jean-Baptiste-Camps/stemmatology/master.svg)](https://codecov.io/github/Jean-Baptiste-Camps/stemmatology?branch=master) 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/stemmatology)](https://cran.r-project.org/package=stemmatology)
[![DOI](https://zenodo.org/badge/21941228.svg)](https://zenodo.org/badge/latestdoi/21941228)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/stemmatology)](https://cran.r-project.org/package=stemmatology)



    Package: stemmatology
    Type: Package
    Title: Stemmatological Analysis of Textual Traditions
    Version: 0.3.2
    Date: 2019-03-28
    Author: Jean-Baptiste Camps ; Florian Cafiero
    Maintainer: Jean-Baptiste Camps <jbcamps@hotmail.com>
    Description: Explore and analyse the genealogy of textual or musical traditions, from their variants, with various stemmatological methods, mainly the disagreement-based algorithms suggested by Camps and Cafiero (2015) <doi:10.1484/M.LECTIO-EB.5.102565>.
    BugReports: https://github.com/Jean-Baptiste-Camps/stemmatology/issues
    Imports: graphics, stats, cluster, igraph, xml2
    Suggests: 
        testthat,
        knitr,
        rmarkdown,
        covr
    License: GPL-3 | file LICENSE
    Encoding: UTF-8
    NeedsCompilation: no
    URL: https://github.com/Jean-Baptiste-Camps/stemmatology
    VignetteBuilder: knitr
    RoxygenNote: 6.0.1


This repository contains the source file of the development version for the project of a stemmatology package for R. This package already contains functions for the PCC method as described in Camps & Cafiero 2015 (see below, section _On the Method_), and will implement as well  other stemmatological methods (see roadmap at https://graal.hypotheses.org/925).

## Installation

You can install stemmatology from CRAN, with:
```r
install.packages("stemmatology")
```

or, alternatively, if you want the last version,
you can install from Github with:

```r
# install.packages("devtools")
devtools::install_github("Jean-Baptiste-Camps/stemmatology")
```

## Utils

Conversion utils, including a TEI_app to csv conversion stylesheet, are 
available on another repository: 
[https://github.com/Jean-Baptiste-Camps/stemmatology-utils](stemmatology-utils).

## Documentation and papers

For the documentation of the main functions, once you have loaded `stemmatology`, have a look at the R documentation,
by typing
````r
?'stemmatology-package'
````

### On the Software

- Camps, Jean-Baptiste, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2), edited by Andrew U. Frank et al., 2018, pp. 65–74.

The paper and the poster can be found in the `inst/doc` directory.

### On the Method

- Camps, Jean-Baptiste, and Florian Cafiero. ‘Genealogical Variant Locations and Simplified Stemma: A Test Case’. Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches, edited by Tara Andrews and Caroline Macé, Brepols, 2015, pp. 69–93, [https://halshs.archives-ouvertes.fr/halshs-01435633](https://halshs.archives-ouvertes.fr/halshs-01435633), DOI: [http://dx.doi.org/10.1484/M.LECTIO-EB.5.102565](10.1484/M.LECTIO-EB.5.102565).
