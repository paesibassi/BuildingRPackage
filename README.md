# BuildingRPackage
## Build R package assignment, FARS functions

This package has been built to complete the assignment for the Coursera R Packages course.

It contains some functions that extract data from csv files, aggregate and show observations and plot them on a map.

Travis shield: [![Travis-CI Build Status](https://travis-ci.org/paesibassi/BuildingRPackage.svg?branch=master)](https://travis-ci.org/paesibassi/BuildingRPackage)

AppVeyor shield: [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/paesibassi/BuildingRPackage?branch=master&svg=true)](https://ci.appveyor.com/project/paesibassi/BuildingRPackage)

### Instructions

The purpose of this assessment is for you to combine your skills of creating, writing, documenting, and testing an R package with releasing that package on GitHub. In this assessment you'll be taking the R files from Week 2's assessment about documentation and putting that file in an R package.

For this assessment you must:
1. write a vignette to include in your package using knitr and R Markdown
2. write at least one test written using testthat
3. put your package on GitHub
4. set up the repository so that the package can be checked and built on Travis
5. Once your package has built on Travis and the build is passing with no errors, warnings, or notes you should add your Travis badge to the README.md file of your package repository.

#### Review criteria
For this assignment you'll submit a link to the GitHub repository which contains your package. This assessment will ask reviewers the following questions:
1. Does this package contain the correct R file(s) under the R/ directory?
2. Does this package contain a man/ directory with corresponding documentation files?
3. Does this package contain a vignette which provides a meaningful description of the package and how it should be used?
4. Does this package have at least one test included in the tests/ directory?
5. Does this package have a NAMESPACE file?
6. Does the README.md file for this directory have a Travis badge?
7. Is the build of this package passing on Travis?
8. Are the build logs for this package on Travis free of any errors, warnings, or notes?
