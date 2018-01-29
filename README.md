#### Package downloads and build status</b>

##### Downloads from the [RStudio CRAN Mirror](http://cran-logs.rstudio.com/)

This month      | In total
--------------- | -----------
![month](http://cranlogs.r-pkg.org/badges/gimms) | ![total](http://cranlogs.r-pkg.org/badges/grand-total/gimms)

##### Build status

CRAN | Travis-CI (master) | Travis-CI (devel)
---- | ------------------ | -----------------
[![](http://www.r-pkg.org/badges/version/gimms)](http://www.r-pkg.org/pkg/gimms) | [![](https://travis-ci.org/environmentalinformatics-marburg/gimms.svg?branch=master)](https://travis-ci.org/environmentalinformatics-marburg/gimms) | [![](https://travis-ci.org/environmentalinformatics-marburg/gimms.svg?branch=devel)](https://travis-ci.org/environmentalinformatics-marburg/gimms)

<hr>

#### Introducing the 'gimms' package

... is an [open-access tutorial](https://www.gitbook.com/book/envin-marburg/introducing-the-r-gimms-package/details) about the **gimms** package which has been developed using [GitBook](https://www.gitbook.com/). The book will regularly be updated as **gimms** develops further, so make sure to check it out every now and then!

<hr>

#### What's new?

##### 2018-01-13, **gimms** 1.1.0 is now on CRAN

As of 2018-01-13, the next minor release of **gimms** has finally arrived on CRAN. Check out [NEWS](https://github.com/environmentalinformatics-marburg/gimms/blob/master/NEWS) for a full list of changes. In addition, note that the accompanying [GitBook](https://www.gitbook.com/book/envin-marburg/introducing-the-r-gimms-package/details) will be updated (and hopefully extended) soon.

<hr>

##### 2017-01-02, "traditional" NDVI3g.v0 names from new NDVI3g.v1 files via `oldNaming`
For all users who prefer to work with the now outdated NDVI3g.v0 file names, I've added a function called `oldNaming` to the 'develop' branch. It takes a vector of .nc4 file names as input and transforms them to traditional half-monthly file names, optionally appending a suffix *e.g.* in preparation for `writeRaster`. As this is not on CRAN yet, remember to install the 'develop' version via 
```r
devtools::install_github("environmentalinformatics-marburg/gimms", 
                         ref = "develop")
```
to be able to use that function in the first place.

<hr>

##### 2016-12-17, **gimms** 1.0.0 is now on CRAN
I am happy to announce that the brand-new package update (v1.0.0) has successfully been built for all platforms and is now available from [CRAN](https://cran.r-project.org/package=gimms). Among the major improvements are:

* comprehensive support for the recently released GIMMS NDVI3g.v1 which comes as half-yearly NetCDF container files and spans the period until December 2015. For reasons of convenience, continuing support for NDVI3g.v0 ENVI binary files is maintained. 
* quality control is now directly available through `rasterizeGimms`. In order to make sure older scripts are still operable, separate calls to `qualityControl` are possible, but explicitly require the specification of a 2-layered `RasterStackBrick` object (NDVI and flags).
* `rasterizeGimms` further takes an optional argument 'ext' which is passed to `raster::crop` which, if used, drastically reduces computation times. At the same time, the application of a scale factor and the rejection of 'mask-water' and 'mask-nodata' values is no longer optional.
* parallel processing is no longer realized through **foreach** (alongside with **doParallel**), but instead relies on the built-in **parallel** package only. Therefore, the former two are no longer part of the package Imports section.
* et cetera

<hr>

##### 2016-01-15, **gimms** 0.5.0 is now on CRAN
As of today, **gimms** 0.5.0 is available from [CRAN](https://cran.r-project.org/package=gimms) and has some new functionality:

* enabled flag support in `rasterizeGimms`. In addition to the raw and scaled values of NDVI3g, the function now optionally returns flag layers which can subsequently be used for quality control. Please refer to the official [README](ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00READMEgeo.txt) for further reading. 
* improved performance of parallel processing.
* revised package documentation.

<hr>

##### 2015-12-16, added parallel support
I decided to add optional multi-core support to `downloadGimms`, `rasterizeGimms` and `monthlyComposite`. The referring arument is called 'cores' and, if not specified otherwise, defaults to 1 (i.e., parallel computing is disabled). In the course of this, the **gimms** package version on branch 'develop' has been incremented to 0.4.0 and can be installed via `devtools::install_github` (see further below).

<hr>

##### 2015-11-13, **gimms** 0.3.0 is now on CRAN
It's Friday 13th and an updated version of the **gimms** package has been published on [CRAN](https://CRAN.R-project.org/package=gimms). The new version includes

* `significantTau` to calculate pixel-based (and optionally pre-whitened) Mann-Kendall trend tests from a previously processed GIMMS NDVI<sub>3g</sub> (or any kind of) 'RasterStack/Brick' object. Note that it also works with simple 'numeric' vectors (i.e., univariate time series observations);
* the below compatibility update of `downloadGimms` that enabled 'Date' input;
* and some minor bug-fixes.

<hr>

##### 2015-11-11, `downloadGimms` now works with 'Date' input
In response to recent user suggestions, I decided to enable 'Date' input for `downloadGimms` which grants the user a finer control over the temporal coverage of the data to be downloaded. The changes are currently available from the 'develop' branch via 


```r
devtools::install_github("environmentalinformatics-marburg/gimms", 
                         ref = "develop")
```

and will be submitted to CRAN soon.

<hr>
##### 2015-11-06, pre-whitened Mann-Kendall trend test via `significantTau`
In order to account for lag-1 autocorrelation when trying to deduce reliable long-term monotonous trends, **gimms** now features a function called `significantTau`. The code imports the standard (i.e., without pre-whitening) procedure included in package **Kendall** (McLeod, 2011) or, if the user decides to apply pre-whitening prior to the actual trend test, one of the algorithms included in **zyp** (Bronaugh and Consortium, 2013). Check out `?significantTau` for further details. 

<hr> 
##### 2015-10-26, `downloadGimms` now works properly on Windows
I recently received a bug report about some strange behavior of `downloadGimms` (when working on Windows platforms) which resulted in a rather awkward look of the rasterized images. 

<center>
  <img src="http://i.imgur.com/MySaI9F.png" alt="windows_bug" style="width: 650px;"/>
</center>

The problem was obviously related to `download.file` which worked just fine on Linux when using the default settings, but introduced distortions on Windows. In the newest package version 0.2.0 which is now brand-new on [CRAN](https://CRAN.R-project.org/package=gimms), I therefore specified `download.file(..., mode = "wb")` to explicitly enable binary writing mode. 

Thanks again for the input! 

<hr>

