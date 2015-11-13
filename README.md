

#### Package downloads from the [RStudio CRAN Mirror](http://cran-logs.rstudio.com/)</b>

This month      | In total
--------------- | -----------
![month](http://cranlogs.r-pkg.org/badges/gimms) | ![total](http://cranlogs.r-pkg.org/badges/grand-total/gimms)

<hr>

#### Introducing the 'gimms' package

... is an open-access tutorial about the **gimms** package which is now available from [GitBook](https://www.gitbook.com/book/fdetsch/gimmsgitbook/details). The book will be continuously updated as **gimms** develops further, so make sure to check it out regularly!

<hr>

#### What's new?

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

The problem was obviously related to `download.file` which worked just fine on Linux when using the default settings, but introduced distortions on Windows. In the newest package version 0.2.0 which is now brand-new on [CRAN](https://cran.r-project.org/web/packages/gimms/index.html), I therefore specified `download.file(..., mode = "wb")` to explicitly enable binary writing mode. 

Thanks again for the input! 

<hr>

