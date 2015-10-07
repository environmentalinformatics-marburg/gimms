

# Introducing the R **gimms** package

### What it is all about
We've been collecting functions related to the download and processing of AVHRR 
GIMMS data for quite some time and with the most recent update to GIMMS3g 
(Pinzon and Tucker, 2014), we thought it was a good time to stuff the most 
fundamental work steps into a proper R package. In this context, 'most 
fundamental' refers to certain operations which we tended to repeat over and 
over again, including

* list all GIMMS files available online at [NASA ECOCAST](http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/),
* download selected (or all) files, 
* re-arrange the list of downloaded files according to date, 
* transform binary data in ENVI format to proper **raster** format (Hijmans, 2015) and 
* aggregate the bi-monthly datasets to monthly maximum value composites (MVC).

In the following, you'll find a short introduction of what we came up with so 
far. Feel free to comment, raise issues and provide (constructive) criticism.
Any suggestions on how to improve the **gimms** package are highly appreciated!

### How to install
So far, the **gimms** package has not been submitted to CRAN but a preliminary 
package version can be installed directly from 
[GitHub](https://github.com/environmentalinformatics-marburg/gimms) via 


```r
# ## install 'gimms' package
# library(devtools)
# install_github("environmentalinformatics-marburg/gimms")

## load 'gimms' package
library(gimms)
```

There is no such thing as a 'development' branch yet, but be assured that such 
things will possibly be provided in the near-distant future given that there is 
a need for further functionality, improvements, bug-fixes etc. 

### List available files
For any further processing, it is helpful to know which GIMMS files are 
currently hosted on the [ECOCAST servers]((http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/). 
`updateInventory` has been designed just for that purpose as it imports the file 
inventory stored online in *00FILE-LIST.txt* as 'character' vector directly into 
R. By setting `sort = TRUE`, it is even possible to return the output sorted by 
date which is anything but intuitive when dealing with naming conventions in the 
form of 'geo83sep15a.n07-VI3g'). If there is no active internet connection 
available, `updateInventory` automatically imports the latest offline version of 
the file inventory which is stored (and regularly updated) in 
'inst/extdata/inventory.rds'.


```r
gimms_files <- updateInventory(sort = TRUE)
```

```
## Trying to update GIMMS inventory from server...
## Online update of the GIMMS file inventory successful!
```

```r
gimms_files[1:10]
```

```
##  [1] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81jul15a.n07-VI3g"
##  [2] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81jul15b.n07-VI3g"
##  [3] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81aug15a.n07-VI3g"
##  [4] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81aug15b.n07-VI3g"
##  [5] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81sep15a.n07-VI3g"
##  [6] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81sep15b.n07-VI3g"
##  [7] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81oct15a.n07-VI3g"
##  [8] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81oct15b.n07-VI3g"
##  [9] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81nov15a.n07-VI3g"
## [10] "http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/1980s_new/geo81nov15b.n07-VI3g"
```

### Download files
With the information about files currently hosted on the remote server at hand, 
the next logical step of the **gimms** processing chain is to download selected 
(if not all) bi-monthly datasets. This can be achieved by running 
`downloadGimms`, but since the function works with different types of input 
parameters (if any), some *ex ante* information is possibly helpful to explain 
the function's proper use.

##### 'missing' input - download entire collection
Specifying no particular input is possibly the most straightforward way to 
perform data download. The function will automatically start to download the 
entire collection of files (currently Jul 1981 to Dec 2013) and store the data 
in `dsn`. It is up to the user's judgement to set `overwrite = TRUE` which would 
tell R to overwrite previously downloaded data located in `dsn`. In most cases, 
however, such a behavior is not particularly desirable.


```r
## download entire gimms collection
downloadGimms(dsn = paste0(getwd(), "/data"))
```

##### 'numeric' input - download temporal range
It is also possibly to specify a start year (`x`) and/or end year (`y`) to 
restrain the temporal coverage of the datasets to be downloaded. In case `x` 
(or `y`) is missing, data download will automatically start from the first (or 
finish with the last) year available. 


```r
## download gimms data from 1998-2000
downloadGimms(x = 1998, y = 2000, 
              dsn = paste0(getwd(), "/data"))
```

##### 'character' input - download particular files
As a third and final possibility to run `downloadGimms`, it is also possible to 
supply a 'character' vector consisting of valid online filepaths. The latter can 
easily be retrieved from `updateInventory` (as demonstrated above) and directly 
passed on to the input argument `x`. 


```r
## download manually selected files
downloadGimms(x = gimms_files[c(98:110, 132)], 
              dsn = paste0(getwd(), "/data"))
```

### Rearrange files
As mentioned above, it is possible to have `updateInventory` return a vector of 
filenames sorted by date in ascending order. Sorting the files surely makes 
sense when it comes to `stack`-ing continuous observations, calculating monthly 
MVC layers and so forth, but is not necessarily required at the initial stage. 
`updateInventory(sort = TRUE)` is merely a wrapper around `rearrangeFiles` 
which may as well be executed as a stand-alone version later on. 

`rearrangeFiles` works in two different ways, either with a 'character' vector 
of (local or online) filenames passed on to `x` or with `list.files`-style 
pattern matching. While the former approach is quite straightforward, the latter 
requires the function to search the folder `dsn` for previously downloaded files 
matchin a particular pattern (`pttrn`; typically starting with the default 
setting "^geo"). Importing a sorted vector of already downloaded files from 
2013, for instance, would work as follows.


```r
gimms_files <- rearrangeFiles(dsn = paste0(getwd(), "/data"), pttrn = "^geo13")
gimms_files
```


```
##  [1] "geo13jul15a.n19-VI3g" "geo13jul15b.n19-VI3g" "geo13aug15a.n19-VI3g"
##  [4] "geo13aug15b.n19-VI3g" "geo13sep15a.n19-VI3g" "geo13sep15b.n19-VI3g"
##  [7] "geo13oct15a.n19-VI3g" "geo13oct15b.n19-VI3g" "geo13nov15a.n19-VI3g"
## [10] "geo13nov15b.n19-VI3g" "geo13dec15a.n19-VI3g" "geo13dec15b.n19-VI3g"
```
