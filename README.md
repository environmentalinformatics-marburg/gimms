

#### Package downloads from the [RStudio CRAN Mirror](http://cran-logs.rstudio.com/)</b>

This month      | In total
--------------- | -----------
![month](http://cranlogs.r-pkg.org/badges/gimms) | ![total](http://cranlogs.r-pkg.org/badges/grand-total/gimms)

<hr>


#### What's new?

I recently received an invaluable bug report about some strange behavior of `downloadGimms` and a rather awkward look of the rasterized images resulting therefrom. 

<center>
  <img src="http://i.imgur.com/MySaI9F.png" alt="windows_bug" style="width: 650px;"/
</center>

The problem was obviously related to `download.file` which worked just fine under Linux when using the default settings but introduced distortions under Windows. In the newest package version which is currently on GitHub (install via `devtools::install_github("environmentalinformatics-marburg/gimms", ref = "develop")`) and (hopefully) soon on CRAN, I therefore specified `download.file(..., mode = "wb")` to explicitly enable binary writing mode. 

Thanks again for the input! 


# Introducing the R 'gimms' package

### What it is all about
We've been collecting functions related to the download and processing of AVHRR 
GIMMS data for quite some time and with the most recent update to NDVI3g 
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
The **gimms** package is now officially on CRAN and can be install directly via 


```r
# ## install 'gimms' package
# install.packages("gimms")

## load 'gimms' package
library(gimms)
```

If you wish to install the development version including latest bug-fixes etc. 
instead (no liability assumed!), directly install the package from 
[GitHub](https://github.com/environmentalinformatics-marburg/gimms) via 
`install_github` from the **devtools** package (Wickham and Chang, 2015).


```r
# ## install 'gimms' package
# library(devtools)
# install_github("environmentalinformatics-marburg/gimms", ref = "develop")

## load 'gimms' package
library(gimms)
```

There is no 'development' branch yet, but be assured that such a thing will 
possibly be opened in the near-distant future given that there is a need for 
further functionality, improvements, bug-fixes etc. 

### List available files
For any subsequent processing steps, it is helpful to know which GIMMS files are 
currently hosted on the [ECOCAST servers]((http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/). 
`updateInventory` has been designed just for that purpose as it imports the file 
inventory stored online in *00FILE-LIST.txt* as 'character' vector directly into 
R. By setting `sort = TRUE`, it is even possible to return the output sorted by 
date which is anything but intuitive when dealing with naming conventions in the 
form of 'geo83sep15a.n07-VI3g'). If there is no active internet connection 
available, `updateInventory` automatically imports the latest offline version of 
the file inventory which is stored (and regularly updated) in 
'inst/extdata/inventory.rds'. Additionally setting `sort = TRUE` tells the 
function to return the list of available files sorted by date in ascending order.


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

##### 'missing' input = download entire collection
Specifying no particular input is possibly the most straightforward way of data 
acquisition. The function will automatically start to download the entire 
collection of files (currently July 1981 to December 2013) and store the data in 
`dsn`. It is up to the user's judgement to set `overwrite = TRUE` which would 
tell R to overwrite previously downloaded data located in `dsn`. In most cases, 
however, such a behavior is not particularly desirable, and instead setting 
`overwrite = FALSE` would simply tell R to skip the currently processed file.


```r
## download entire gimms collection
downloadGimms(dsn = paste0(getwd(), "/data"))
```

##### 'numeric' input = download temporal range
It is also possibly to specify a start year (`x`) and/or end year (`y`) to 
limit the temporal coverage of the datasets to be downloaded. In case `x` 
(or `y`) is missing, data download will automatically start from the first (or 
finish with the last) year available. 


```r
## download gimms data from 1998-2000
downloadGimms(x = 1998, y = 2000, 
              dsn = paste0(getwd(), "/data"))
```

##### 'character' input = download particular files
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
matching a particular `pattern` (typically starting with the default setting 
"^geo"). Importing a sorted vector of already downloaded files from 2013, for 
instance, would work as follows.


```r
rearrangeFiles(dsn = paste0(getwd(), "/data"), 
               pattern = "^geo13")
```


```
##  [1] "geo13jan15a.n19-VI3g" "geo13jan15b.n19-VI3g" "geo13feb15a.n19-VI3g"
##  [4] "geo13feb15b.n19-VI3g" "geo13mar15a.n19-VI3g" "geo13mar15b.n19-VI3g"
##  [7] "geo13apr15a.n19-VI3g" "geo13apr15b.n19-VI3g" "geo13may15a.n19-VI3g"
## [10] "geo13may15b.n19-VI3g" "geo13jun15a.n19-VI3g" "geo13jun15b.n19-VI3g"
## [13] "geo13jul15a.n19-VI3g" "geo13jul15b.n19-VI3g" "geo13aug15a.n19-VI3g"
## [16] "geo13aug15b.n19-VI3g" "geo13sep15a.n19-VI3g" "geo13sep15b.n19-VI3g"
## [19] "geo13oct15a.n19-VI3g" "geo13oct15b.n19-VI3g" "geo13nov15a.n19-VI3g"
## [22] "geo13nov15b.n19-VI3g" "geo13dec15a.n19-VI3g" "geo13dec15b.n19-VI3g"
```

### Create a header file
In order to import the GIMMS binary files into R via `raster::raster`, the 
creation of header files (.hdr) that are located in the same folder as the 
binary files staged for processing is mandatory. The standard files required to 
properly process GIMMS NDVI3g data are created via `createHeader` and typically 
include the following parameters. 


```
## [1] "ENVI"
## [1] "description = { R-language data }"
## [1] "samples = 2160"
## [1] "lines = 4320"
## [1] "bands = 1"
## [1] "data type = 2"
## [1] "header offset = 0"
## [1] "interleave = bsq"
## [1] "byte order = 1"
```

It is possibly to automatically remove the created header files by setting 
`rasterizeGimms(..., remove_header = TRUE)` once all operations have finished. 
Although `rasterizeGimms` automatically invokes `createHeader`, the function also 
runs as stand-alone version.


```r
## create gimms ndvi3g standard header file
gimms_header <- createHeader("~/geo13jul15a.n19-VI3g")

gimms_header
```

```
## [1] "~/geo13jul15a.n19-VI3g.hdr"
```

```r
readLines(gimms_header)
```

```
## [1] "ENVI"                              "description = { R-language data }"
## [3] "samples = 2160"                    "lines = 4320"                     
## [5] "bands = 1"                         "data type = 2"                    
## [7] "header offset = 0"                 "interleave = bsq"                 
## [9] "byte order = 1"
```

### Rasterize downloaded data
`rasterizeGimms` is possibly the core part of the **gimms** package as it 
transforms the downloaded GIMMS files from native binary format into objects of 
class 'Raster*', which is much easier to handle as compared to simple ENVI 
files. The function works with both single and multiple files passed on to `x` 
and, in the case of the latter, returns a 'RasterStack' rather than a single 
'RasterLayer'. It is up to the user to decide whether or not to discard 
'mask-water' values (-10,000) and 'mask-nodata' values (-5,000) (see also the 
[official README](http://ecocast.arc.nasa.gov/data/pub/gimms/3g.v0/00READMEgeo.txt)). 
Also, the application of the scaling factor (1/10,000) is not mandatory. Taking 
the above set of `gimms_files` (Jul-Dec 2013) as input vector (notice the 
`full.names` argument passed on to `list.files` in the example below), the 
function call looks as follows.


```r
## list available files
gimms_files <- rearrangeFiles(dsn = paste0(getwd(), "/data"), 
                              pattern = "^geo13", full.names = TRUE)

## rasterize files
gimms_raster <- rasterizeGimms(gimms_files)
```



Since this operation usually takes some time, we highly recommend to make use of 
the `filename` argument that automatically invokes `raster::writeRaster`. With a 
little bit of effort and the help of **RColorBrewer** 
(Neuwirth, 2014) and **spplot** (Pebesma and Bivand, 2005; Bivand, Pebesma, and Gomez-Rubio, 2013), it 
is now easy to check whether everything worked out fine.


```r
## adjust layer names
names(gimms_raster) <- basename(gimms_files)

## visualize single layers
library(RColorBrewer)
spplot(gimms_raster, 
       at = seq(-0.2, 1, 0.1), 
       scales = list(draw = TRUE), 
       col.regions = colorRampPalette(brewer.pal(11, "BrBG")))
```

<center>
  <img src="http://i.imgur.com/Qr3FuNr.png" alt="spplot" style="width: 800px;"/>
  <br><br><b>Figure 1.</b>Global bi-monthly GIMMS NDVI3g images from July to December 2013.
</center>


### Generate monthly composites
Sometimes, it is required to calculate monthly value composites from the 
bi-monthly GIMMS datasets, e.g. to ensure temporal overlap with some other 
ecological or eco-climatological time series. **gimms** features a function 
called `monthlyComposite` which works both on vectors of filenames and entire 
'RasterStack' objects (ideally returned by `rasterizeGimms`) and calculates 
monthly values based on a user-defined function (e.g. `fun = max` to create 
monthly MVC layers). Needless to say, the function is heavily based on 
`stackApply` from the fabulous **raster** package and assumes numeric vectors 
of monthly `indices` (or text substrings from `pos1` to `pos2` from which to 
deduce such indices, see `?monthlyIndices`) as input variable. The actual code 
work is relatively straightforward.


```r
## .tif files created during the previous step
gimms_files_tif <- sapply(gimms_raster@layers, function(i) attr(i@file, "name"))

## create monthly maximum value composites
gimms_raster_mvc <- monthlyComposite(gimms_files_tif)
```

Again and this time with a little help from **reshape2** 
(Wickham, 2007) and **ggplot2** (Wickham, 2009), 
the effects from `monthlyComposite` can easily be seen. Displayed below are the 
densityplots of all NDVI values during the 1st half of July 1981 (green), during 
the 2nd half of July 1981 (turquoise) and the resulting MVC values (black).


```r
## concatenate data
val <- data.frame("ndvi_15a" = na.omit(getValues(gimms_raster[[1]])), 
                  "ndvi_15b" = na.omit(getValues(gimms_raster[[2]])), 
                  "ndvi_mvc" = na.omit(getValues(gimms_raster_mvc[[1]])))

## wide to long format
library(reshape2)
val_mlt <- melt(val)

## colors
devtools::install_github("environmentalinformatics-marburg/Rsenal")
library(Rsenal)
cols <- envinmrPalette(5)[c(3, 2, 5)]
names(cols) <- levels(val_mlt$variable)

## linetypes
ltys <- c("solid", "solid", "longdash")
names(ltys) <- levels(val_mlt$variable)

## build ggplot
library(ggplot2)
ggplot(aes(x = value, group = variable, colour = variable, 
           linetype = variable), data = val_mlt) + 
  geom_hline(yintercept = 0, size = .5, colour = "grey65") +
  geom_line(stat = "density", size = 1.2) + 
  scale_colour_manual("dataset", values = cols) + 
  scale_linetype_manual("dataset", values = ltys) + 
  labs(x = "\nNDVI", y = "Density\n") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.key.width = grid::unit(1.8, "cm"))
```



<center>
  <img src="http://i.imgur.com/WmTlFyV.png" alt="ggplot" style="width: 650px;"/>
  <br><br><b>Figure 2.</b> Kernel density distribution of GIMMS NDVI3g values during the first (green) and second half of July 2013 (turquoise) and resulting value distribution of the maximum value composite layer (MVC; black). 
</center> 

### Some considerations on code performance
In order to speed things up a little bit, it is quite easy to add multi-core 
functionality to the operations provided by **gimms**. This is particularly 
applicable to `rasterizeGimms` with `raster::writeRaster` option enabled, i.e. 
parameter `filename` specified. When run in parallel, the operation performs 
considerably faster as compared to the base implementation. 


```r
## first, the base version from above
system.time(
  rasterizeGimms(gimms_files, 
                 filename = paste0(gimms_files, ".tif"), overwrite = TRUE)
)
#    user  system elapsed 
#  48.142   3.003  54.535

## next, the parallelized version
rasterizeGimmsParallel <- function(files, nodes = 4, overwrite = FALSE, ...) {

# create and register parallel backend
library(doParallel)
cl <- makeCluster(nodes)
registerDoParallel(cl)

# loop over 'x' and process single files in parallel
ls_rst <- foreach(i = files, .packages = "gimms", 
                  .export = ls(envir = globalenv())) %dopar% {
                    filename <- paste0(i, ".tif")
                    
                    if (overwrite | !file.exists(filename)) {
                      rasterizeGimms(i, filename = filename, overwrite = TRUE, ...)
                    } else {
                      raster(i, crs = "+init=epsg:4326")
                    }
                  }

# deregister parallel backend
closeAllConnections()

# return stacked layers
return(stack(ls_rst))
}

system.time(
  rasterizeGimmsParallel(gimms_files, overwrite = TRUE)
)
#   user  system elapsed 
#  0.142   0.102  29.144
```

In the context of parallel processing, feel free to also browse the advanced 
applications based on GIMMS NDVI3g data below. There are some more examples 
included demonstrating the reasonable use of **doParallel** functionality 
(Analytics and Weston, 2015) along with the **gimms** package, which is 
probably particulary applicable for `downloadGimms` (given that your internet 
connection is fast enough to manage multi-core file downloads).




### Advanced applications
The last section of this brief introduction is meant to demonstrate the use of 
GIMMS NDVI3g data in a more practical sense. Note that all necessary work steps 
are briefly documented as in-line comments. Perhaps it might be interesting for 
some of you...

##### Global Mann-Kendall trend based on GIMMS NDVI3g

```r
################################################################################
## download data
################################################################################

## download entire gimms ndvi3g collection in parallel
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)

gimms_files <- updateInventory(sort = TRUE)
gimms_files <- foreach(i = gimms_files, .packages = "gimms", 
                       .combine = "c") %dopar% downloadGimms(i, dsn = "data/")

stopImplicitCluster()

################################################################################
## rasterize binary files
################################################################################

## rasterize gimms ndvi3g binary files in parallel (see above function 
## definition of `rasterizeGimmsParallel`)
gimms_raster <- rasterizeGimmsParallel(gimms_files, overwrite = TRUE)

## remove incomplete first year
gimms_files <- gimms_files[-(1:12)]
gimms_raster <- gimms_raster[[-(1:12)]]

################################################################################
## resample to a lower spatial resolution (to avoid stack overflow)
################################################################################

## aggregate to a lower spatial resolution
cl <- makeCluster(4)
registerDoParallel(cl)

gimms_raster_agg <- foreach(i = 1:nlayers(gimms_raster), 
                            .packages = c("raster", "rgdal")) %dopar%
  aggregate(gimms_raster[[i]], fact = 3, fun = median, 
            filename = paste0("data/agg/AGG_", names(gimms_raster[[i]])), 
            format = "GTiff", overwrite = TRUE)

gimms_raster_agg <- stack(gimms_raster_agg)

################################################################################
## remove seasonal signal
################################################################################

## calculate long-term bi-monthly means
gimms_list_means <- foreach(i = 1:24, 
                            .packages = c("raster", "rgdal")) %dopar% {
  
  # layers corresponding to current period (e.g. '82jan15a')
  id <- seq(i, nlayers(gimms_raster_agg), 24)
  gimms_raster_agg_tmp <- gimms_raster_agg[[id]]
  
  # calculate long-term mean of current period (e.g. for 1982-2013 'jan15a')
  calc(gimms_raster_agg_tmp, fun = mean, na.rm = TRUE)
} 

gimms_raster_means <- stack(gimms_list_means)

## replicate bi-monthly 'gimms_raster_means' to match up with number of layers of 
## initial 'gimms_raster_agg' (as `foreach` does not support recycling!)
gimms_list_means <- replicate(nlayers(gimms_raster_agg) / nlayers(gimms_raster_means), 
                              gimms_raster_means)
gimms_raster_means <- stack(gimms_list_means)

## subtract long-term mean from bi-monthly values
files_out <- names(gimms_raster_agg)
gimms_list_deseason <- foreach(i = 1:nlayers(gimms_raster_agg), 
                               .packages = c("raster", "rgdal")) %dopar% {
  
  rst <- gimms_raster_agg[[i]] - gimms_raster_means[[i]]
  rst <- writeRaster(rst, 
                     filename = paste0("data/dsn/DSN_", names(gimms_raster_agg[[i]])), 
                     format = "GTiff", overwrite = TRUE)
  
}

gimms_raster_deseason <- stack(gimms_list_deseason)

################################################################################
## mann-kendall trend test (p < 0.001)
################################################################################

## custom function that returns significant values of tau only
library(Kendall)

significantTau <- function(x) {
  mk <- MannKendall(x)
  # reject value of tau if p >= 0.001
  if (mk$sl >= 0.001) {
    return(NA) 
  # keep value of tau if p < 0.001
  } else {
    return(mk$tau)
  }
}

## apply custom function on a pixel basis
gimms_raster_trend <- overlay(gimms_raster_deseason, fun = significantTau, 
                              filename = "data/out/gimms_mk001_8213", 
                              format = "GTiff", overwrite = TRUE)

################################################################################
## visualize data
################################################################################

## complementary shapefile data
library(rworldmap)
data("countriesCoarse")

## colors, see http://colorbrewer2.org/
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))

## create plot
spplot(gimms_raster_trend, col.regions = cols(100), scales = list(draw = TRUE), 
       sp.layout = list("sp.polygons", countriesCoarse, col = "grey65"), 
       at = seq(-.6, .6, .1))
```



<center>
  <img src="http://i.imgur.com/M6sUz6z.png" alt="spplot" style="width: 800px;"/><br><br>
  <b>Figure 3.</b> Long-term trend (1982-2013; <i>p<0.01</i>) in global GIMMS NDVI3g derived from pixel-based Mann-Kendall trend tests (Mann, 1945).
</center>



##### Global vegetation response to the 1997/98 El Niño

