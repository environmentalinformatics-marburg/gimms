## gimms 1.2.0.9002 (2021-07-27)

#### 🐛 bug fixes

  * Read `"poles"` FTP download info directly from website to avoid problems associated with changed username or password (#4).

#### 💬 documentation etc

  * Turned on markdown support for roxygen


## gimms 1.2.0

New features:

  * File retrieval from A Big Earth Data Platform for Three Poles provided by 
The National Center for Atmospheric Research at http://poles.tpdc.ac.cn/en/data/9775f2b4-7370-4e5e-a537-3482c9a83d88/.
  * Initialized 'tinytest' unit testing (tbc).

Changes:

  * Server order has changed and now defaults to `c("poles", "nasanex", "ecocast")`
  * In case server is unavailable, updateInventory() returns offline files list for target server straight away without testing alternate servers first.


## gimms 1.1.3

Changes:

* Handle expired ECOCAST SSL certificate in updateInventory().
* gimms:::updateNasanex() now yields valid online filepaths.


## gimms 1.1.2

Changes:

* Removed 'RCurl' dependency.


## gimms 1.1.1

Changes:

* Roxygen2 update
* Use DOI hyperlinks rather than direct journal links due to deprecation issues.
* Rasterized NDVI3g.v0 images are no longer kept in memory, but linked to corresponding file on disk if 'filename' is specified.


## gimms 1.1.0

New features:

  * Function oldNaming() to transform new .nc4 file names into "traditional" NDVI3g.v0 naming convention, optionally appending a suffix (e.g., '.tif').
  * Added parallel support to significantTau(),'RasterStackBrick'-method.

Changes:

* Issued MIT license.  


## gimms 1.0.0

New features: 

  * Added support for the recently released NDVI3g.v1 to all package functions.
  * Included local file inventories for all available GIMMS products (NDVI3g.v0 and NDVI3g.v1) and their respective servers (ECOCAST, NASANEX).
  * rasterizeGimms() now includes an extent argument ('ext') to enable clipping based on raster::crop().

Changes:

  * Quality control has been added to rasterizeGimms() through specifying permitted flag values ('keep'), rendering the subsequent application of a separate function unnecessary. For reasons of convenience, however, qualityControl() is still available.
  * Scaling and the rejection of water and nodata-mask values are no longer optional.
  * If 'timestamp = TRUE', monthlyIndices() now returns an object of class 'Date' (instead of 'character') and '...' is no longer available.


## gimms 0.5.0

New features:

  * Enabled retrieval of quality flags from rasterizeGimms().
  * Added parallel support to downloadGimms(), rasterizeGimms() and monthlyComposite().

Changes:

  * Revised package documentation.

