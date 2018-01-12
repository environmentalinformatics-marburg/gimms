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

