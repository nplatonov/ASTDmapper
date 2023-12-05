# ASTDmapper

Scripts for processing ASTD AIS data

## Pre-requirements

### Land mask

Land is used for terrestial masking of marine traffic 

[Land polygons](https://osmdata.openstreetmap.de/data/land-polygons.html) of the [Data Derived from OpenStreetMap for Download](https://osmdata.openstreetmap.de/) project is used for land mask.

Please download "Large simplified polygons" (the most bottom "Download" button). Or use this [direct link](https://osmdata.openstreetmap.de/download/simplified-land-polygons-complete-3857.zip).

Extract full set of ESRI Shapefiles to current directory.

+ `simplified_land_polygons.cpg`
+ `simplified_land_polygons.dbf`
+ `simplified_land_polygons.prj`
+ `simplified_land_polygons.shp`
+ `simplified_land_polygons.shx`

Optionally, to save disk space, zip them (with junked paths) to `simplified_land_polygons.shp.zip` filename. 

### Large Marine Ecosystems (LME's) of the Arctic

These regions are used for defining of study area.

Please download them from [PAME LME project](https://pame.is/projects/ecosystem-approach/arctic-large-marine-ecosystems-lme-s) using "Download" button or try to use this [direct link](https://pame.is/document-library/ecosystem-approach-to-management-documents/large-marine-ecosystems/384-lme-shapefile-zip/file).

Extract only ESRI shapefile polygons to current directory.

+ `LME_2013_polygon.dbf`
+ `LME_2013_polygon.prj`
+ `LME_2013_polygon.shp`
+ `LME_2013_polygon.shx`

Optionally, to save disk space, zip them (with junked paths) to `LME_2013_polygon.shp.zip` filename. 

### R-packages

Required packages are **`ursa`**, **`fasterize`**, **`digest`**. Optional package is **`qs`**.

```{r}
install.packages(c("ursa","fasterize","digest"),repos="https://cloud.r-project.org")
# install.packages(c("qs"),repos="https://cloud.r-project.org")
```

## Prepare study area

Use R-script `10_define_study_area.R`.

