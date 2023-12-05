
# ASTDmapper

Scripts for processing ASTD AIS data

## DISCLAIMER

Under license agreement, only fictive AIS data is used here.

## Pre-requirements

### Land mask

Land is used for terrestrial masking of marine traffic.

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

These [PAME](https://pame.is/) LME regions are used for defining of study area.

Please download them from [PAME LME project](https://pame.is/projects/ecosystem-approach/arctic-large-marine-ecosystems-lme-s) using "Download" button or try to use this [direct link](https://pame.is/document-library/ecosystem-approach-to-management-documents/large-marine-ecosystems/384-lme-shapefile-zip/file).

Extract only ESRI shapefile polygons to current directory.

+ `LME_2013_polygon.dbf`
+ `LME_2013_polygon.prj`
+ `LME_2013_polygon.shp`
+ `LME_2013_polygon.shx`

Optionally, to save disk space, zip them (with junked paths) to `LME_2013_polygon.shp.zip` filename. 

### R-packages

Required packages are **`ursa`**, **`fasterize`**, **`digest`**, **`argosfilter`**. Optional package is **`qs`**.

```{r, eval=FALSE}
install.packages(c("ursa","fasterize","digest","argosfilter"),repos="https://cloud.r-project.org")
# install.packages(c("qs"),repos="https://cloud.r-project.org")
```

## Prepare study area

Use R-script

+ `10_define_study_area.R`

to cut land areas. This is spatial difference between geometries of PAME LME polygons and land polygons. 

Output is gz-compressed `study_area.sqlite`.

## Define grid

Use R-script

+ `20_create_grid.R`

to define grid cells. Parameters of geographical projection (Coordinate Reference System, CRS) and square cell resolution are specified in `crs` and `res` variables.

```{r, eval=FALSE}
crs <- 6931
res <- 20000
```

CRS is either EPSG code (`6931`, `"EPSG6931"`) or proj4 string (`"+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"`). Cell resolution is in meters.

Output is gz-compressed `LME_2013_grid.sqlite`.


## Processing by seasons

Use R-script

+ `30_processing_by_seasons.R`

for data filtering and map producing.

### Parameterization

Two branches (or, user profiles) are managed by `release` variable. If `release` is `FALSE`, then fictive AIS data `Phistachos_import-export.csv` is used. In production, use `TRUE`. 

In source: 

```{r, eval=FALSE}
release <- FALSE
```

You need specify explicitly, `release <- TRUE` or `release <- FALSE`.

+ `aisfile` - filename of AIS data in comma-separated format. It can be compressed (gzip, bzip2, xz). . File extension for compressed file (e.g., \*.gz) can be omitted.
+ `separator` - separator for `aisfile` comma-separated file
+ `dateTimeFormat` - date + time format for conversion character time to POSIX time.
+ `aoifile` - filename for study area (area of interest, AOI), in GDAL format with polygonal geometry. For compressed file (zip, gzip, bzip2, xz) compression file extension (e.g., \*.zip, \*.gz) can be omitted. In this repository `study_area.sqlite` is used and is produced by `10_define_study_area.R` script.
+ `gridfile` - filename for grid cells, in GDAL format with polygonal geometry. For compressed file (zip, gzip, bzip2, xz) compression file extension (e.g., \*.zip, \*.gz) can be omitted. In this repository `LME_2013_grid.sqlite` is used and is produced by `20_create_grid.R` script.
+ `vmax` - maximal allowed vessel speed, in kilometers per hour.
+ `occasionalEnries` - minimal number of AIS positions for each vessel to avoid excepting from processing.

```{r eval=FALSE}
if (release) {
   separator <- ";"
   dateTimeFormat <- "%Y-%m-%d %H:%M:%S"
   aisfile <- "C:\\Boris\\PAMPAN\\compatibility\\ASTD work\\ASTD_area_level3_202302.csv"
   aoifile <- "C:\\Boris\\PAMPAN\\compatibility\\ASTD work\\LME_2013_polygon.shp"
   gridfile <- "C:\\Boris\\PAMPAN\\compatibility\\ASTD work\\LME_2013_grid.shp.zip"
   vmax <- 60 ## km/h
   occasionalEnries <- 10L
} else {
   separator <- ","
   dateTimeFormat <- "%d/%m/%Y %H:%M"
   aisfile <- "./Phistachos_import-export.csv"
   aoifile <- "study_area.sqlite"
   gridfile <- "LME_2013_grid.sqlite"
   vmax <- 12 ## km/h
   occasionalEnries <- 10L
}
```

### Processing steps

+ Source AIS data is transformed to spatial structure under simple features standards.

+ All locations outside of study area are excluded.

+ Speed filter ([Freitas et al., 2008](https://dx.doi.org/10.1111/j.1748-7692.2007.00180.x)) is applied to remove spikes in trajectory using speed threshold.

+ Weighted cell aggregating for individual vessels based on attribute `sec_nextpoint`, which is interpreted as a residence time for given location.

+ Non-weighted cell aggregation for all vessels. Units are "days per period". It is expected monthly periods, and units are "days per month".
