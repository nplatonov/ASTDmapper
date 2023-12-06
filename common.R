if (!require(ursa)) {
   stop("Needs required packages 'digest', 'ursa' and 'fasterize' and optional package 'qs'")
}
release <- FALSE # (digest::digest(Sys.getenv("USERNAME"),"crc32")=="423c1bf6") ## devel
if (release) {
   separator <- ";"
   dateTimeFormat <- "%Y-%m-%d %H:%M:%S"
   pamefile <- "LME_2013_polygon.shp"
   landfile <- "C:\\Boris\\PAMPAN\\compatibility\\ASTD work\\simplified-land-polygons-complete-3857\\simplified-land-polygons-complete-3857\\simplified_land_polygons.shp"
   aoifile <- "C:\\Boris\\PAMPAN\\compatibility\\ASTD work\\LME_2013_polygon.shp"
   gridfile <- "C:\\Boris\\PAMPAN\\compatibility\\ASTD work\\LME_2013_grid.shp.zip"
   gridCRS <- 6931
   gridCellsize <- 5000
   vmax <- 60 ## km/h
   occasionalEnries <- 10L
} else {
   separator <- ","
   dateTimeFormat <- "%d/%m/%Y %H:%M"
   pamefile <- "LME_2013_polygon.shp"
   aoifile <- "study_area.sqlite"
   gridfile <- "LME_2013_grid.sqlite"
   landfile <- "simplified_land_polygons.shp"
   gridCRS <- 6931
   gridCellsize <- 20000
   vmax <- 12 ## km/h
   occasionalEnries <- 10L
}
stopifnot(length(spatial_dir(pamefile))==1L)
stopifnot(length(spatial_dir(landfile))==1L)
prelimDrop <- FALSE
devel <- FALSE
'interim_read' <- function(patt,list=TRUE,verbose=FALSE) {
   res <- NULL
   if (length(list1 <- spatial_dir(paste0(patt)))>0) {
      if (verbose)
         print(list1)
      ursa:::.elapsedTime("spatial read -- start")
      res <- lapply(list1,spatial_read)
      ursa:::.elapsedTime("spatial read -- finish")
   }
   if (!is.list(res))
      requireNamespace("sf")
   if (!is.list(res)) {
      if (length(list1 <- dir(pattern=paste0(patt,"\\.qs$")))>0) {
         if (verbose)
            print(list1)
         ursa:::.elapsedTime("qs read -- start")
         res <- try(lapply(list1,qs::qread))
         ursa:::.elapsedTime("qs read -- finish")
      }
   }
   if (!is.list(res)) {
      if (length(list1 <- dir(pattern=paste0(patt,"\\.rds$")))>0) {
         if (verbose)
            print(list1)
         ursa:::.elapsedTime("RDS read -- start")
         res <- lapply(list1,readRDS)
         ursa:::.elapsedTime("RDS read -- finish")
      }
   }
   if (!is.list(res)) {
      if (length(list1 <- dir(pattern=paste0(patt,"\\.csv(\\.gz)*$")))>0) {
         if (verbose)
            print(list1)
         ursa:::.elapsedTime("CSV read -- start")
         res <- lapply(list1,\(fname) {
            a <- read.csv(fname,sep=";")
            a <- sf::st_as_sf(a,coords=c("longitude","latitude"),crs=4326)
            a$date_time_utc <- as.POSIXct(a$date_time_utc,tz="UTC")
            a
         })
         ursa:::.elapsedTime("CSV read -- finish")
      }
   }
   if (!is.list(res))
      stop("TODO for reading interim files")
   res
}
'interim_write' <- function(obj,fname) {
   if (FALSE) {
      ursa:::.elapsedTime("spatial write -- start")
      ret <- spatial_write(obj,paste0(fname,".sqlite"),compress=TRUE)
      ursa:::.elapsedTime("spatial write -- finish")
      return(ret)
   }
   ursa:::.elapsedTime("qs write -- start")
   ret <- try(qs::qsave(obj,paste0(fname,".qs")))
   ursa:::.elapsedTime("qs write -- finish")
   if (inherits(ret,"try-error")) {
      ursa:::.elapsedTime("rds write -- start")
      ret <- saveRDS(obj,paste0(fname,".rds"))
      ursa:::.elapsedTime("rds write -- finish")
   }
   ret
}
