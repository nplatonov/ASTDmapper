source("common.R")
if (release) {
   aisfile <- "C:\\Boris\\PAMPAN\\compatibility\\ASTD work\\ASTD_area_level3_202302.csv"
} else {
   aisfile <- "./Phistachos_import-export.csv"
}
aisfile <- dir(path=dirname(aisfile),pattern=basename(aisfile)
              ,recursive=FALSE,full.names=TRUE)
stopifnot(length(aisfile)==1)
stopifnot(length(spatial_dir(aoifile))==1L)
'speedFilter' <- function(src,vmax=vmax*10/36,verbose=FALSE) {
   if (verbose)
      print(spatial_count(src))
   ind2 <- which(!duplicated(src$date_time_utc))
   loc <- src[ind2,]
   if (verbose)
      print(spatial_count(loc))
   if (verbose) {
      loc <- ursa:::spatialize(loc,resetGrid=TRUE)
      session_grid(loc,expand=1.1)
      cl <- compose_coastline(detail="f",fill="#FF000030")
      compose_open(2)
      compose_panel(loc["shipid"],blank="white")
      compose_panel(segmentize(loc),blank="white")
      compose_close()
   }
   if (verbose) {
      tr <- segmentize(loc,connect="consequent")
      tr$dur <- as.numeric(tr$date_time_utc-tr$date_time_utc.1,"hours")
      tr$length <- spatial_length(tr)*1e-3
      tr$speed <- tr$length/tr$dur
      print(summary(tr$dur))
      print(summary(tr$length))
      print(summary(tr$speed))
     # lv <- plutil::lavielle(tr$speed,x=tr$date_time_utc,digits=2) ## for zero speeds
     # print(lv)
      plot(tr$date_time_utc,tr$speed,type="l")
      abline(h=0,col="grey90")
      points(tr$date_time_utc,tr$speed,pch=19,cex=0.3)
   }
   crs <- spatial_crs(loc)
   loc <- spatial_transform(loc,4326)
   xy <- spatial_coordinates(loc)
   a <- data.frame(dtime=loc$date_time_utc,lc="3",lon=xy[,1],lat=xy[,2])
   if (F)
      f <- try(stop("skip sda-filter"))
   else {
      if (verbose)
         cat("argosfilter::sdafilter():\n")
      opW <- options(warn=ifelse(verbose,1,-1))
      f <- try(with(a,argosfilter::sdafilter(lat=lat,lon=lon,dtime=dtime,lc=lc
                                            ,vmax=vmax
                                            ,ang=c(15,25)
                                            ,distlim=c(2500,5000)))
              ,silent=!verbose)
      options(opW)
   }
   if (inherits(f,"try-error")) {
      if (verbose) {
         cat(f)
         cat("argosfilter::vmask():\n")
      }
      opW <- options(warn=ifelse(verbose,1,-1))
      f <- try(with(a,argosfilter::vmask(lat=lat,lon=lon,dtime=dtime,vmax=vmax))
              ,silent=!verbose)
      options(opW)
      if (inherits(f,"try-error")) {
         if (verbose)
            cat(f)
      }
   }
   if (!inherits(f,"try-error")) {
      if (verbose)
         print(table(f))
      ind <- which(f %in% c("not","end_location"))
      if (verbose)
         print(spatial_count(loc[ind,]))
      loc <- loc[ind,]
      if (verbose) {
         tr <- segmentize(loc2,connect="consequent")
         tr$dur <- as.numeric(tr$date_time_utc-tr$date_time_utc.1,"hours")
         tr$length <- spatial_length(tr)*1e-3
         tr$speed <- tr$length/tr$dur
         print(summary(tr$dur))
         print(summary(tr$length))
         print(summary(tr$speed))
         cl <- compose_coastline(detail="f",fill="#FF000030")
         compose_open(2)
         compose_panel(loc2["shipid"],blank="white")
         compose_panel(segmentize(loc2),blank="white")
         compose_close()
      }
   }
   else
      ind <- seq_along(ind2)
   if ("keep" %in% spatial_fields(src)) {
      src$keep[ind2[ind]] <- TRUE
      return(src)
   }
   loc
}
'aoiFilter' <- function(a) {
  # spatial_write(a,"Bananas_import-export.geojson",compress=TRUE)
  # land <- spatial_read(file.path("D:/users/platt/shapefile/auxiliary"
  #                               ,"naturalearth/5.1.2/10m_physical/ne_10m_land.shp"))
   aoi <- spatial_read(aoifile)
  # aoi <- spatial_transform(aoi,a)
   a <- spatial_transform(a,aoi)
   print(c('CRS AIS'=spatial_crs(a)))
   print(c('CRS AOI'=spatial_crs(aoi)))
   print(c('Records before AOI cropping'=spatial_count(a)))
  # a <- spatial_intersection(a,spatial_geometry(aoi))
   a <- a[as.logical(sapply(sf::st_intersects(a,aoi),length)),]
  # a <- a[!a$shipid %in% spatial_intersection(a,aoi)$shipid,]
   print(c('Records after AOI cropping'=spatial_count(a)))
   a
}
'main' <- function() {
   #unique(spatial_area(aoi)*1e-6)
   ursa:::.elapsedTime("AIS reading -- start")
   a <- read.csv(aisfile,sep=separator)
   ursa:::.elapsedTime("AIS reading -- finish")
   colnames(a)[grep("latitude",colnames(a))] <- "latitude"
   a <- a[!is.na(a$longitude) & !is.na(a$longitude),]
   a <- sf::st_as_sf(a,coords=c("longitude","latitude"),crs=4326)
   ursa:::.elapsedTime("AIS is in spatial format now")
  # print(c('AIS CRS'=spatial_crs(a)))
   # summary(spatial_coordinates(a)) |> print()
  # spatial_crs(a) |> print()
   # summary(spatial_coordinates(a)) |> print()
   d3 <- as.POSIXct(a$date_time_utc,format=dateTimeFormat,tz="UTC")
   if (all(is.na(d3)))
      stop("Please check datetime format in source")
   ursa:::.elapsedTime("datetime is in POSIX format now")
   a$date_time_utc <- d3
   a <- a[with(a,order(shipid,date_time_utc)),]
   ursa:::.elapsedTime("Fall into AOI -- start")
   a <- aoiFilter(a)
   ursa:::.elapsedTime("Fall into AOI -- finish")
   print(c('Records for all seasons'=spatial_count(a)))
   season <- format(a$date_time_utc,"%Y%m")
   print(c(seasons=table(season)))
   res <- by(a,season,\(c2) {
      if (devel & spatial_count(c2)<50000)
         return(NULL)
      s <- format(c2$date_time_utc[1],"%Y%m")
      sa <- spatial_count(c2)
      names(sa) <- paste("Records for",sQuote(s),"season")
      print(sa)
      if (prelimDrop) {
         ursa:::.elapsedTime("Grid reading -- start")
         grd <- spatial_read(gridfile)
         ursa:::.elapsedTime("Grid reading -- finish")
         print(c('Grid CRS'=spatial_crs(grd)))
         print(c('Records before grid seeding'=spatial_count(c2)))
         ursa:::.elapsedTime("seeding within grid -- start")
         c2 <- spatial_intersection(grd,spatial_transform(c2,grd))
         ursa:::.elapsedTime("seeding within grid -- finish")
         print(c('Records after greed seeding'=spatial_count(c2)))
      }
      tb <- table(c2$shipid)
      tb <- tb[tb>occasionalEnries]
      if (devel) {
         tb <- tb[order(tb,decreasing=TRUE)]
         tb <- head(tb,3)
        # tb <- tb[tb>400]
      }
      c2 <- c2[as.character(c2$shipid) %in% sample(names(tb)),]
      print(c('Records with long-term stay in AOI'=spatial_count(c2)))
      ursa:::.elapsedTime("speed filter -- start")
      c2$keep <- FALSE
      shipid <- unique(c2$shipid) |> sample()
      pb <- ursaProgressBar(shipid)
      for (imo in shipid) {
         setUrsaProgressBar(pb,title=imo)
         ind <- which(c2$shipid %in% imo)
         c2[ind,] <- speedFilter(c2[ind,])
      }
      close(pb)
      c2 <- c2[c2$keep,]
      c2$keep <- NULL
      ursa:::.elapsedTime("speed filter -- finish")
      print(c('Records after speed filtering'=spatial_count(c2)))
      interim_write(c2,paste0("interim",s))
      if (release) {
         ursa:::.elapsedTime("write CSV -- start")
         c2 <- spatial_transform(c2,4326)
         xy <- spatial_coordinates(c2)
         ret <- cbind(spatial_data(c2),longitude=xy[,1],latitude=xy[,2])
         write.table(ret,paste0("interim",s,".csv")
                    ,sep=";",row.names=FALSE,quote=FALSE)
         ursa:::.elapsedTime("write CSV -- finish")
      }
      ursa:::.elapsedTime(paste("completed for",sQuote(s),"season"))
   })
   res
}
invisible({
   if (!interactive())
      main()
})
