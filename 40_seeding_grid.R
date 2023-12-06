source("common.R")
stopifnot(length(spatial_dir(gridfile))==1L)
'main' <- function(res) {
   session_grid(NULL)
   ursa:::.elapsedTime("Grid reading -- start")
   grd <- spatial_read(gridfile)
   ursa:::.elapsedTime("Grid reading -- finish")
   if (missing(res)) {
      res <- interim_read(".*interim.*\\d{6}.*")
   }
   g1 <- allocate(spatial_centroid(grd),resetGrid=TRUE)
   ret <- lapply(res,\(c2) {
      s <- unique(format(c2$date_time_utc,"%Y%m"))
      fileout <- paste0("heatmap",s,".tif")
      if (file.exists(fileout)) {
         message(sQuote(fileout)," is ready. Skipping")
         return(NULL)
      }
      if (length(s)!=1) {
         print(s)
         stop("Damaged season splitting")
      }
      if (!prelimDrop) {
         print(c('Records before grid seeding'=spatial_count(c2)))
         ursa:::.elapsedTime("seeding within grid -- start")
         c2 <- spatial_intersection(grd,spatial_transform(c2,grd))
         ursa:::.elapsedTime("seeding within grid -- finish")
         print(c('Records after grid seeding'=spatial_count(c2)))
      }
      cond <- list(cell=c2$cell,shipid=c2$shipid)
      ursa:::.elapsedTime("aggregating individual ships -- start")
      d <- aggregate(list(duration=c2$sec_nextpoint),cond,sum)
      d$duration <- d$duration/(24*60*60)
      d$firstentry <- aggregate(list(x=c2$date_time_utc),cond,min)$x
      d$lastentry <- aggregate(list(x=c2$date_time_utc),cond,max)$x
      ursa:::.elapsedTime("aggregating individual ships -- finish")
      print(series(d))
      ursa:::.elapsedTime("aggregating all ships -- start")
      d2 <- aggregate(list(duration=d$duration),by=list(cell=d$cell),sum)
      ursa:::.elapsedTime("aggregating all ships -- finish")
      print(series(d2))
      r2 <- g1
     # save(r2,d2,file="interim.Rdata")
      ind <- ursa_value(r2) %in% d2$cell
      ursa_value(r2)[!ind] <- NA
      ursa_value(r2)[ind] <- d2$duration
      ursa_write(r2,fileout)
      r3 <- ursa_crop(r2,border=1)
      print(r3)
      try(display(r3,fileout=gsub("\\..+$",".png",fileout),bpp=8
             ,stretch="equal",blank="white",coast.fill="#00000010"))
      ursa:::.elapsedTime(paste("completed for",sQuote(s),"season"))
   })
   0L
}
invisible({
   if (!interactive())
      main()
})
