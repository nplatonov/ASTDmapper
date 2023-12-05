require(ursa)
if (F) {
   aoi <- "LME_2013_polygon.shp" |> spatial_read() |> spatial_transform(4326)
   xy <- spatial_coordinates(aoi) |> do.call(rbind,args=_) |> do.call(rbind,args=_)
   summary(xy) |> print() ## find minimal latitude here
}
lat0 <- 49.75
if (F) {
   xy <- cbind(x=c(c(-180,179,by=1),-180),y=lat0)
   xy <- sf::st_polygon(list(xy))
   xy <- sf::st_sfc(xy,crs=4326)
   xy <- sf::st_sf(foo="bar",geometry=xy)
   xy <- sf::st_transform(xy,6931)
   str(xy)
   # xy <- sf::st_segmentize(xy,10000)
   glance(xy)
   q()
}
north <- ursa:::spatialize(c(-180,lat0,180,90),crs=4326)
north <- sf::st_transform(north,6931) |> sf::st_union()
# glance(north,resetGrid=TRUE,decor=FALSE,blank="white")
session_grid(NULL)
land <- "simplified_land_polygons.shp" |>
   spatial_read() |>
   spatial_transform(4326)
ind <- sapply(spatial_coordinates(land),\(xy) {
   any(do.call(rbind,xy)[,2]>lat0)
})
land <- land[ind,]
if (F) {
  #  str(land)
   land <- sf::st_combine(land)
   spatial_data(land) <- data.frame(id=1L)
} else {
   spatial_data(land) <- data.frame(id=10000L+seq(spatial_count(land)))
}
# str(land)
# spatial_data(land) <- data.frame(id=1L) # 10000L+seq(spatial_count(land)))
session_grid(NULL)
aoi <- "LME_2013_polygon.shp" |>
   spatial_read() |>
   spatial_transform(6931) |>
   spatial_union() |>
  # sf::st_combine() |>
   spatial_buffer(20*1e3)
north <- north |> spatial_transform(aoi) |> spatial_union()
spatial_data(north) <- data.frame(desc="north")
land <- land |> spatial_transform(aoi) |> spatial_union()
spatial_crs(north)
spatial_crs(land)
sf::sf_use_s2(FALSE)
ocean <- spatial_difference(north,land)
str(ocean)
summary(spatial_area(ocean)*1e-6)
spatial_crs(aoi)
spatial_crs(aoi)
ursa:::.elapsedTime("spatial intersection -- start")
aoi <- spatial_intersection(aoi,ocean)
spatial_data(aoi) <- data.frame(desc="Mask for ASTD data")
ursa:::.elapsedTime("spatial intersection -- finish")
str(aoi)
glance(aoi)
spatial_write(aoi,"study_area.sqlite",compress=TRUE)
