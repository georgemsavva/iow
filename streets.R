
lat_range = c(50.57,50.78)
long_range	= c(-1.6,-1.04)

osm_bbox = c(long_range[1],lat_range[1], long_range[2],lat_range[2])

iow_highway = opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 
iow_highway


iow_lines = st_transform(iow_highway$osm_lines, crs=crs(er))



ggplot(subset(iow_lines, highway%in%c("primary", "secondary", "tertiary"))) + 
  geom_sf(lwd=1,col="white") + theme_dark() + 
  geom_sf(data=subset(iow_lines, highway%in%c("residential")), col="white", lwd=0.5) + 
  geom_sf(data=subset(iow_lines, highway%in%c("cycleway")), col="blue", lwd=0.5)+ 
  geom_sf(data=subset(iow_lines, highway%in%c("footway")), col="green", lwd=0.5)

names(iow_lines)

table(iow_lines$highway)
