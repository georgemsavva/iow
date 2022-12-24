library(sf)
library(ggplot2)

st_layers("c:/kontur/kontur_population_GB_20220630.gpkg")
kontur <- st_read("c:/kontur/kontur_population_GB_20220630.gpkg")

ggplot(kontur) + geom_sf()

plot(kontur$geom[1:10000])

kontur$geom[1:10000] |> sf::NA_z_range_()
kontur$geom[1:1] |> sf::st_bbox()

kontur$xmin <- sapply(kontur$geom , \(s) st_bbox(s)[1])
kontur$ymin <- sapply(kontur$geom , \(s) st_bbox(s)[2])

plot(kontur$xmin, kontur$ymin, pch=".")

iow <- subset(kontur, xmin>(-180000) & xmin<(-110000) & ymin>(6500000) & ymin<(6580000))

plot(iow$xmin, iow$ymin, pch=19)


iowkontur <- ggplot(iow) + geom_sf(aes(fill=population), col=NA) + 
  theme_bw() + scale_fill_gradient2(low="white", high="black") 
iowkontur
library(rayshader)

plot_gg(iowkontur, multicore = TRUE, width = 7 ,height=6, fov = 40)
