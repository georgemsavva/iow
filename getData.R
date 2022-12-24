# To install the latest version from Github:
# install.packages("devtools")
devtools::install_github("tylermorganwall/rayshader")

library(rayshader)
library(elevatr)
library(raster)

library(osmdata)
library(sf)

Latitude	50.57 - 50.78
Longitude	-1.6 - -1.04

dat   <- expand.grid(x=seq(-1.6, -1.0, l=1000),y=seq(50.5, 50.8, l=1000))
er    <- get_elev_raster(dat,z=12, prj="EPSG:4326")
library(terra)
er <- terra::aggregate(er,4)
elmat <- raster_to_matrix(er)
elmat_water = elmat
elmat_water[elmat_water <= 2] = 0

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat_water), color = "desert") %>%
  plot_map()


library(data.table)
bigm <- matrix(-1,nrow=2000,ncol=2000)
filenames <- dir(path = "terr50_gagg_gb/data/sz", pattern = ".asc$", full.names = TRUE)
lapply(filenames, \(f){
  m = read.table(file=f, skip=5) |> as.matrix()
  starty=200*as.numeric(substr(f,26,26))
  startx=200*as.numeric(substr(f,27,27))
  bigm[startx+(200:1), starty+(1:200)] <<- m
  c(startx,starty)
  })


bigm2 <- bigm[1400:2000,550:1350]
graphics::image(t(bigm2), useRaster=TRUE, col=c(colorRampPalette(c("white", "purple","black"))(100)), asp=6/7)
text("Isle of Wight - elevation", x=.1,y=.1, col="black", adj=c(0,0))


colorRamp(c("white", "purple"))(0.5)

bigm2 <- t(bigm2)

bigm2  |>
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(bigm2, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(bigm2), 0) %>%
  plot_3d(bigm2, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 1200))


bigm3 <- bigm2
bigm3[1:801,] <- bigm2[801:1,]

bigm3[bigm3< -0.0] <-NA
dim(bigm3)
bigm3  |>
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(bigm3, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(bigm3), 0) %>%
  plot_3d(bigm3, 
          zscale = 10, 
          fov = 0, 
          theta = 135, 
          zoom = 0.75, 
          phi = 45, 
          windowsize = c(1000, 1200))

render_highquality(filename="test.png", samples=300)

render_highquality(
  "iow_highres.png", 
  parallel = TRUE, 
  samples = 300,
  interactive = FALSE,
  
)

library(data.table)
bigm3 |> as.data.table() -> bigDT
bigDT$y <- 1:801
table(bigDT$y)
melt(bigDT, id.vars = "y", variable.name = "x", value.name="elevation") -> dat
dat$x <- as.numeric(sub("V","",dat$x))

dat
library(ggplot2)
gg_nc <- ggplot(dat, aes(x,y,fill=elevation)) + geom_raster() + coord_flip() + scale_y_reverse() + 
  theme_bw() + scale_fill_viridis_c(na.value=NA) + 
  ggtitle("Elevation of the Isle of Wight", "Contains OS data © Crown copyright and database right 2022") + 
  labs(fill="Height (m)",x=NULL, y=NULL) + theme(axis.text = element_blank(), axis.ticks = element_blank())
  
gg_nc

plot_gg(gg_nc, multicore = TRUE, width = 7 ,height=6, fov = 40, scale = 50)
render_depth(focallength = 100,focus=0.72)

render_movie(filename = "testmovie" , 
             phi = 60,frames = 720)
