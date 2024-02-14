library(raster)
library(tidyverse)
library(sf)
library(rpart)
library(rpart.plot)
library(rasterVis)
library(mapedit)
library(mapview)
library(caret)
library(rgdal)
library(maptools)
library(sf)
library(terra)
band1 <- raster("C:/Users/Charvi-Jaiswal/Desktop/LC08_L2SP_144051_20131107_20200912_02_T1/LC08_L2SP_144051_20131107_20200912_02_T1_SR_B1.tif")
plot(band1)
crop_extent <- readOGR("C:/Users/Charvi-Jaiswal/Downloads/Bengaluru/Bengaluru.shp")
plot(crop_extent,
     main = "crop extent",
     axes = TRUE,
     border = "red")
L_crop1 <- crop(band1, crop_extent)
plot(L_crop1, main = "Cropped ")
Mask_L_crop1 <- mask(L_crop1, crop_extent)
plot(Mask_L_crop1)


band2 <- raster("C:/Users/Charvi-Jaiswal/Desktop/LC08_L2SP_144051_20131107_20200912_02_T1/LC08_L2SP_144051_20131107_20200912_02_T1_SR_B2.tif")
plot(band2)
crop_extent <- readOGR("C:/Users/Charvi-Jaiswal/Downloads/Bengaluru/Bengaluru.shp")
plot(crop_extent,
     main = "crop extent",
     axes = TRUE,
     border = "red")
L_crop2 <- crop(band2, crop_extent)
plot(L_crop2, main = "Cropped ")
Mask_L_crop2 <- mask(L_crop2, crop_extent)
plot(Mask_L_crop2)

band3 <- raster("C:/Users/Charvi-Jaiswal/Desktop/LC08_L2SP_144051_20131107_20200912_02_T1/LC08_L2SP_144051_20131107_20200912_02_T1_SR_B3.tif")
plot(band3)
L_crop3 <- crop(band3, crop_extent)
plot(L_crop3, main = "Cropped ")
Mask_L_crop3 <- mask(L_crop3, crop_extent)
plot(Mask_L_crop3)

band4 <- raster("C:/Users/Charvi-Jaiswal/Desktop/LC08_L2SP_144051_20131107_20200912_02_T1/LC08_L2SP_144051_20131107_20200912_02_T1_SR_B4.tif")
plot(band4)
L_crop4 <- crop(band4, crop_extent)
plot(L_crop4, main = "Cropped ")
Mask_L_crop4 <- mask(L_crop4, crop_extent)
plot(Mask_L_crop4)

band5 <- raster("C:/Users/Charvi-Jaiswal/Desktop/LC08_L2SP_144051_20131107_20200912_02_T1/LC08_L2SP_144051_20131107_20200912_02_T1_SR_B5.tif")

plot(band5)
L_crop5 <- crop(band5, crop_extent)
plot(L_crop5, main = "Cropped ")
Mask_L_crop5 <- mask(L_crop5, crop_extent)
plot(Mask_L_crop5)

band6 <- raster("C:/Users/Charvi-Jaiswal/Desktop/LC08_L2SP_144051_20131107_20200912_02_T1/LC08_L2SP_144051_20131107_20200912_02_T1_SR_B6.tif")

plot(band6)

L_crop6 <- crop(band6, crop_extent)
plot(L_crop6, main = "Cropped ")
Mask_L_crop6 <- mask(L_crop6, crop_extent)
plot(Mask_L_crop6)

band7 <- raster("C:/Users/Charvi-Jaiswal/Desktop/LC08_L2SP_144051_20131107_20200912_02_T1/LC08_L2SP_144051_20131107_20200912_02_T1_SR_B7.tif")
plot(band7)
L_crop7 <- crop(band7, crop_extent)
plot(L_crop7, main = "Cropped ")
Mask_L_crop7 <- mask(L_crop7, crop_extent)
plot(Mask_L_crop7)


band10 <- raster("C:/Users/Charvi-Jaiswal/Desktop/LC08_L2SP_144051_20131107_20200912_02_T1/LC08_L2SP_144051_20131107_20200912_02_T1_ST_B10.tif")
plot(band10)
L_crop10 <- crop(band10, crop_extent)
plot(L_crop10, main = "Cropped ")
Mask_L_crop10 <- mask(L_crop10, crop_extent)
plot(Mask_L_crop10)

image <- stack(Mask_L_crop1, Mask_L_crop2, Mask_L_crop3, Mask_L_crop4, Mask_L_crop5, Mask_L_crop7, Mask_L_crop10)
ndvi <- (image[[5]] - image[[4]])/(image[[5]] + image[[4]])
min(ndvi@data@values, na.rm = T)
max(ndvi@data@values, na.rm = T)
as(ndvi, "SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  ggplot(data = .) +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "NDVI for Calagary",
       x = " ",
       y = " ") +
  scale_fill_gradient(high = "#CEE50E",
                      low = "#087F28",
                      name = "NDVI")
res(band1)

nlayers(image)

par(col.axis="white",col.lab="white",tck=0)
plotRGB(image, r = 4, g = 3, b = 2, axes = TRUE, 
        stretch = "lin", main = "True Color Composite")
box(col="white")

par(col.axis="white",col.lab="white",tck=0)
plotRGB(image, r = 5, g = 4, b = 3, axes = TRUE, stretch = "lin", main = "False Color Composite")
box(col="white")

library(sp)
#points <- viewRGB(image, r = 4, g = 3, b = 2) %>% editMap()
points <- viewRGB(image, r = 5, g = 4, b = 3) %>% editMap()
clouds <- points$finished$geometry %>% st_sf() %>% mutate(class = "clouds", id = 1)
view(clouds)
points <- viewRGB(image, r = 5, g = 4, b = 3) %>% editMap()
urban <- points$finished$geometry %>% st_sf() %>% mutate(class = "urban", id = 2)
view(urban)

points <- viewRGB(image, r = 5, g = 4, b = 3) %>% editMap()
forest <- points$finished$geometry %>% st_sf() %>% mutate(class = "forest", id = 3)
view(forest)

points <- viewRGB(image, r = 5, g = 4, b = 3) %>% editMap()
water <- points$finished$geometry %>% st_sf() %>% mutate(class = "water", id = 4)
view(water)

training_points <- rbind(clouds, urban, forest, water)

training_points <- as(training_points, 'Spatial')

df = extract(image, training_points)
view(training_points)[]

df <- data.frame(training_points$class, df)

model.class <- rpart(as.factor(training_points.class)~., data = df, method = 'class')

rpart.plot(model.class, box.palette = 0, main = "Classification Tree")
pr <- predict(image, model.class, type ='class', progress = 'text') %>% 
  ratify()


levels(pr) <- levels(pr)[[1]] %>%
  mutate(legend = c("cloud","forest","urban","water"))
levelplot(pr, maxpixels = 1e6,
          col.regions = c('cyan', 'darkgreen','burlywood',  'blue'),
          scales=list(draw=FALSE),
          main = "Supervised Classification of Imagery")
writeRaster(pr, 
            filename = "D:/RF/classified.tiff",
            format = "GTiff", 
            overwrite=TRUE)

