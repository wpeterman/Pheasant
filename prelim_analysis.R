#test of the lab 6 content
#finding core habitat using NLCD
install.packages('rgdal')

# library -----------------------------------------------------------------


library(terra)
library(sf)
library(landscapemetrics)
library(gdistance)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(JuliaCall)



#need to look at the distribution of areas at each count, obviously its single cell inflated
#compare distribution of "utilized" grass patch sizes to distribution of sizes in general
# prep  ---------------------------------------------------------------


#load in the cdls raster
cdls <- raster("C:/Users/boatg/OneDrive/Desktop/ARCfiles/cdlsrast_final.tif")

#load in the crp polygon
shape <- st_read("crp_vector.shp")
# shape <- read_sf(dsn = ".", layer = "crp_vector", fid_column_name = 'OBJECTID')

#make it a raster
crp <-fasterize(shape,cdls)

#make all the NA equal 0
crp[is.na(crp[])]<-0

#make values in crp 0 where the values in crp are not equal to 1
#and values in cdls is greater than 0
crp[!(cdls > 0)] <- NA
cdls[is.na(crp[])]<-NA

#now that we have both we can make them a spatraster, use og cdls to retain values for selection later
cdls<-rast("C:/Users/boatg/OneDrive/Desktop/ARCfiles/cdlsrast_final.tif")
crp<-rast(crp)


# patch id test multi ------------------------------------------------------------
#testing to see if the patch ide works with multiple cell types
install.packages("fasterize")
library(fasterize)

dev<- cdls ==  'Developed/Open Space'|
  cdls == 'Developed/Low Intensity'|
  cdls == 'Developed/Med Intensity'|
  cdls == 'Developed/High Intensity'
forest<- cdls ==  'Deciduous Forest'|
  cdls == 'Evergreen Forest'|
  cdls == 'Mixed Forest'|
  cdls == 'Christmas Trees'|
  cdls == 'Other Tree Crops'
grass <- cdls == 'Grassland/Pasture'|
  cdls == 'Fallow/Idle Cropland'|
  cdls == 'Clover/Wildflowers'|
  cdls == 'Switchgrass'|
  cdls == 'Shrubland' 
#see if we can layer them into one terra object

#turn them into a raster so as to make outsidde the study area NA
dev<-raster(dev)
forest<-raster(forest)
grass<-raster(grass)
#make the outside NA
dev[is.na(crp[])]<-NA
forest[is.na(crp[])]<-NA
grass[is.na(crp[])]<-NA
#return to spatraster
dev<-rast(dev)
forest<-rast(forest)
grass<-rast(grass)



#matrix sized to 2km squared to match the DOW report values
wt_mat <- matrix(nrow = 23,
                 ncol = 23,
                 data = 1/(23^2))
## Calculate mean using focal function
crp_prop <- focal(x = crp, 
                  w = wt_mat,
                  fun = 'mean')
dev_prop <- focal(x = dev, 
                  w = wt_mat,
                  fun = 'mean')
forest_prop <- focal(x = forest, 
                  w = wt_mat,
                  fun = 'mean')
grass_prop <- focal(x = grass, 
                     w = wt_mat,
                     fun = 'mean')
plot(crp_prop)
plot(dev_prop)
plot(forest_prop)
plot(grass_prop)
#selecting patches that have percent heuristically chose from literature
core_hab <-dev_prop < 0.20 &
  forest_prop < 0.50 &
  (crp_prop + grass_prop > 0.30)

plot(core_hab)

## Get core patches (using landscape metrics)
c_patch <- get_patches(core_hab)

## Covert to `terra` object
c_patch.terra <- rast(c_patch$layer_1$class_1)

plot(c_patch.terra,col = 'black')


# patch identification grass ----------------------------------------------------


## Create focal weight matrix
wt_mat <- matrix(nrow = 51,
                 ncol = 51,
                 data = 1/(51^2))
## Calculate mean using focal function
grass_prop <- focal(x = grass, 
                   w = wt_mat,
                   fun = 'mean')
#selecting patches that have xx% grassland cover
core_grass <-grass_prop > 0.30 #slot proportion chosen from schindler et al 2020
#replace the nas with 0
core_grass[core_grass == 0] <- NA

## Get core patches (using landscapemetrics)
c_patch <- get_patches(core_grass)


## Covert to `terra` object
c_patch.terra <- rast(c_patch$layer_1$class_1)

plot(c_patch.terra, col= 'red' )
## Calculate patch areas 
## NOTE: Area is returned in hectares
patch_area_tab <- lsm_p_area(c_patch.terra)

area_ha <- 20 #given that the home range for rnph is close to 20 in NA

#selects patches greater than or equal to the home range size
rnph_patch.id <- patch_area_tab$id[patch_area_tab$value >= area_ha]

## Now select core patches, make everything else NA
rnph_core <- c_patch.terra

rnph_core[c_patch.terra %in% rnph_patch.id] <- 999 ## Set to unique value
rnph_core[rnph_core != 999] <- NA
plot(rnph_core, col='red' )

writeRaster(rnph_core, "C:/Users/boatg/OneDrive/Desktop/ARCfiles/20ha_grass30_rnph.tif", overwrite=TRUE)


# resistance surfaces -----------------------------------------------------


#load in the cdls raster
cdls <- raster("C:/Users/boatg/OneDrive/Desktop/ARCfiles/cdlsrast_final.tif")

#load in the crp polygon
shape <- st_read("crp_vector.shp")
# shape <- read_sf(dsn = ".", layer = "crp_vector", fid_column_name = 'OBJECTID')

#make it a raster
crp <-fasterize(shape,cdls)

#make all the NA equal 0
crp[is.na(crp[])]<-0

#make values in crp 0 where the values in crp are not equal to 1
#and values in cdls is greater than 0
crp[!(cdls > 0)] <- NA
cdls[is.na(crp[])]<-NA

#now that we have both we can make them a spatraster, use og cdls to retain values for selection later
cdls<-rast("C:/Users/boatg/OneDrive/Desktop/ARCfiles/cdlsrast_final.tif")
crp<-rast(crp)
plot(crp)
#we can read in the levels from cdls, most of its ag and the lit hasn't shown any
#appreciable dif between any row crops except winter wheat but that has a temporal element so we'll clump it for now
rcl<-as.data.frame(levels(cdls)) #as a data frame so we can mutate on a column of weights
#its a data frame now, we'll manipulate it to have just the ID and resistances
#setting categories of cover: arable land, Grassland, Forest, Developed, water, wetland,

rcl<-mutate(rcl, resistance= ifelse(rcl$Value == 176,100, #grassland
ifelse(rcl$Value %in% 121:124, 20, #developed
ifelse(rcl$Value %in% 141:143, 50, #forest
ifelse(rcl$Value %in% 190:195, 60, #wetland
ifelse(rcl$Value == 111, 20, #open water change for connectivity as apposed to use
ifelse(rcl$Value == 152, 100, #shrub land
ifelse(rcl$Value == 131, 20,50#barren, arable
       ))))))))
#cut out the class label after checking
rcl_mat<- as.matrix(cbind(as.integer(rcl$Value), as.double( rcl$resistance)))


## Reclassify the landcover
cdls_prob <- classify(cdls, rcl_mat, right = NA)


#repeat that but with crp layer

crpmat<-as.data.frame(levels(crp))
#make a value in the first column to attache things to 
crpmat[1] = c(1)
crpmat<-mutate(crpmat, resistance = 100)#
crpmat<-as.matrix(cbind(as.integer(crpmat$X..), as.double(crpmat$resistance)))
crp_prob<- classify(crp, crpmat, right =NA )
####
#make the surface proportions rather than1:100
prob_layers <- cdls_prob / 100
crpprob_layers <-crp_prob / 100
## To multiply layers together, we use the `prod` function from Terra
prob_surf <- prod(prob_layers, crpprob_layers)
plot(prob_surf)
str(prob_surf)
prob_surf <- subst(prob_surf,
                   from = 0,
                   to = 0.001)

prob_surf <- subst(prob_surf,
                   from = 2.5,
                   to = 0.001)

cuts=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1) #set breaks
pal <- colorRampPalette(c("grey","forestgreen"))
plot(prob_surf, breaks=cuts, col = pal(10))

writeRaster(prob_surf, "resist_rnph_11_03.tif", overwrite=FALSE)
