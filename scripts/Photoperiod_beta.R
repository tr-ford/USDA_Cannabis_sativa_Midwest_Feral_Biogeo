library(geosphere)
library(terra)
library(raster)
library(geodata)

# ## Use a blank terra raster as the basis to fill with daylength information for selected days to generate rasters. 
# ## Forsythe, William C., Edward J. Rykiel Jr., Randal S. Stahl, Hsin-i Wu and Robert M. Schoolfield, 1995. A model comparison for daylength as a function of latitude and day of the year. Ecological Modeling 80:87-95.
# ## https://stackoverflow.com/questions/72653018/r-terra-create-spatraster-of-day-lengths-for-a-year
# r <- rast(res=0.5, ymin=-60)
# y <- init(r, "y")
# x <- app(y, \(i) apply(i, 1, \(j) daylength(j, c(172,356)))) 
# ## day number from: https://ag.arizona.edu/azmet/julian.html
# ## June 21st, December 22nd
# 
# ## List clipped raster to sample
# list <- list.files("raw/rasters/bio/wc2.1_2.5m/", pattern = "*.tif", full.names = TRUE)
# 
# ## Sort them into numerical and alphabetical order
# list <- mixedsort(sort(list))
# 
# ## Stack the "region" clipped rasters
# stack <- raster::stack(list)
# 
# ## Subset one raster and transform into terra obkect
# clip <- rast(stack[[1]])
# 
# ## Crop daylength rasters to extent, resample to correct resolution, then mask to delete NA's and reveal clipped shape.
# dayleng <- terra::crop(x, clip)
# resample_layer <-	resample(dayleng, clip)
# masked <- mask(resample_layer, clip)

clipped.daylength <- function(
		
	day.num,
	ref.rast,
	daylength.dir,
	...
	
){
	
	require(geodata, quietly = TRUE)
	require(raster, quietly = TRUE)
	require(terra, quietly = TRUE)
	require(geosphere, quietly = TRUE)
	require(gtools, quietly = TRUE)
	
	
	for (d in day.num) {
		

	## Use a blank terra raster as the basis to fill with daylength information for selected days to generate rasters. 
	## Forsythe, William C., Edward J. Rykiel Jr., Randal S. Stahl, Hsin-i Wu and Robert M. Schoolfield, 1995. A model comparison for daylength as a function of latitude and day of the year. Ecological Modeling 80:87-95.
	## https://stackoverflow.com/questions/72653018/r-terra-create-spatraster-of-day-lengths-for-a-year
	r <- rast(res=0.04, ymin=-60)
	y <- init(r, "y")
	x <- app(y, \(i) apply(i, 1, \(j) daylength(j, d))) 
	## day number from: https://ag.arizona.edu/azmet/julian.html
 
	## Transform into terra obkect
	clip <- rast(ref.rast)
	
	## Crop daylength rasters to extent, resample to correct resolution, then mask to delete NA's and reveal clipped shape.
	daylengths <- terra::crop(x, clip)
	day_sample <-	resample(daylengths, clip)
	day_mask <- mask(day_sample, clip)
	
	## Add names, must select an origin of -1 day from start bc numeric
	number <- as.Date(d, origin = "2023-01-01")
	
	sub_num <- gsub("-","_",number)

	names(day_mask) <- paste0("daylength_on_",sub_num)
	
	day_mask <- raster(day_mask)
	
	## Save your clipped daylength rasters
	raster::writeRaster(day_mask,	overwrite = TRUE, filename = paste0(daylength.dir,"/daylength_on_",d),... )
	print(paste0("daylength of ", sub_num, " raster written"))
	}
}


