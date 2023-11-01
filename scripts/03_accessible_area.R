## 03_accessible_area.R
## Script Three
## Tori Ford
### 07-02-23
#### Objective: Download environmental predictors and define accessible area through a multitude of parametres.


library(dplyr) # 1.1.0
library(geodata) # 0.5.6
library(sdmpredictors) # 0.2.14
library(raster) # 3.6.13
library(gtools) # 3.9.4
library(sp) # 1.5.1
library(rangeBuilder) # 2.1
library(countrycode) # 1.4.0
library(rnaturalearth) # 0.3.2
library(ggplot2) # 3.4.1
library(rgdal) # 1.6.5
library(sf) # 1.0.9
library(rgeos) # 0.6.2

# Downloading and Resample Environmental Predictors ------------------------------------


# ## Create a list for sets of variables downloaded with same naming convention, update as you go
wc_var <- c("bio", "tmin","tmax","prec","srad","wind","vapr")
esa_var <- c("trees", "grassland", "shrubs", "cropland", "built", "bare", "snow", 
						 "water", "wetland", "mangroves", "moss")


## Download for WorldClim Data

	## Loop for WorldClim Data
	for (q in 1:length(wc_var)) {
		var <- wc_var[q]
		print(var)
		
		## Only needed once per project, same goes for every created directory in this script
		# dir.create(paste0("raw/rasters/", var))
		
		worldclim <- geodata::worldclim_global(var = var,
																					 res = 2.5,
																					 path = paste0("raw/rasters/", var))
		
	}
	

	## These are what can be used to resample finer grain resolution rasters
	## Subset one raster for resampling purposes, any worldclim is fine, m
	clim_rast <- raster("raw/rasters/srad/wc2.1_2.5m/wc2.1_2.5m_srad_06.tif")
	
	## Verify extent as even numbers, -180,180,90,90 and resolution as 2.5 arc-minute degrees, 0.04166667. 
	## 0.00833333 for 30 arc-seconds
	## (This can be changed based on study preference, just use this step to ensure your data has been pulled correctly.)
	clim_rast

	
## Download Elevation Data

	## Create directory for elevation data
	dir.create("raw/rasters/elev")

	elevation <- geodata::elevation_global(res = 2.5,
																				 path = paste0("raw/rasters/elev"))
	
	
## Download photoperiod Rasters
	
	dir.create("raw/rasters/daylength")
	
	source("bin/scripts/Photoperiod_beta.R")
	
	## Use a local function to select days where photoperiod is/may be important to growth, choose a raster to reference extent and resolution, save in appropriate directory and format
	clipped.daylength(day.num = c(172,356), ref.rast = "raw/rasters/srad/wc2.1_2.5m/wc2.1_2.5m_srad_06.tif",
										daylength.dir = "raw/rasters/daylength",format = 'raster') ## Northern/Southern Hemisphere longest day
	
## Download Soil properties dataset in the United States: U.S. Geological Survey dataset
	
	sgolist <- list.files("raw/rasters/gNATSGO", pattern = "*.tif", full.names = TRUE)
	
	for (j in 1:length(sgolist)) {
		
		sgorast <- rast(sgolist[j]) ## Read in the raster

				name <-	names(sgorast) ## Create variable to name the raster outfile, this will make the names more
		print(name)
		
		shared_shp <- vect("raw/shapefiles/midwest_us.shp") # Read in shape to clip rasters to
		
		cropped <- terra::crop(sgorast, ext(shared_shp))
		masked <- mask(cropped, shared_shp)
		sgoraster <- raster(masked) ## Turn back into raster object
		
		cropped_samp <- terra::crop(rast(clim_rast), ext(shared_shp)) ## Crop and mask the sample raster to the exent of study region
		masked_samp <- mask(cropped_samp, shared_shp) %>% raster()
		
		sgo_resample <-
			resample(sgoraster, masked_samp) ## Sample both the CRS and Resolution to resample to 2.5m, do not choose a 'method', in this dataset, there are both categorical and continuous variables, allow resample to default based on the inherited values.

		sgo_ex <- extend(sgo_resample, extent(clim_rast), value=NA)
		
		raster::writeRaster(
			sgo_ex,
			filename = paste0("raw/rasters/gNATSGO/", name, "_2_5m"),
			format = "raster",
			overwrite = TRUE
		) ##  
		
	}
	
	
# ## Download ESA WorldCover Data: https://esa-worldcover.org/en
# 	
# 	
# 	## IF you already have these downloaded, skip this loop and go to resampling
# 	for (p in 1:length(esa_var)) {
# 		var <- esa_var[p]
# 		print(var)
# 		
# 		## Create ESA WorldCover Directory
# 		## Only needed once per project
# 		# dir.create(paste0("raw/rasters/", var))
# 		
# 		worldcover <- geodata::landcover(var = var,
# 																		 path = paste0("raw/rasters/", var))
# 		
# 	}
# 	
# 	
# 	## Read in the 30s landuse rasters
# 	landlist <- list.files(paste0("raw/rasters/",esa_var), pattern = "*30s.tif", full.names = TRUE)
# 	
# 	## Read in a classification table, at the moment, the we have 5 bins of Classification(you can change the classification by changing the dataframe, but it must be in matrix format to reclassify
# 	## https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/classify-raster/
# 	## .02/0=no presence, .04/1=low presence, .06/2=average presence, .08/3=moderate presence, 1/4=high presence
# 	rat_class <- as.matrix(read.csv("raw/rasters/raster_attributes_V2.csv",header = FALSE))
# 	
# 	## Create WorldCover Directory
# 	## Only needed once per project
# 	dir.create(paste0("raw/rasters/worldcover"))
# 	
# 	
# 	## Resample 30s raster to 2.5m
# 	for (r in 1:length(landlist)) {
# 		landvar <-
# 			strsplit(landlist[r], '[/]')[[1]][3] ## Create worldcover variable for naming purposes
# 		print(landvar)
# 		
# 		rst <-
# 			landlist[r] ## iterate through landlist for rasters, only one can be processed at once
# 
# 		landrast <- raster(rst)
# 		
# 		land_classified <- reclassify(landrast,rat_class)
# 		
# 		names(land_classified) <- paste0("wdcvr_",landvar)
# 		# reProj_layer <-
# 		# 	projectRaster(
# 		# 		landrast,
# 		# 		crs = crs(clim_rast),
# 		# 		res = res(clim_rast),
# 		# 		over = TRUE
# 		# 	)
# 		
# 		land_resample <- resample(land_classified, clim_rast,method= 'ngb') ## Sample both the CRS and Resolution to resample to 2.5m
# 		landcover_ex <- extend(land_resample, extent(clim_rast), value=NA)
# 		
# 		raster::writeRaster(
# 			landcover_ex,
# 			filename = paste0("raw/rasters/worldcover/esa_", landvar, "_2_5m"),
# 			format = "raster", 
# 			overwrite = TRUE
# 		) ##  Download Landuse Rasters
# 		
# 	}
	
	
# ## Download Soil Profile Data
# 	
# 	type_var <- c("Alisols", "Andosols", "Arenosols", #"Acrisols", "Albeluvisols", 
# 								"Calcisols", "Cambisols", "Chernozems", "Cryosols", "Durisols", "Ferralsols",
# 								"Fluvisols", "Gleysols", "Gypsisols", "Histosols", "Kastanozems", "Leptosols",
# 								"Lixisols", "Luvisols", "Nitisols", "Phaeozems", "Planosols", "Plinthosols", 
# 								"Podzols", "Regosols", "Solonchaks", "Solonetz", "Stagnosols", "Umbrisols", "Vertisols")
# 	
# 	## Create SoilGridsV2 WRB Directory
# 	# dir.create("raw/rasters/soiltype")
# 	
# 	## Resample 30s raster to 2.5m
# 	for (p in 1:length(type_var)) {
# 		name <- type_var[p]
# 		print(name)
# 		
# 		soiltypes <- geodata::soil_world_vsi(
# 			var = "wrb",
# 			name = name,
# 			# path = "raw/rasters/soiltype/"
# 		) ## Soil download not working, use VSI to interface in the meantime, read to local variable to resample
# 	
# 		rastime <- Sys.time()## Time the terra:writeraster
# 		terra::writeRaster(soiltypes, filename = paste0("raw/rasters/soiltype/",name, "_30s.tif"),  overwrite = FALSE) ## format = "GTiff", Download Soil Type data
# 		sty <- list.files("raw/rasters/soiltype/", pattern = paste0(name, "_30s*.tif"), full.names = TRUE) ##read file back in
# 		
# 		terr <- Sys.time() - rastime # calculate difference
# 		print(terr) ## Time it takes to process terra write
# 		
# 		restime <- Sys.time()## Time the resampling
# 		stype <- raster(sty) ## Read as rasterlayer
# 		s <- crop(stype, extent(clim_rast)) ## Match the extent of the rasters
# 		resample_layer <- raster::resample(s, clim_rast, filename = paste0("raw/rasters/soiltype/",name, "_2_5m"), format = "GTiff") ## Sample both the CRS and Resolution to resample to 2.5m
# 		
# 		resam <- Sys.time() - restime # calculate difference
# 		print(resam)
# 	}
# 	
# 	
	# ## Download World Soils Data
	# 
	# sworld_var <- c("phh2o", "sand", "silt", "soc", "bdod","cfvo", "clay", "nitrogen","ocd", "ocs")
	# depth <- c(5, 15, 30, 60, 100, 200) # , 100, 200 cm not working atm, prolly use diff script
	# 
	# ## Create SoilGridsV2 World Soils Directory
	# # dir.create("raw/rasters/worldsoils")
	# 
	# ## Resample 30s raster to 2.5m
	# for (p in 1:length(sworld_var)) {
	# 	for (d in depth) {
	# 
	# 		print(d)
	# 		name <- sworld_var[p]
	# 		print(name)
	# 
	# 		soilworld <- geodata::soil_world(
	# 			var = "nitrogen",
	# 			depth = 60,
	# 			stat="mean",
	# 			path = "raw/rasters/worldsoils"
	# 		) ## Soil download not working, use VSI to interface in the meantime, read to local variable to resample
	# 
	# 		sw <- list.files("raw/rasters/worldsoils/", pattern = paste0(names(soilworld), "_mean_30s*.tif"), full.names = TRUE) ##read file back in
	# 		spat_clim <- rast(clim_rast)
	# 		sw_crs<- terra::project(soilworld,spat_clim,gdal=TRUE, threads = TRUE)
	# 		sworld <- raster(soilworld) ## Coerce spatRaster to raster layer
	# 		reProj_layer <- projectRaster(sworld, crs = crs(clim_rast), res = res(clim_rast), over = TRUE) ## Sample target CRS and Resolution, ignore wrapping of the dataline with 'over'
	# 		resample_layer <- resample(reProj_layer, clim_rast) ## Sample both the CRS and Resolution to resample to 2.5m
	# 
	# 		# sw_stars <- st_as_stars(resample_layer) ## Change raster object to stars to retain metadata
	# 		# write_stars(sw_stars, paste0("raw/rasters/worldsoils/",name,"_",d,"cm_2_5m.tif"))
	# 		#
	# 
	# 		raster::writeRaster(resample_layer, filename = paste0("raw/rasters/worldsoils/",name,"_",d,"cm_2_5m2"), format="raster", overwrite = TRUE) ##  Download Soil Type data
	# 
	# 	}
	# }
	# 
	# 
# ## Download Harmonized World Soil Database v1.2 Data
# 	
# 	## Downloaded an updated regridded version from: https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1247
# 	## Could also download this data, using a pixel point or bounding box using the r package, hwsdr: https://github.com/bluegreen-labs/hwsdr
# 	
# 	### BEFORE you start this section, reference the HWSDB documentation. Delete any NC4 file that is not categorized as 'Physico-chemical properties', the inclusion of this class of data will not be suitable for this analysis and will have to be manually deleted. 
# 	### https://daac.ornl.gov/daacdata/global_soil/HWSD/comp/HWSD1.2_documentation.pdf, you will need a username and password to access, as of 28th Feb 2023
# 	
# 	hwsblist <- list.files("raw/rasters/hwsb", pattern = "*.nc4", full.names = TRUE)
# 	hwsblist <- mixedsort(sort(hwsblist)) ## Sort the rasters to be in alphabetical order
# 	
# 	## Loop for all HWSDB rasters to resample from 30s to 2.5 minute resolution
# 	for (t in 1:length(hwsblist)) {
# 		hwsbrast <- raster(hwsblist[t], band = 1) ## Read in the raster
# 		names(hwsbrast) <-
# 			gsub("\\.", "_", names(hwsbrast)) ## Need the variable names for outfile, cannot print dots, so sub them for underscores
# 		name <-	names(hwsbrast) ## Create variable to name the raster outfile, this will make the names more
# 		print(name)
# 		
# 		names(hwsbrast) <- paste0("hwsb_",name)
# 		
# 		hwsb_resample <-
# 			resample(hwsbrast, clim_rast) ## Sample both the CRS and Resolution to resample to 2.5m, do not choose a 'method', in this dataset, there are both categorical and continuous variables, allow resample to default based on the inherited values.
# 		hwsb_ex <- extend(hwsb_resample, extent(clim_rast), value=NA)
# 		
# 		raster::writeRaster(
# 			hwsb_ex,
# 			filename = paste0("raw/rasters/hwsb/hwsb_", name, "_2_5m"),
# 			format = "raster",
# 			overwrite = TRUE
# 		) ##  Download Soil Type data
# 		
# 	} ## Manually delete the 'MU_GLOBAL.nc4'/" or 'HWSD_global_mapping_unit_identifier_2_5m' files, they correspond to something only useful in the native HWSDB viewer. 

## STOP HERE, have layers been aggregated/resampled to 2.5 arc min resolution?
	
## Can possibly add variables from sdmpredictors
	
	## Create ENVIREM World Soils Directory
	dir.create("raw/rasters/ENVIREM")
	
	## First, make a list of variables to evaluate which ones to keep
	## WATCH HERE, layers are in 5 arc minutes, if this is larger than your study resolution, other options must be manually downloaded
	## ENVIREM github and main page (if other download link is obselete, they will have other download methods): http://envirem.github.io/
	## Manual .zip downloads (30 arc-sec, 2.5,5, and 10 arc-minutes): https://deepblue.lib.umich.edu/data/concern/data_sets/gt54kn05f
	# lay.list <- list_layers(datasets=c(), terrestrial = TRUE, marine = NA, 
	# 												freshwater = FALSE, monthly = FALSE, version = NULL)
	# layers <- c('ER_annualPET','ER_aridityIndexThornthwaite','ER_climaticMoistureIndex','ER_growingDegDays0',
	# 						'ER_growingDegDays5','ER_PETseasonality','ER_thermicityIndex','ER_tri','ER_topoWet')
	# 
	# envirem <- load_layers(layercodes = layers, equalarea=FALSE, rasterstack=TRUE, datadir= "raw/rasters/ENVIREM")
	
	envmlist <- mixedsort(sort(list.files("raw/rasters/ENVIREM", pattern = "*.tif", full.names = TRUE)))

	## Rename and write out as .grd files (just easier)
	for (y in 1:length(envmlist)) {
		envmrast <- raster(envmlist[y])
		names(envmrast) <- paste0("ENVIREM_",strsplit(names(envmrast),'[_]')[[1]][3])
		name <- names(envmrast)
		envm_resample <- resample(envmrast, clim_rast) 
		envm_ex <- extend(envm_resample, extent(clim_rast), value=NA)
		
		raster::writeRaster(
			envm_ex,
			filename = paste0("raw/rasters/ENVIREM/", name),
			format = "raster",
			overwrite = TRUE
		) 
	}
	
	# Stack 2.5m Resolution Global Rasters -----------------------------------------------------------

	
	## Read in WorldClim Rasters
		climlist <- list.files(paste0("raw/rasters/",wc_var,"/wc2.1_2.5m"), pattern = "*.tif", full.names = TRUE)
		
	## Read in Elevation Rasters
		elevlist <- list.files("raw/rasters/elev/wc2.1_2.5m/", pattern = "*.tif", full.names = TRUE)
			
	## Read in Photoperiod Rasters
		photolist <- list.files("raw/rasters/daylength/", pattern = "*.grd", full.names = TRUE)
		
	## Read in ENVIronmental Rasters for Ecological Modeling (ENVIREM) Rasters		
		enviremlist <- list.files("raw/rasters/ENVIREM/", pattern = "*.grd", full.names = TRUE)
		
	## Read in gridded National Soil Survey Geographic (gNATSGO) Rasters
		sgolist <- list.files("raw/rasters/gNATSGO/", pattern = "*.grd", full.names = TRUE)
	# 
	# ## Read in ESA WorldCover Rasters		
	# 	wdcvrlist <- list.files(paste0("raw/rasters/worldcover/"), pattern = "*2_5m.grd", full.names = TRUE)	
	# 	
	# ## Read in Harmonized World Soil Database v1.2 Rasters
	# 	hwsblist <- list.files("raw/rasters/hwsb", pattern = "*.grd", full.names = TRUE)
		
	## Create a list containing the listed files for each category of raster
		predictors <- c(climlist, elevlist,enviremlist,sgolist,photolist) ## The original script runs 123 environmental predictors
		
	## Sort them into alphabetical order
		predictors <- mixedsort(sort(predictors))

	## Stack the predictors into raster stack object
		predstack <- raster::stack(predictors)

	## Substitute out any dots in names to not interfere with further naming conventions. 
		names(predstack) <- gsub("\\.","_", names(predstack))


# Define Accessible Area -------------------------------------


## Read in dataframe of all accessions
alldf <- read.csv("rst/samples/prelim_accessions.csv")

## Build list of regions
regions <- unique(alldf$REGION)
		
## Build list of thinning distance in kilometers
dist <- c(0.5)


## This is effectively the same thing as the last script, but prints a zoomed-in map
## Read in dataframe containing species occurrences, Subset data for each region and thinning distance, assign each dataset a unique name and save to csv
for (k in regions) {

	for (j in 1:length(dist)) {
		distance <- dist[j]
		
		## Read in accessions for each distance and Continent
		spdf <- read.csv(paste0("rst/samples/",k,"/",k,"_accessions_",distance,"km.csv"))
		# spdf <- spdf[!(spdf$LONG == "12.17"), ] # To filter out nuisance points
		
		## Map for each distance and region
		world <-
			borders(
				database = "world",
				colour = "gray20",
				fill = "white",
				xlim = c(min(spdf$LONG),max(spdf$LONG)),
				ylim = c(min(spdf$LAT),max(spdf$LAT))
			)
		
		spat_map <- ggplot() +
			world +
			ggtitle(paste0(
				"Cannabis sativa occurrences in ",k,
				" thinned to ", distance, " km"
			)) +
			geom_point(
				spdf,
				mapping = aes(x = LONG,
											y = LAT ,
											color = COUNTRY),
				alpha = 0.5,
				size = 1.2
			)

		## Save 'dist' km thinned map per Region
		ggsave(
			paste0("Map_of_",k, "_accessions_thinned_to_", distance, "km.png"),
			plot = spat_map ,
			path = paste0("rst/maps/", k, "/"),
			height = 7,
			width = 14
		)
		
	}
	
}



# Crop Accessible Area ----------------------------------------------------

## Modified from: https://github.com/soltislab/BotanyENMWorkshops/blob/main/Demos/Rbased/CrashCourse/04_ClimateProcessing.R

## Read in dataframe of all accessions
alldf <- read.csv("rst/samples/prelim_accessions.csv")

## Build list of thinning distance in kilometers
dist <- c(0.5)

## Build list of regions
regions <- unique(alldf$REGION)
# regions <- regions[-c(1)]

for (k in regions) {
	for (j in 1:length(dist)) {
		set.seed(2023)
		
		distance <- dist[j]

		## Read in accessions for each distance and Continent
		spdf <-
			read.csv(paste0(
				"rst/samples/", k,
				"/", k,
				"_accessions_",	distance,
				"km.csv"
			))
		
		## Create a dataframe of long/lat coordinates to feed to spatial dataframe
		bw <- spdf[, c(3,2)]
		
		## Create a spatial points dataframe that we can extract coordinates from
		spatdf <- SpatialPointsDataFrame(
			coords = bw,
			data = spdf,
			proj4string = CRS("+proj=longlat +datum=WGS84")
		)
		
		## Use rangebuilder to create an alpha hull, with a 5km buffer
		a_hull <- rangeBuilder::getDynamicAlphaHull(
			x = spatdf@coords,
			coordHeaders = c("LONG", "LAT"),
			fraction = 1,
			# min. fraction of records we want included (e.g. 1 = 100%)
			partCount = 1,
			# number of polygons, this may change per region, if your rasters are not turning out right, change this code first
			initialAlpha = 3,
			# initial alpha size, try not to set to 1, use a minimum of 3
			clipToCoast = "terrestrial",
			# "the resulting polygon is clipped to the coastline, using a basemap from naturalearth"
			buff = 5000,
			# distance in meters, (5) 10 km
			verbose = TRUE
			# prints alpha to console for debugging purposes
		) ## RAN as 1,1,3,"terrestrial",10000,TRUE | MW: 1,2,3,"terrestrial",10000,TRUE
		## IF YOU ENCOUNTER ERROR "Error in FUN(X[[i]], ...) : polygons require at least 4 points", YOUR 'spdf' file may be corrupted, run script 02 again using the final thinning section, delete old files and create new ones.
		
		# ## Plot your buffered hull, optional, must be turned off if using supercomputer or have to add dev.off()
		# plot(a_hull[[1]], col=transparentColor('gray50', 0.5), border = NA)
		# points(x = spatdf$LONG, y = spatdf$LAT, cex = 0.5, pch = 3)
		
		## Convert the bbuffered hull to a spatial polygon that can be used to crop rasters
		b_hull <- sf::as_Spatial(st_geometry(a_hull[[1]]))
		
		## Transform using the appropriate CRS: https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf
		b_hull <-
			spTransform(b_hull,	CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
		
		## Setup for outfile
		dir.create(paste0("raw/rasters/", k))
		path <- paste0("raw/rasters/", k , "/")
		end <- ".asc"
		
		## Loop through climstack for dist by region
		for (o in 1:length(names(predstack))) {
			# Subset raster layer
			preds <- predstack[[o]]
			# Setup file names
			name <- names(preds)
			print(name) ## Just here to check on progress of loop
			out <- paste0(path, name)
			outfile <- paste0(out, end)
			# Crop and mask
			cropped <- crop(preds, raster::extent(b_hull))
			masked <- mask(cropped, b_hull)
			# Write raster
			raster::writeRaster(masked,
													outfile,
													format = "ascii",
													overwrite = TRUE)
		}
		
	}
	
}

# Select Optimally Thinned Occurrences for Maxent ------------------------------------


source("bin/scripts/functions/03_accessible_area_Functions.R")

# dir.create("rst/samples/aggregate")

maxent.csv(in.dir = "rst/samples/",
					 regions = c("Midwest"),
					 opt.dist = 0.5,
					 out.csv = "maxent_ready.csv",
					 aggregate.model = TRUE,
					 aggregate.dir = "rst/samples/Midwest/")


# Generate Source Table ---------------------------------------------------

for (k in regions) {
	## Read in dataframe containing Maxent ready csv
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	
	## Count the number of occurrences for each source
	hits <- spdf %>% count(SOURCE)
	
	## Rename columns to publication appropriate name
	final.hits <- dplyr::rename(hits, `number of occurrences` = 2)
	
	write.csv(final.hits,file = paste0("rst/samples/", k,"/occurrence_table.csv"),row.names = FALSE)
	
}


# Generate Aggregate Rasters (OPTIONAL) ----------------------------------------------


ferallist <- mixedsort(sort(list.files("raw/rasters/NY-Trial/", pattern = "*.asc", full.names = TRUE)))

nylist <- mixedsort(sort(list.files("raw/rasters/Midwest-Feral/", pattern = "*.asc", full.names = TRUE)))


dir.create("raw/rasters/aggregate")


path <- paste0("raw/rasters/aggregate/")
end <- ".asc"

## ALL this does is take the individual polygons formed through the "crop" section from different regions
for (p in 1:length(ferallist)) {
	
	if (length(ferallist) != length(nylist)) {
		break
	}
	
	print(p)
	
	merge1 <- raster(ferallist[[p]])
	merge2 <- raster(nylist[[p]])	
	
	if (names(merge1) != names(merge2)) {
		break
	}
	
	merged <- raster::merge(merge1,merge2)
	
	names(merged) <- names(merge2)
	
	name <- names(merged)
	
	out <- paste0(path, name)
	outfile <- paste0(out, end)
	
	raster::writeRaster(merged,
											outfile,
											format = "ascii",
											overwrite = TRUE)
	 
}
