download_worldclim <- function(wclim_var ,
															 download_elev = FALSE,
															 output_dir,...
){
	## Loop for WorldClim Data
	for (q in 1:length(wclim_var)) {
		var <- wclim_var[q]
		print(var)

		## Only needed once per project, same goes for every created directory in this script
		dir.create(paste0(output_dir, var))

		worldclim <- geodata::worldclim_global(var = var,
																					 ...,
																					 path = paste0(output_dir, var))

	}


	## Download Elevation Data
	if(isTRUE(download_elev)){
	## Create directory for elevation data
	dir.create(paste0(output_dir,"elevation"))

	elevation <- geodata::elevation_global(...,
																				 path = paste0(output_dir,"elevation"))
	}

}


# download_worldclim(wclim_var = "bio", download_elev = TRUE, output_dir = "raw/rasters/")

download_worldcover <- function(esa_var,
																raw_dir,
																ref_rast,
																classification_csv, 
																out_folder, 
																...){
	## Download ESA WorldCover Data: https://esa-worldcover.org/en
	
	
	## IF you already have these downloaded, skip this loop and go to resampling
	for (p in 1:length(esa_var)) {
		var <- esa_var[p]
		print(var)
		
		## Create ESA WorldCover Directory
		## Only needed once per project
		dir.create(paste0(raw_dir, var))
		
		worldcover <- geodata::landcover(var = var,
																		 path = paste0(raw_dir, var))
		
	}
	
	
	## Read in the 30s landuse rasters
	landlist <- list.files(paste0(raw_dir,esa_var), pattern = "*30s.tif", full.names = TRUE)
	
	## Read in a classification table, at the moment, the we have 5 bins of Classification(you can change the classification by changing the dataframe, but it must be in matrix format to reclassify
	## https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/classify-raster/
	## .02/0=no presence, .04/1=low presence, .06/2=average presence, .08/3=moderate presence, 1/4=high presence
	rat_class <- as.matrix(read.csv(paste0(classification_csv),header = FALSE))
	
	## Create WorldCover Directory
	## Only needed once per project
	dir.create(paste0(out_folder))
	
	ref_rast <- raster(ref_rast)
	
	## Resample 30s raster to 2.5m
	for (r in 1:length(landlist)) {
		landvar <-
			strsplit(landlist[r], '[/]')[[1]][length(strsplit(landlist[r], '[/]'))] ## Create worldcover variable for naming purposes
		
		rst <-
			landlist[r] ## iterate through landlist for rasters, only one can be processed at once
		
		landrast <- raster(rst)
		
		land_classified <- reclassify(landrast,rat_class)
		
		names(land_classified) <- paste0("wdcvr_",landvar)
	
		land_resample <- resample(land_classified, clim_rast,method= 'ngb') ## Sample both the CRS and Resolution to resample to 2.5m
		landcover_ex <- extend(land_resample, extent(clim_rast), value=NA)
		
		raster::writeRaster(
			landcover_ex,
			filename = paste0("raw/rasters/worldcover/esa_", landvar, "_2_5m"),
			..., ##format = "raster" 
			overwrite = TRUE
		) ##  Download Landuse Rasters
	}
	
}

# download_worldcover(esa_var = "trees",raw_dir = "raw/rasters/",
# 										ref_rast = "raw/rasters/srad/wc2.1_2.5m/wc2.1_2.5m_srad_06.tif",
# 										classification_csv = "raw/rasters/raster_attributes_V2.csv",
# 										out_folder = "raw/rasters/worldcover", format = "raster")


resample_hwsb <- function(input_folder,
													ref_rast,
													out_folder,...
){
	
	## Download Harmonized World Soil Database v1.2 Data
	
	# ## Create Harmonized World Soil Database Directory
	# ## Only needed once per project
	dir.create(paste0(out_folder))
	
	## Downloaded an updated regridded version from: https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1247
	## Could also download this data, using a pixel point or bounding box using the r package, hwsdr: https://github.com/bluegreen-labs/hwsdr
	hwsblist <- list.files(input_folder, pattern = "*.nc4", full.names = TRUE)
	hwsblist <- mixedsort(sort(hwsblist)) ## Sort the rasters to be in alphabetical order
	
	ref_rast <- raster(ref_rast)
	
	## Loop for all HWSDB rasters to resample from 30s to 2.5 minute resolution
	for (t in 1:length(hwsblist)) {
		hwsb_rast <- raster(hwsblist[t], band = 1) ## Read in the raster
		names(hwsb_rast) <-
			gsub("\\.", "_", names(hwsb_rast)) ## Need the variable names for outfile, cannot print dots, so sub them for underscores
		name <-	names(hwsb_rast) ## Create variable to name the raster outfile, this will make the names more
		print(name)
		
		hwsb_resample <-
			resample(hwsbrast, ref_rast) ## Sample both the CRS and Resolution to resample to 2.5m, do not choose a 'method', in this dataset, there are both categorical and continuous variables, allow resample to default based on the inherited values.
		hwsb_ex <- extend(hwsb_resample, extent(ref_rast), value=NA)
		
		print("projected")
		
			raster::writeRaster(
				hwsb_ex,
			filename = paste0(out_folder,"/", name, "_2_5m"),
			..., ## format = "raster"
			overwrite = TRUE
		) ##  Download Soil Type data
		
	} ## Manually delete the 'MU_GLOBAL.nc4'/" or 'HWSD_global_mapping_unit_identifier_2_5m' files, they correspond to something only useful in the native HWSDB viewer.
	
}

# resample_hwsb(input_folder = "raw/rasters/hwsb",
# 							ref_rast = "raw/rasters/srad/wc2.1_2.5m/wc2.1_2.5m_srad_06.tif",
# 							out_folder = "raw/rasters/hwsb"
# )


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
		r <- rast(res=0.5, ymin=-60)
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


stack_env_predictors <- function(wclim_var = wclim_var
																 # esa_var = "yes",
																 # elev_var = FALSE,  
																 # hwsb_var = "yes"
){
	source("bin/scripts/PACKAGES.R") 
	
	## Read in WorldClim Rasters
	climlist <- list.files(paste0("raw/rasters/",wclim_var,"/wc2.1_2.5m"), pattern = "*.tif", full.names = TRUE) 
	print(climlist)
	
	# elev_check <- readLine(prompt = "Have you downloaded Elevation Variables?(yes/no)")
	# if (elev_var) {print("Have you downloaded Elevation Variables?()")
	# 	scan()
	# 	
	# }
	elv_check <- menu(c("Yes", "No"), title="Have you downloaded Elevation Variables?()")
	if (elv_check == 1) {elevlist <- list.files("raw/rasters/elev/wc2.1_2.5m/", pattern = "*.tif", full.names = TRUE)} ## use menu input to check for
		
	
	## Read in ESA WorldCover Rasters
	wdcvrlist <- list.files(paste0("raw/rasters/worldcover/"), pattern = "*2_5m.grd", full.names = TRUE)


	## Read in Harmonized World Soil Database v1.2 Rasters
	hwsblist <- list.files("raw/rasters/hwsb", pattern = "*.grd", full.names = TRUE)

	## Create a list containing the listed files for each category of raster
	
	predictors <- c(lists)

	## Sort them into alphabetical order
	predictors <- mixedsort(sort(predictors))
	print(predictors)
	## Stack the predictors into raster stack object
	predstack <- raster::stack(predictors)

	## Substitute out any dots in names to not interfere with further naming conventions.
	names(predstack) <- gsub("\\.","_", names(predstack))

	return(predstack)
}

# stack_env_predictors(wclim_var = wclim_var)






crop_area <- function(
		dist=dist, 
		region=region
){
	source("bin/scripts/PACKAGES.R") 
	
	for (k in region) {
	for (j in 1:length(dist)) {
		distance <- dist[j]
		print(distance)
		
		## Read in accessions for each distance and Continent
		spdf <-
			read.csv(paste0(
				"rst/samples/", k,
				"/", k,
				"_accessions_",	distance,
				"km.csv"
			))
		
		## Create a dataframe of long/lat coordinates to feed to spatial dataframe
		bw <- spdf[, c(6, 5)]
		
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
			initialAlpha = 5,
			# initial alpha size, try not to set to 1, use a minimum of 5
			clipToCoast = "terrestrial",
			# "the resulting polygon is clipped to the coastline, using a basemap from naturalearth"
			buff = 5000,
			# distance in meters, 5 km
			verbose = TRUE
			# prints alpha to console for debugging purposes
		)
		
		# ## Plot your buffered hull, optional, must be turned off if using supercomputer or have to add dev.off()
		# plot(a_hull[[1]], col=transparentColor('gray50', 0.5), border = NA)
		# points(x = spatdf$LONG, y = spatdf$LAT, cex = 0.5, pch = 3)
		
		## Convert the bbuffered hull to a spatial polygon that can be used to crop rasters
		b_hull <- sf::as_Spatial(st_geometry(a_hull[[1]]))
		
		## Transform using the appropriate CRS: https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf
		b_hull <-
			spTransform(b_hull,
									"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
		
		## Setup for outfile
		# dir.create(paste0("raw/rasters/", k))
		path <- paste0("raw/rasters/", k , "/")
		end <- ".asc"
		print(path)
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
	
}

# crop_area(dist = 5,region = c("Africa","Asia"))


maxent.csv <- function(
		in.dir,
		regions,
		out.csv,
		opt.dist,
		aggregate.model = NULL,
		aggregate.dir,
		...
) {
	
	require(dplyr,quietly = TRUE)
	
	agg.list <- c()
	
	for (r in 1:length(regions)) {
		region <- regions[[r]]
		
		df <- read.csv(file = paste0(in.dir,region,"/",region,"_accessions_",opt.dist,"km.csv"))
		
		if (is.null(aggregate.model)) {
			write.csv(df, paste0(in.dir,region,"/",out.csv),row.names = FALSE)
		} else {	
			df$TOTAL <- "aggregate"
			agg.list[[r]] <- df
			}
		
	}
	
	work <- do.call("rbind",agg.list)
	write.csv(work, paste0(aggregate.dir,out.csv),row.names = FALSE)
	
}

