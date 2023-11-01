## 08_Climate_Projection.R
## Script Eight
## Tori Ford
### 16-03-23
#### Objective: Use our global projection models to predict the presence of Cannabis sativa under various climate change scenarios. 

library(geodata)
library(biomod2)
library(ecospat)
library(raster)
library(rangeBuilder)
library(sf)
library(gtools)
library(ggplot2)
library(viridis)
library(usmap)
library(svglite)
library(ggimage)
library(magick)



# Climate Projection ------------------------------------------------------

##https://www.worldclim.org/data/cmip6/cmip6climate.html

## Select your choice of climate prediction model to pull, we're basing our selection on :
## https://www.researchgate.net/figure/Ranking-of-CMIP6-models-The-models-are-ranked-least-rank-is-the-best-based-on-Model_fig4_347296031
## and https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/

## Below is a list of all selections possible, choose Climate model, one Shared Socio-economic Pathway code, one Time Period, and one resoultion

## Select the relevant Climate Model
	c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", 
							"CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", 
							"GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", 
							"MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL")
	
		models <- "GISS-E2-1-H"

## Select the relevant Shared Socio-economic Pathways (ssp): https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/

	c("126","245","370","585")
	
		ssps <- "126"

## Select the relevant time periods

	c("2021-2040","2041-2060","2061-2080")
	
		time_period <- "2021-2040"

## Select the climatic variables to include (match these to the ones you downloaded in script three)

	c("tmin","tmax","prec","bioc")
	
		variables <- c("tmin","tmax","prec","bioc")

## Select a resolution (minutes of a degree)
	
	c(10, 5, 2.5)
	
	 res <- 2.5
	 
	 
# Download Future Environmental Predictors --------------------------------


## Create a directory to save the raw (unclipped/masked) future rasters
dir.create(paste0("raw/rasters/future"))

	 
## Source your functions for downloading and clipping rasters
source("bin/scripts/functions/08_Climate_Projection_Functions.R")

	 
## Use a local function to download one model, in one time period, of one ssp, with one (or multiple) variable(s), in your select resolution. 
## Provide a path to be created within the above "future" directory, assign it to cmip6.dir. EX:"raw/rasters/future/" .
# path = "raw/rasters/future/126ecveg4160"

	## Please supply a cmip6.dir regardless if downloading rasters,IF you choose not to correct your "path" variable manually
	path <- run_cmip6(models = models,
						time_period = time_period,
						ssps = ssps, 
						variables = variables,
						res = res,
						cmip6.dir = "raw/rasters/future/",
						download.opt = NULL)#path = path

	folder <- strsplit(path,'[/]')[[1]][4]
	
	models <- strsplit(folder,'[_]')[[1]][1]
	during <- paste(strsplit(strsplit(folder,'[_]')[[1]][2],'[-]')[[1]], collapse = ' to ')
	celcius <- paste(strsplit(as.character(strsplit(folder,'[_]')[[1]][3]), "")[[1]][c(2,3)], collapse = ".")
	scenario <- strsplit(as.character(strsplit(folder,'[_]')[[1]][3]), "")[[1]][1]
	ssp <- paste0(scenario,"-",celcius,"°C")
	
	## IF you choose not to 
	
	## Provide the same 'path' variable as the last function
	futures <- unlist_future(path = path)
	
	
# Stack Future Environmental Predictors -----------------------------------


## Combine Static Variables with Future Variables

## First, fix the formatting of the future variables
			## Create a list of rasters to match variable name (this is nescessary for dismo to match the rasters during prediction)
			matches <- c("bio", "tmin","tmax","prec")
			
			matchlist <- list.files(paste0("raw/rasters/",matches,"/wc2.1_2.5m"), pattern = "*.tif", full.names = TRUE)
		
			## Stack rasters to match names
			matchstack <- stack(matchlist)
			matchname <- mixedsort(sort(names(matchstack)))
		
			## Use proper names to match future rasters
			names(futures) <- matchname
	
			## Save the global rasters with fixed names
			for (r in 1:length(names(futures))) {
				# Subset raster layer
				fut <- futures[[r]]
				name <- names(fut)
				raster::writeRaster(fut,filename = paste0(path, name),
														format = "raster", overwrite = TRUE)
			}
		
	futulist <- list.files(paste0(path), pattern = "*.grd", full.names = TRUE)
			
			
## Call all other variables not future, we assume they are going to remain consistent 
			
## Read in WorldClim Rasters
	stats <- c("srad","wind","vapr")
			
	climlist <- list.files(paste0("raw/rasters/",stats,"/wc2.1_2.5m"), pattern = "*.tif", full.names = TRUE)
						
## Read in Elevation Rasters
	elevlist <- list.files("raw/rasters/elev/wc2.1_2.5m/", pattern = "*.tif", full.names = TRUE)

## Read in Photoperiod Rasters
	photolist <- list.files("raw/rasters/daylength/", pattern = "*.grd", full.names = TRUE)
	
## Read in ENVIronmental Rasters for Ecological Modeling (ENVIREM) Rasters		
	enviremlist <- list.files("raw/rasters/ENVIREM/", pattern = "*.grd", full.names = TRUE)
	
## Read in gridded National Soil Survey Geographic (gNATSGO) Rasters
	sgolist <- list.files("raw/rasters/gNATSGO/", pattern = "*.grd", full.names = TRUE)

## List rasters to be stacked	
	predictors <- c(climlist, elevlist,photolist,enviremlist,sgolist,futulist) ## Should equal the same number as original full set of rasters (First run, 121 rasters)
	
## Sort them into alphabetical order
	predictors <- mixedsort(sort(predictors))
	
## Stack the predictors into raster stack object
	sharedstack <- raster::stack(c(predictors))

	names(sharedstack) <- gsub("\\.","_", names(sharedstack))
	
	

# Save Future Variables in Shared Space (Option if not Global Mapping) --------
	shared_shp <- vect("raw/shapefiles/midwest_us.shp")
	
	## Setup for outfile
	dir.create(paste0("raw/rasters/future/shared_space"))
	dir.create(paste0("raw/rasters/future/shared_space/",folder))
	path <- paste0("raw/rasters/future/shared_space/", folder, "/")
	end <- ".grd"
	
	## Loop through predstack for dist by region
	for (o in 1:length(names(sharedstack))) {
		# Subset raster layer
		shared <- sharedstack[[o]]
		# Setup file names
		name <- names(shared)
		print(name) ## Just here to check on progress of loop
		out <- paste0(path, name)
		outfile <- paste0(out, end)
		# Terra-ize, Crop and mask
		sharedterr <- rast(shared)
		cropped <- terra::crop(sharedterr, ext(shared_shp))
		masked <- mask(cropped, shared_shp)
		masked <- raster(masked)
		# Write raster
		raster::writeRaster(masked,
												outfile,
												format = "raster",
												overwrite = TRUE)
	}
	
	

# Clip To Accessible Area -------------------------------------------------

	## Read in dataframe of all accessions
	alldf <- read.csv("rst/samples/prelim_accessions.csv")
	
	## Subset countries to continent
	regions <- unique(alldf$REGION)

	regions <- regions[-c(1)]
	
	## Create a future folder in a results folder
	dir.create(paste0("rst/rasters/future"))
	dir.create(paste0("rst/rasters/future/",folder))
	
	
	for (k in regions) {
			set.seed(2023)
			
			## Read in accessions for each distance and Continent
			spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
			
			# if(k == ""){spdf <- subset(spdf, LONG > 100)} ## filter out potential problem point for debugging sake
			
			## Create a dataframe of long/lat coordinates to feed to spatial dataframe
			## THIS MAY BE DIFFERENT PER YOUR NEEDS, if error = "cannot derive coordinates from non-numeric matrix"
			bw <- spdf[, c(3, 2)]
			
			## Create a spatial points dataframe that we can extract coordinates from
			spatdf <- SpatialPointsDataFrame(
				coords = bw,
				data = spdf,
				proj4string = CRS("+proj=longlat +datum=WGS84")
			)
			
			## Use rangebuilder to create an alpha hull, with a 5km buffer
			## Use the same parameters used in 03_accessible_area.R
			a_hull <- rangeBuilder::getDynamicAlphaHull(
				x = spatdf@coords,
				coordHeaders = c("LONG", "LAT"),
				fraction = 1,
				# min. fraction of records we want included (e.g. 1 = 100%)
				partCount = 1,
				# number of polygons, this may change per region, if your rasters are not turning out right, change this code first
				initialAlpha = 3,
				# initial alpha size, try not to set to 1, use a minimum of 5
				clipToCoast = "terrestrial",
				# "the resulting polygon is clipped to the coastline, using a basemap from naturalearth"
				buff = 5000,
				# distance in meters, 5 km
				verbose = TRUE
				# prints alpha to console for debugging purposes
			) ## 1,1,3,"terrestrial",5000,TRUE (parameters for initial run) | MW: 1,2,3,"terrestrial",10000,TRUE
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
			path_up <- paste0(paste(strsplit(gsub("raw","rst",path), '[/]')[[1]][c(1,2,3)], collapse = "/"),"/")

			dir.create(paste0(path_up,folder))
			
			path_new <- paste0(path_up,folder)
			
			dir.create(paste0(path_up,folder, "/",k))
			
			path_out <- paste0(path_up,folder, "/",k , "/")
			
			end <- ".asc"

			## Loop through  for dist by region
			for (o in 1:length(names(sharedstack))) {
				# Subset raster layer
				preds <- sharedstack[[o]]
				# Setup file names
				name <- names(preds)
				print(name) ## Just here to check on progress of loop
				out <- paste0(path_out, name)
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
	

# Predict Using Future Climatic Scenarios ---------------------------------

	## Print variable of output folder for selected model, verify its accuracy
	print(paste0(path_new))
	
	## Create a output directory for plots
	dir.create("rst/maps/future/")
	dir.create(paste0("rst/maps/future/",folder))
	
	
	for (k in regions) {
		
		set.seed(333)
		
		## Read in accessions for each region
		spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
		
		# if(k == ""){spdf <- subset(spdf, LONG > 100)} ## filter out potential problem point for debugging sake
		
		futureregionstack <-  stack(mixedsort(sort(list.files(paste0(path_new,"/",k), pattern = "*.asc", full.names = TRUE))))
	
		## Load the selected Optimal Ecological Niche Model
		load(file = paste0("rst/samples/", k, "/optimalAICc_Model.RData"))
		
		## Read in categorical variables
		cats <- readRDS(paste0("rst/samples/",k,"/categoricals.RData"))
		
		## Use the optimal ENM to project global habitat suitability for the regional dataset, save it in your preffered raster format
		## If you run into a memory heap error here, use gc() to clean environment and free memory, or use options.java to call more memory, or restart R. 
		clim_proj <- dismo::predict(mod.aicc, futureregionstack, factors = cats, progress = 'text', overwrite = TRUE,filename = paste0("rst/rasters/future/",folder,"/",k, "_Climate_Projection.grd"))#
		
		## If your script fails the plotting step (or you want to experiment with plotting), you do not have to run the entire loop again, read in the projection as follows:
		# clim_proj <- raster(paste0("rst/rasters/future/126ecveg4160/",k, "_Climate_Projection.grd"))
		
		clim.proj <- projectRaster(clim_proj, crs = usmap::usmap_crs()@projargs)
		
		## Convert the shared model to spatial points
		clim.points <- rasterToPoints(clim.proj, spatial = TRUE)
		
		## Convert spatial points to a dataframe
		clim.df  <- data.frame(clim.points)
		
		## Load relevent mapping packages
		## If plotting outside of the US, consider using tmaps
		library(usmap)
		library(ggplot2)
		library(svglite)
		library(ggimage)
		
		set.seed(999)
		
		clim_title <- expression(paste("Predicted Habitat Suitability of ", italic("Cannabis sativa"), " in the Midwestern United States from 2061 to 2080, "))
		
		climmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "white",
													fill = "grey15", alpha = 0.25, linewidth = 0.75, cex = 10) +
			geom_raster(data = clim.df , aes(x = x, y = y, fill = layer,alpha = 2)) +
			scale_fill_viridis(option = "H",direction = -1, name = "Habitat Suitability",labels = scales::percent,
												 position = "top")+
			scale_alpha(guide = 'none') +
			labs(title = clim_title,
					 subtitle = paste0("using the ",models," Climate Model in Shared Socioeconomic Pathway ",ssp, " from ", during),
					 size = "Magnitude") +
			theme(legend.position = "top",legend.justification='left',
						legend.title = element_text(size=12), legend.key.height = unit(1, 'cm'),
						legend.key.size = unit(2, 'cm'),plot.title=element_text(size=18),
						plot.subtitle = element_text(size = 14),legend.text=element_text(size=10),
						plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())
		
		ggsave(file=paste0(k,"_Subset_Habitat_Suitability_In_",models,"_during_",during,"_ssp",ssps,".svg"), plot=climmaps,path = paste0("rst/maps/future/",folder), width=22, height=12)
		
		}
	

# Predict Climate Scenarios in Shared Space -------------------------------

	
	dir.create(paste0("rst/maps/future/",folder,"/shared_space"))
	dir.create(paste0("rst/rasters/future/",folder,"/shared_space"))

	
	for (k in regions) {
		
		set.seed(888)
		
		## Read in accessions for each region
		spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
		
		# if(k == ""){spdf <- subset(spdf, LONG > 100)} ## filter out potential problem point for debugging sake
		
		## Read in future shared raster stack
		futrsharedstack <-  stack(mixedsort(sort(list.files(paste0("raw/rasters/future/shared_space/", folder), pattern = "*.grd", full.names = TRUE))))
		
		## Load the selected Optimal Ecological Niche Model
		load(file = paste0("rst/samples/", k, "/optimalAICc_Model.RData"))		
		
		## Read in categorical variables
		cats <- readRDS(paste0("rst/samples/",k,"/categoricals.RData"))
		
		## Use the Climate Model to project global habitat suitability for the regional dataset, save it in your preffered raster format
		## If you run into a memory heap error here, use gc() to clean environment and free memory, or use options.java to call more memory, or restart R. 
		clim_shared_proj <- dismo::predict(mod.aicc, futrsharedstack, factors = cats, progress = 'text', overwrite = TRUE,filename = paste0("rst/rasters/future/",folder,"/shared_space/",k, "_Climate_Projection.grd"))#

		## If your script fails the plotting step (or you want to experiment with plotting), you do not have to run the entire loop again, read in the projection as follows:
		# clim_shared_proj <- raster(paste0("rst/rasters/future/126ecveg4160/",k, "_Global_Climate_Projection.grd"))
		
		## Convert to correct projection
		shared.proj <- projectRaster(clim_shared_proj, crs = usmap::usmap_crs()@projargs)
		
		## Convert the shared model to spatial points
		shared.points <- rasterToPoints(shared.proj, spatial = TRUE)
		
		## Convert spatial points to a dataframe
		shared.df  <- data.frame(shared.points)
		
		## Load relevent mapping packages
		## If plotting outside of the US, consider using tmaps
		library(usmap)
		library(ggplot2)
		library(svglite)
		library(ggimage)
		
		set.seed(999)
		
		shared_title <- expression(paste("Predicted Habitat Suitability of ", italic("Cannabis sativa"), " in the Midwestern United States from 2061 to 2080, "))
		
		sharedmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "white",
													 fill = "grey15", alpha = 0.25, linewidth = 0.75, cex = 10) +
			geom_raster(data = shared.df , aes(x = x, y = y, fill = layer,alpha = 2)) +
			scale_fill_viridis(option = "H",direction = -1, name = "Habitat Suitability",labels = scales::percent,
												 position = "top")+
			scale_alpha(guide = 'none') +
			labs(title = shared_title,
					 subtitle = paste0("using the ",models," Climate Model in Shared Socioeconomic Pathway ",ssp, " from ", during),
					 size = "Magnitude") +
			theme(legend.position = "top",legend.justification='left',
						legend.title = element_text(size=12), legend.key.height = unit(1, 'cm'),
						legend.key.size = unit(2, 'cm'),plot.title=element_text(size=18),
						plot.subtitle = element_text(size = 14),legend.text=element_text(size=10),
						plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())
		
		ggsave(file=paste0(k,"_Subset_Habitat_Suitability_In_",models,"_during_",during,"_ssp",ssps,".svg"), plot=sharedmaps,path = paste0("rst/maps/future/",folder,"/shared_space"), width=22, height=12)
		
		
	}
	
	

# Range Change between Current and Future Climatic Scenario Habitat Suitability --------

	
	## figure out if the binary models need to be native to biomod2 or not
	## will prefer to just run the ThresholdModel() function
	## ThresholdModel does work
	##  -2 : predicted to be lost
	##  -1 : predicted to remain occupied
	##  0 : predicted to remain unoccupied
	##  1 : predicted to be gained
	# bm_BinaryTransformation(spatraster, threshold)
	
	## Source a function by Anthony Melton
	dir.create("rst/samples/future")
	dir.create(paste0("rst/samples/future/",folder))
	
	## Read in dataframe of all accessions
	alldf <- read.csv("rst/samples/prelim_accessions.csv")
	
	## Subset countries to continent
	regions <- unique(alldf$REGION)
	
	for (k in regions) {
		
		## Set reproducibility seed
		set.seed(230)
		
		Gp.proj <- raster(paste0("rst/rasters/shared_space/",k,"_Shared_Space_Projection.grd"))
		Gp.clim <- raster(paste0("rst/rasters/future/",folder,"/shared_space/",k,"_Climate_Projection.grd"))
		
		occs.df <- read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
		occs.ll <- occs.df[, c("LONG", "LAT")]
		
		# The minimal predicted area (MPA) is the minimal surface obtained by considering all pixels with
		# predictions above a defined probability threshold (e.g. 0.7) that still encompasses 90 percent of the
		# species‘ occurrences (Engler et al. 2004).
		Gp.proj.thresh <- ecospat.mpa(Gp.proj, Sp.occ.xy = occs.ll, perc = 0.9)
		Gp.proj.binary <- Gp.proj
		Gp.proj.binary[Gp.proj.binary < Gp.proj.thresh] <- 0
		Gp.proj.binary[Gp.proj.binary >= Gp.proj.thresh] <- 1
		
		Gp.clim.thresh <- ecospat.mpa(Gp.clim, Sp.occ.xy = occs.ll, perc = 0.9)
		Gp.clim.binary <- Gp.clim
		Gp.clim.binary[Gp.clim.binary < Gp.clim.thresh] <- 0
		Gp.clim.binary[Gp.clim.binary >= Gp.clim.thresh] <- 1
		
		range <- BIOMOD_RangeSize(Gp.proj.binary,Gp.clim.binary)
		
		range.df <- range$Compt.By.Models %>% as.data.frame()
		# write.csv(range.df,paste0("rst/samples/future/",folder,"/", k, "_range_change.csv"),row.names = FALSE)
		
		plots <- bm_PlotRangeSize(bm.range = range,do.count = TRUE,do.perc = TRUE,do.maps = TRUE,do.plot = TRUE,row.names = c("layer"))
		
		# ggsave(paste0(ssps,"_",time_period,"_percentage_range_change.png"),	plot = plots$plot.perc ,	path = paste0("rst/maps/future/",folder,"/shared_space/"),	height = 10,	width = 20)
		
		## Get raster of range change
		rangerast <- raster(plots$tab.maps)
		
		## Correct the CRS of the range change raster to match the mapping schema (If not using USmaps package, read the vignette of the mappinng package for protocol)
		rangecrs <- projectRaster(rangerast, crs = usmap::usmap_crs()@projargs)
		
		## Convert the shared model to spatial points
		range_points <- rasterToPoints(rangecrs, spatial = TRUE)
		
		## Convert spatial points to a dataframe
		range_df  <- data.frame(range_points) 
		
		## Add an appropriate title
		range_title <- expression(paste("Predicted Range Change of ", italic("Cannabis sativa"), " in the Midwestern United States "))
	
		## Add loss and gain labels
		rc_labs <- c("loss", "remain occupied", "remain unoccupied", "gain")
		
		rangemaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "white",
													 fill = "grey15",col = "black", alpha = 0.25, linewidth = 0.75, cex = 10) +
			geom_raster(data = range_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
			scale_fill_viridis(option = "C",direction = -1, name = "Range Change", breaks = -2:1,labels = rc_labs,
												 position = "top", limits = c(-2,1))+
			scale_alpha(guide = 'none') +
			labs(title = range_title,
					 subtitle = paste0("using the ",models," Climate Model in Shared Socioeconomic Pathway ",ssp, " from ", during),
					 size = "Magnitude") +
			theme(legend.position = "top",legend.justification='left',
						legend.title = element_text(size=16), legend.key.height = unit(1.2, 'cm'), legend.key.width = unit(4, 'cm'),
						legend.key.size = unit(4, 'cm'),plot.title=element_text(size=26),
						plot.subtitle = element_text(size = 14),legend.text=element_text(size=14),
						plot.background = element_rect(fill = 'white', colour = NA),legend.background=element_blank()) +
			guides(colour = guide_legend(override.aes = list(size=8)))
		# ggsave(paste0(k,"_MPA_Range_Comparison_using",folder,".svg"),	plot = rangemaps ,path = paste0("rst/maps/future/",folder,"/shared_space/"),	height = 10,	width = 20)
		
	}
	

	## Create a GIF of Future Maps
	## Make a list of the images, in numerical order
	images <- mixedsort(list.files("rst/maps/future/", pattern = "*.svg", full.names = TRUE))
	
	## Apply image_read() functin across images
	img_list <- lapply(images, image_read)
	
	## Creates external object of joined images
	img_joined <- image_join(img_list)
	
	## Animate at 0.5 fps
	img_animated <- image_animate(img_joined, fps = 0.5)
	
	## Save final animation in gif format
	image_write(image = img_animated,
							path = "rst/maps/future/All_Range_Change_Maps.gif")
	

# Range Change between Time Periods --------

		## Read in dataframe of all accessions
	alldf <- read.csv("rst/samples/prelim_accessions.csv")
	
	## Subset countries to continent
	regions <- unique(alldf$REGION)
	
	for (k in regions) {
		
		## Set reproducibility seed
		set.seed(230)
		
		# Gp.2021 <- raster("rst/rasters/future/GISS-E2-1-H_2021-2040_126/shared_space/Midwest_Climate_Projection.grd")
		# Gp.2061 <- raster("rst/rasters/future/GISS-E2-1-H_2061-2080_126/shared_space/Midwest_Climate_Projection.grd")
		
		Gp.2021 <- raster("rst/rasters/future/GISS-E2-1-H_2021-2040_585/shared_space/Midwest_Climate_Projection.grd")
		Gp.2061 <- raster("rst/rasters/future/GISS-E2-1-H_2061-2080_585/shared_space/Midwest_Climate_Projection.grd")
		
		occs.df <- read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
		occs.ll <- occs.df[, c("LONG", "LAT")]
		
		# The minimal predicted area (MPA) is the minimal surface obtained by considering all pixels with
		# predictions above a defined probability threshold (e.g. 0.7) that still encompasses 90 percent of the
		# species‘ occurrences (Engler et al. 2004).
		Gp.2021.thresh <- ecospat.mpa(Gp.2021, Sp.occ.xy = occs.ll, perc = 0.9)
		Gp.2021.binary <- Gp.2021
		Gp.2021.binary[Gp.2021.binary < Gp.2021.thresh] <- 0
		Gp.2021.binary[Gp.2021.binary >= Gp.2021.thresh] <- 1
		
		Gp.2061.thresh <- ecospat.mpa(Gp.2061, Sp.occ.xy = occs.ll, perc = 0.9)
		Gp.2061.binary <- Gp.2061
		Gp.2061.binary[Gp.2061.binary < Gp.2061.thresh] <- 0
		Gp.2061.binary[Gp.2061.binary >= Gp.2061.thresh] <- 1
		
		range <- BIOMOD_RangeSize(Gp.2021.binary,Gp.2061.binary)
		
		range.df <- range$Compt.By.Models %>% as.data.frame()
		write.csv(range.df,paste0("rst/samples/future/SSP5-range_change.csv"),row.names = FALSE)
		
		plots <- bm_PlotRangeSize(bm.range = range,do.count = TRUE,do.perc = TRUE,do.maps = TRUE,do.plot = TRUE,row.names = c("layer"))
		
		ggsave(paste0("SSP5-percentage_range_change.png"),	plot = plots$plot.perc ,	path = paste0("rst/maps/future/"),	height = 10,	width = 20)
		
		## Get raster of range change
		rangerast <- raster(plots$tab.maps)
		
		## Correct the CRS of the range change raster to match the mapping schema (If not using USmaps package, read the vignette of the mappinng package for protocol)
		rangecrs <- projectRaster(rangerast, crs = usmap::usmap_crs()@projargs)
		
		## Convert the shared model to spatial points
		range_points <- rasterToPoints(rangecrs, spatial = TRUE)
		
		## Convert spatial points to a dataframe
		range_df  <- data.frame(range_points) 
		
		## Add an appropriate title
		range_title <- expression(paste("Predicted Range Change of ", italic("Cannabis sativa"), " in the Midwestern United States in SSP5"))
		
		## Add loss and gain labels
		rc_labs <- c("loss", "remain occupied", "remain unoccupied", "gain")
		
		rangemaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "white",
														fill = "grey15", alpha = 0.25, linewidth = 0.75, cex = 10) +
			geom_raster(data = range_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
			scale_fill_viridis(option = "C",direction = -1, name = "Range Change", breaks = -2:1,labels = rc_labs,
												 position = "top", limits = c(-2,1))+
			scale_alpha(guide = 'none') +
			labs(title = range_title,
					 subtitle = paste0("from the near (2021-2040) future to the mid-term (2061-2080) future"),
					 size = "Magnitude") +
			theme(legend.position = "top",legend.justification='left',
						legend.title = element_text(size=12), legend.key.height = unit(1, 'cm'),
						legend.key.size = unit(2, 'cm'),plot.title=element_text(size=18),
						plot.subtitle = element_text(size = 14),legend.text=element_text(size=10),
						plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())
		ggsave(paste0("MPA_Range_Comparison_5-SSP.svg"),	plot = rangemaps ,path = paste0("rst/maps/future/"),	height = 10,	width = 20)
		
	}
	
	## Create a GIF of Future Maps
	## Make a list of the images, in numerical order
	images <- mixedsort(list.files("rst/maps/future/", pattern = "*SSP.svg", full.names = TRUE))
	
	## Apply image_read() functin across images
	img_list <- lapply(images, image_read)
	
	## Creates external object of joined images
	img_joined <- image_join(img_list)
	
	## Animate at 0.5 fps
	img_animated <- image_animate(img_joined, fps = 0.5)
	
	## Save final animation in gif format
	image_write(image = img_animated,
							path = "rst/maps/future/SSP_Range_Change_Maps.gif")
	