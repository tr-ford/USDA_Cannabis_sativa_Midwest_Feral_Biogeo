## 07_Shared_Projection.R
## Script Seven
## Tori Ford
### 07-03-23
#### Objective: Project subsetted models into common space to compare them
								## Preliminarily, this will be done by projecting globally. 


library(dismo)
library(ENMTools) ## CRAN version currently, 16 March 2023 unavailable, instead devtools::install_github("danlwarren/ENMTools")
library(raster)
library(magick)
library(terra)
library(ecospat)
library(rasterVis)
library(ggplot2)
library(gtools)
library(rJava) ## Please have this installed properly if you are still calling a maxent.jar model, if not, the model will not load for dismo
library(parallel)
library(sf)
library(viridis)
library(RColorBrewer)
library(paletteer)
library(scico)
library(landscapetools)
library(ntbox) # devtools::install_github('luismurao/ntbox'), MUST have RTools installed

## In this script, we are operating within the "Gp" variable space, where shared space - in which the optimal model is projected - are considered.


# Read in "Shared Region"/Global Rasters ----------------------------------


## Read in WorldClim Rasters (use variable list or individually call the rasters)
wc_var <- c("bio", "tmin","tmax","prec","srad","wind","vapr")
climlist <- list.files(paste0("raw/rasters/",wc_var,"/wc2.1_2.5m"), pattern = "*.tif", full.names = TRUE)

## Read in Elevation Rasters
elevlist <- list.files("raw/rasters/elev/wc2.1_2.5m/", pattern = "*.tif", full.names = TRUE)

## Read in Photoperiod Rasters
photolist <- list.files("raw/rasters/daylength/", pattern = "*.grd", full.names = TRUE)

## Read in ENVIronmental Rasters for Ecological Modeling (ENVIREM) Rasters		
enviremlist <- list.files("raw/rasters/ENVIREM/", pattern = "*.grd", full.names = TRUE)

## Read in gridded National Soil Survey Geographic (gNATSGO) Rasters
sgolist <- list.files("raw/rasters/gNATSGO/", pattern = "*.grd", full.names = TRUE)

## Create a list containing the listed files for each category of raster
predictors <- c(climlist, elevlist,enviremlist,sgolist,photolist) ## The original script runs 115 environmental predictors

## Sort them into alphabetical order
predictors <- mixedsort(sort(predictors))

## Stack the predictors into raster stack object
predstack <- raster::stack(predictors)

## Substitute out any dots in names to not interfere with further naming conventions. 
names(predstack) <- gsub("\\.","_", names(predstack))



# OR Generate Shared Space Rasters ----------------------------------------


shared_shp <- vect("raw/shapefiles/midwest_us.shp")


## Setup for outfile
dir.create("raw/rasters/shared_space")
	path <- "raw/rasters/shared_space/"
	end <- ".grd"
	
	## Loop through predstack for dist by region
	for (o in 1:length(names(predstack))) {
		# Subset raster layer
		preds <- predstack[[o]]
		# Setup file names
		name <- names(preds)
		print(name) ## Just here to check on progress of loop
		out <- paste0(path, name)
		outfile <- paste0(out, end)
		# Terra-ize, Crop and mask
		predterr <- rast(preds)
		cropped <- terra::crop(predterr, ext(shared_shp))
		masked <- mask(cropped, shared_shp)
		masked <- raster(masked)
		# Write raster
		raster::writeRaster(masked,
												outfile,
												format = "raster",
												overwrite = TRUE)
	}
	
	# midwest_shp <- vect("raw/shapefiles/midwest_us.shp")
	# 
	# dir.create("raw/rasters/shared_space/midwest")
	# path <- "raw/rasters/shared_space/midwest/"
	# end <- ".grd"
	# 
	# ## Loop through predstack for dist by region
	# for (o in 1:length(names(predstack))) {
	# 	# Subset raster layer
	# 	preds <- predstack[[o]]
	# 	# Setup file names
	# 	name <- names(preds)
	# 	print(name) ## Just here to check on progress of loop
	# 	out <- paste0(path, name)
	# 	outfile <- paste0(out, end)
	# 	# Terra-ize, Crop and mask
	# 	predterr <- rast(preds)
	# 	cropped <- terra::crop(predterr, ext(midwest_shp))
	# 	masked <- mask(cropped, midwest_shp)
	# 	masked <- raster(masked)
	# 	# Write raster
	# 	raster::writeRaster(masked,
	# 											outfile,
	# 											format = "raster",
	# 											overwrite = TRUE)
	# }
	# 


# Project into Shared Space -----------------------------------------------

## Create shared space directories for raster and map outputs
dir.create("rst/rasters/shared_space")
dir.create("rst/maps/shared_space")


## Use the dataframe containing all species information to get your region names, in this case, Continents
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION) ## Name this variable appropriately to your dataset.
regions <- regions[-c(1)] ## Remove any previously run models if failure encountered.

for (k in regions) {
	
	set.seed(777)
	
	## Read in accessions for each region
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	
	# if(k == ""){spdf <- subset(spdf, LONG > 100)} ## filter out potential problem point for debugging sake
	
	## Read and sort VIF selected Regional Environmental Predictors
	vifstack <- stack(mixedsort(sort(list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE))))
	sharestack <- stack(mixedsort(sort(list.files("raw/rasters/shared_space/", pattern = "*.grd", full.names = TRUE))))
	
	## Generate a list of names of VIF selected rasters 
	vifs <- names(vifstack)
	
	## Subset vif selected rasters from Global/Shared Space Environmental Predictors
	shared <- raster::subset(sharestack,vifs)
	
	## Load the selected Optimal Ecological Niche Model
	load(file = paste0("rst/samples/", k, "/optimalAICc_Model.RData"))
	
	## Read in categorical variables
	cats <- readRDS(paste0("rst/samples/",k,"/categoricals.RData"))
	
	## Use the optimal ENM to project global habitat suitability for the regional dataset, save it in your preffered raster format
	## If you run into a memory heap error here, use gc() to clean environment and free memory, or use options.java to call more memory, or restart R. 
	# shared_proj <- dismo::predict(mod.aicc, shared, factors = cats, progress = 'text', overwrite = TRUE,filename = paste0("rst/rasters/shared_space/",k, "_Midwest_Projection.grd"))#
	shared_proj <- dismo::predict(mod.aicc, shared, factors = cats, progress = 'text', overwrite = TRUE,filename = paste0("rst/rasters/shared_space/",k, "_Shared_Space_Projection.grd"))#
	
	## If your script fails the plotting step (or you want to experiment with plotting), you do not have to run the entire loop again, read in the projection as follows:
	# shared_proj <- raster(paste0("rst/rasters/shared_space/",k, "_Global.grd"))

	
	## Convert the shared model to spatial points
	shared_points <- rasterToPoints(shared_proj, spatial = TRUE)
	
	## Convert spatial points to a dataframe
	shared_df  <- data.frame(shared_points)
	
	## Get Extent of raster to constrain map to region
	exts <- extent(sharestack)
	
	## Call world map (can restrict by region, refer to plotting scheme in 06_ENM_Evaluation.R)
	regmap <- map_data("world",xlim = c(exts@xmin,0),ylim = c(exts@ymin,exts@ymax))
	
	## Create a map of the optimal model, plotted on top of a world map and add points too represent occurrences
	shared_suitability_map <- ggplot() +
		geom_polygon(data = regmap, aes(x = long, y = lat, group = group),color = "snow", fill = "grey38")+
		# geom_polygon(data = regmap, aes(x = long, y = lat, group = group),color = "grey18", fill = "grey30")+
		geom_raster(data = shared_df , aes(x = x, y = y, fill = layer, alpha = 1)) + ## figure out fill
		scale_fill_viridis(option = "G",direction = 1, name = "Habitat Suitability") +
		# scale_fill_distiller(palette = "RdYlGn", direction = 1, name = "Habitat Suitability") +
		# paletteer::scale_fill_paletteer_c("scico::buda",direction = -1, name = "Habitat Suitability") +
		scale_alpha(guide = 'none')+
		geom_point(data=spdf, aes(x=LONG, y=LAT, color = SPECIES),shape=8,stroke = 0.4,size = 2,alpha = 1, show.legend = TRUE) + #color ="#FFC0CB" ,#355A20
		# scale_color_manual(values = c('C.sativa' = 'red')) +
		scale_color_manual(values = c('#355A20')) +
		ggtitle(paste0(k, " Population Subset of Cannabis sativa Habitat Suitability Projected to Shared Space"))+ 
		coord_quickmap()
	
	## Save the globally/regionally projected model (change name per needs)
	# ggsave(paste0("Habitat_Suitability_Midwest_for_",k,"_Subset.png"),	plot = shared_suitability_map ,	path = "rst/maps/shared_space/",	height = 10,	width = 20)
	ggsave(paste0("Habitat_Suitability_in_Shared_Space_for_",k,"_Subset.png"),	plot = shared_suitability_map ,	path = "rst/maps/shared_space/",	height = 10,	width = 20)
	
	}




# Minimal Predicted Area (MPA) Binary  ------------------------------------

# Use the dataframe containing all species information to get your region names, in this case, Continents
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION) ## Name this variable appropriately to your dataset.

for (k in regions) {
	
	
	## Read in accessions for each region
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	occs <- spdf[, c("LONG", "LAT")]
	
	## Read in the shared space raster
	shared_proj <- raster("rst/rasters/shared_space/Midwest_Shared_Space_Projection.grd")
	
	
	# The minimal predicted area (MPA) is the minimal surface obtained by considering all pixels with
	# predictions above a defined probability threshold (e.g. 0.7) that still encompasses 90 percent of the
	# species‘ occurrences (Engler et al. 2004).
	percentages <- seq(from = 0.05, to = 1, by = 0.05)
	
	## For each sequential percentile, create a binary threshold and plot
	## Consider saving each of these as svgs or pngs and then use magick to create a gif
	## https://www.nagraj.net/notes/gifs-in-r/
	for (p in percentages) {
		percent <- paste0((p*100),"%")
		print(percent)
		
		## Use MPA to generate a certain threshold based on percentage of included records
		k.mpa <- ecospat.mpa(shared_proj, Sp.occ.xy = occs, perc = p)
		
		## Generate binary map using the MPA as your threshold
		k.bin <- ecospat.binary.model(shared_proj, k.mpa)

		# Convert projection to the same format for USmap
		bin.proj <- projectRaster(k.bin, crs = usmap::usmap_crs()@projargs)
	
	
		## Convert the model to spatial points
		bin.points <- rasterToPoints(bin.proj, spatial = TRUE)
	
	
		## Convert spatial points to a dataframe
		bin.df  <- data.frame(bin.points)
	
	
		## Load relevent mapping packages
		library(usmap)
		library(ggplot2)
		library(svglite)
		library(ggimage)
	
		set.seed(230)
	
		title <- expression(paste("Minimal Predicted Area of Abiotically Suitable Area for ", italic(" Cannabis sativa "), " in the Midwestern United States"))
		
		csmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "white",
												fill = "grey15", alpha = 0.25, linewidth = 0.75, cex = 10) +
			geom_raster(data = bin.df , aes(x = x, y = y, fill = layer,alpha = 2)) +
			scale_fill_viridis(option = "H",direction = -1, name = "Abiotic Suitability", breaks = 0:1,labels = c("Unsuitable", "Suitable"),
												 position = "top", limits = c(0,1))+
			scale_alpha(guide = 'none') +
			labs(title = title,
					 subtitle = paste0("Using ", percent, " of occurrence records, resulting in a suitability threshold of: ", k.mpa),
					 size = "Magnitude") +
			theme(legend.position = "top",legend.justification='left',
						legend.title = element_text(size=12), legend.key.height = unit(1, 'cm'),
						legend.key.size = unit(2, 'cm'),plot.title=element_text(size=18),
						plot.subtitle = element_text(size = 14),legend.text=element_text(size=10),
						plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())
		
		ggsave(file=paste0("MPA_Suitability_",paste0((p*100)),"_percent_Map.svg"), plot=csmaps,path = "rst/maps/shared_space/", width=18, height=12)
	}
}

## Create a GIF of MPA Maps
## Make a list of the images, in numerical order
images <- mixedsort(list.files("rst/maps/shared_space/", pattern = "*percent_Map.svg", full.names = TRUE))

## Apply image_read() functin across images
img_list <- lapply(images, image_read)

## Creates external object of joined images
img_joined <- image_join(img_list)

## Animate at 1 fps
img_animated <- image_animate(img_joined, fps = 1)

## Save final animation in gif format
image_write(image = img_animated,
						path = "rst/maps/shared_space/MPA_Percentage_Maps.gif")

# Mobility-Oriented Parity (MOP) Analysis ---------------------------------


## Use the dataframe containing all species information to get your region names, in this case, Continents
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION) ## Name this variable appropriately to your dataset.

## Download a dependency for KUENM
devtools::install_github("cjbwalsh/hier.part")
## Download KUENM
devtools::install_github("marlonecobos/kuenm")
library(kuenm)

for (k in regions) {
	
	set.seed(333)
	
	## Read and sort VIF selected Regional Environmental Predictors
	vifstack <- stack(mixedsort(sort(list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE))))
	
	## Read in the rasters for the extrapolated area (shared maps or regions)
	sharestack <- stack(mixedsort(sort(list.files("raw/rasters/shared_space/", pattern = "*.grd", full.names = TRUE))))
	
	## Generate a list of names of VIF selected rasters 
	vifs <- names(vifstack)
	
	## Subset vif selected rasters from Global/Shared Space Environmental Predictors
	regionals <- raster::subset(sharestack,vifs)
	
	mop_res <- kuenm_mop(M.variables = vifstack,
											 G.stack = regionals, 
											 percent = 10,
											 comp.each=2000,
											 parallel = TRUE,
											 n.cores = 8)
	
	assign(paste0(k,"_mop"),mop_res)
	
	## Write out the MOP raster generated
	raster::writeRaster(mop_res,filename = paste0("rst/rasters/shared_space/",k, "_mobility_oriented_parity_analysis"),format = "raster", overwrite = TRUE) 
	
	# Convert projection to the same format for USmap
	mop_proj <- projectRaster(mop_res, crs = usmap::usmap_crs()@projargs)
	
	## Convert the model to spatial points
	mop_points <- rasterToPoints(mop_proj, spatial = TRUE)
	
	## Convert spatial points to a dataframe
	mop_df  <- data.frame(mop_points)
	
	## Correct the layer name
	mop_df <- dplyr::rename(mop_df, layer = 1)
	
	## Load relevent mapping packages
	## If plotting outside of the US, consider using tmaps
	library(usmap)
	library(ggplot2)
	library(svglite)
	library(ggimage)
	
	set.seed(999)
	
	mop_title <- expression(paste("Mobility-Oriented Parity (MOP) Analysis of Feral Midwest ", italic("Cannabis sativa"), " to the Midwestern United States"))
	
	mopmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "white",
											 fill = "grey15", alpha = 0.25, linewidth = 0.75, cex = 10) +
		geom_raster(data = mop_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
		scale_fill_viridis(option = "H",direction = -1, name = "Extrapolation",labels = c("high","low"),
											 position = "top", breaks = c(0,1), limits = c(0,1))+
		scale_alpha(guide = 'none') +
		labs(title = mop_title,
				 # subtitle = "Occurrences Sourced from 13 Repositories, 10 Digitized, 3 University-Level Project Contributions",
				 size = "Magnitude") +
		theme(legend.position = "top",legend.justification='left',
					legend.title = element_text(size=16), legend.key.height = unit(1.2, 'cm'), legend.key.width = unit(4, 'cm'),
					legend.key.size = unit(2, 'cm'),plot.title=element_text(size=26),
					plot.subtitle = element_text(size = 14),legend.text=element_text(size=14),
					plot.background = element_rect(fill = 'white', colour = NA),legend.background=element_blank())
	
	ggsave(file="MW_Mobility-Oriented_Parity_Analysis.svg", plot=mopmaps,path = "rst/maps/shared_space/", width=20, height=12)
}


# Alternative Mobility-Oriented Parity (MOP) Analysis ---------------------


## Alternative function if kuenm is not happy with you :) (only tested on Windows machine, 64-bit R 4.2.2)
devtools::install_github("HemingNM/ENMwizard")

library(ENMwizard)

for (k in regions) {
	
	set.seed(333)
	
	## Read and sort VIF selected Regional Environmental Predictors
	vifstack <- stack(mixedsort(sort(list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE))))
	
	## Read in the rasters for the extrapolated area (shared maps or regions)
	sharestack <- stack(mixedsort(sort(list.files("raw/rasters/shared_space/midwest/", pattern = "*.grd", full.names = TRUE))))
	
	## Generate a list of names of VIF selected rasters 
	vifs <- names(vifstack)
	
	## Subset vif selected rasters from Global/Shared Space Environmental Predictors
	regionals <- raster::subset(sharestack,vifs)
	
	# ## Mop, Save Raster Layer genrated from MOP analysis to shared space folder.
	# mop_res <- mop(M = vifstack, 
	# 							 G = regionals,
	# 							 p=0.05, ## Must be >0 and <=1
	# 							 filename=paste0("rst/rasters/shared_space/",k, "_mobility_oriented_parity_analysis.grd"), 
	# 							 numCores=8)
	## Mop, Save Raster Layer genrated from MOP analysis to shared space folder.
	mop_res <- mop(M = vifstack, 
								 G = regionals,
								 p=0.05, ## Must be >0 and <=1
								 filename=paste0("rst/rasters/",k,"/",k, "_mobility_oriented_parity_analysis.grd"), 
								 numCores=8)
	assign(paste0(k,"_mop"),mop_res)
	
	
}

for (k in regions) {
	
	set.seed(333)
	
	## Read and sort VIF selected Regional Environmental Predictors
	vifstack <- stack(mixedsort(sort(list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE))))
	
	## Read in the rasters for the extrapolated area (shared maps or regions)
	sharestack <- stack(mixedsort(sort(list.files("raw/rasters/shared_space/midwest/", pattern = "*.grd", full.names = TRUE))))
	
	## Generate a list of names of VIF selected rasters 
	vifs <- names(vifstack)
	
	## Subset vif selected rasters from Global/Shared Space Environmental Predictors
	regionals <- raster::subset(sharestack,vifs)
	
	## Mop, this can take a very long time, and is poorly optimized, smaller shared areas will work non-intensively
	mop_res <- ntbox::mop(M_stack = vifstack,
												G_stack = regionals, 
												percent = 10,
												comp_each=2000,
												parallel = TRUE,
												normalized = TRUE,
												ncores = 8)
	assign(paste0(k,"_mop"),mop_res)
	
	## If forced to kuenm mop,  need to make folder of copied global rasters to its own directory under raw/raster/k~
	
	## Save Raster Layer genrated from MOP analysis to shared space folder. 
	raster::writeRaster(mop_res,filename = paste0("rst/rasters/",k,"/",k, "_mobility_oriented_parity_analysis"),format = "ascii", overwrite = TRUE) 
	# raster::writeRaster(mop_res,filename = paste0("rst/rasters/shared_space/",k, "_mobility_oriented_parity_analysis"),format = "ascii", overwrite = TRUE) 
}


# STOP HERE FOR BIOGEOGRAPHY PROJECT, novel code below --------------------



# Compare Niche Suitability Between Shared Space Rasters (Population or Species Level Comparison) ------------------

## Source a function by Anthony Melton
source("bin/scripts/functions/ThresholdModel.R")

## Source a modified biomod2 function
source("bin/scripts/functions/compare_Gp.R")

Gp.one <- rast(paste0("rst/rasters/shared_space/Midwest-Feral_Shared_Space_Projection.grd"))
Gp.two <- rast(paste0("rst/rasters/shared_space/NY-Trial_Shared_Space_Projection.grd"))

one.df <- read.csv("rst/samples/Midwest-Feral/maxent_ready.csv")
one.occs <- one.df[, c("LONG", "LAT")]
Gp.one.binary <- ThresholdModel(usr.raster = Gp.one,usr.occs = one.occs,method = "90pct", output.type = "binary")

two.df<- read.csv("rst/samples/NY-Trial/maxent_ready.csv")
two.occs <- two.df[, c("LONG", "LAT")]
Gp.two.binary <- ThresholdModel(usr.raster = Gp.two,usr.occs = two.occs,method = "90pct", output.type = "binary")

compare.Gp(
	Gp.one.binary = Gp.one.binary,
	Gp.two.binary = Gp.two.binary,
	name.Gp.one = "Midwest Feral",
	name.Gp.two = "New York Trial",
	plot.name = "Comparison_MW_NY_90pct",
	plot.outdir = "rst/maps/shared_space/",
	threshold.type = "90th Percentile"
)

# Threshold Projections for Binary Comparisons ----------------------------


## Use the dataframe containing all species information to get your region names, in this case, Continents
alldf <- read.csv("rst/samples/prelim_accessions.csv")
continents <- unique(alldf$CONTINENT) ## Name this variable appropriately to your dataset.
# continents <- continents[-c(5, 2, 3, 4)] ## Remove any previously run models if failure encountered.


## Source function by: Cecina Babich Morrow
source("bin/scripts/functions/sdm_thresholding_CBM.R")


## Save 'Shared Space Binary Comparison' map
pdf(file = paste0("rst/maps/shared_space/Binary_Comparisons.pdf"), width = 26, height = 24)

par(mfrow = c(3,2),  mar = c(8,4,4,4))

for (k in continents) {
	
	
	## Read in accessions for each region
	spdf <-	read.csv(paste0("rst/samples/", k,"/", k,"_accessions_5km.csv"))
	occs <- spdf[, c("LONG", "LAT")]
	
	
	# if(k == ""){occs <- subset(occs, LONG > 100)} ## filter out potential problem point for debugging sake
	
	
	# projs <- raster(paste0("rst/rasters/shared_space/",k, "_Global_Projection.grd"))
	projs <- raster(paste0("rst/rasters/shared_space/",k, "_Global.grd"))
	
	
	## Create a binary map using the shared projection, only mtp is working atm, will have to tweak function when time lets me
	## Note:: ALL values not above the minumum threshold (including NA and no data) will be coerced to a value of 0, this allows us to plot the distinction between the areas
	## This can easily be misinterpreted as the '0' value area still having minumum capacity for prescence, in this model, IT DOES NOT, see next model scheme for alternative. 
	mtp_bin <- sdm_threshold(projs, occs, type = "mtp", binary = TRUE)
	# plot(p10_bin)
	# 
	assign(paste0(k,"_bin"),mtp_bin)
	
	## Plot the Minumum Training Presence Binary Projection
	raster::plot(get(paste0(k,"_bin")), main = paste0("Optimal Model of ",k," Minimum Training Prescence's Binary Projection"),horizontal = TRUE,
							 legend.args=list(text='No Presence - Minumum Presence',side=3, font=2, line=0.5, cex=0.8),col = paletteer_d("nord::frost"),
							 legend.mar = 1)
	
	## can try terra clamp
}

dev.off() ## Make sure to run this to save your binary comparison map. 


# Niche Breadth (Can choose between binary or non-binary output)-----------------------------------------------------------


## Use the dataframe containing all species information to get your region names, in this case, Continents
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION) ## Name this variable appropriately to your dataset.

## Source a function by Anthony Melton
source("bin/scripts/functions/ThresholdModel.R")

for (k in regions) {
	
	## Read in raster via 'terra', 'raster' is no longer compliant with ENMTOOLS as of March 2023
	# projs <- raster(paste0("rst/rasters/shared_space/",k, "_Shared_Space_Projection.grd"))
	projs <- terra::rast(paste0("rst/rasters/shared_space/",k, "_Shared_Space_Projection.grd"))
	
	breadth <- ENMTools::raster.breadth(x = projs,verbose = TRUE)
	
	assign(paste0(k,".breadth.B1"),breadth$B1)
	assign(paste0(k,".breadth.B2"),breadth$B2)
	
	## Read in accessions for each region
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	occs <- spdf[, c("LONG", "LAT")]
	
	# if(k == ""){occs <- subset(occs, LONG > 100)} ## filter out potential problem point for debugging sake
	
	projs[projs < get(paste0(k,".breadth.B2"))] <- NA 
	
	## Use suitability of niche breadth to develop binary rasters thresholded to: MPT (minimum presence threshold), 90pct or 95pct
	## Output types: "binary" or "suitability.scores", choose one
	binary_breadth <- ThresholdModel(usr.raster = projs,usr.occs = occs,method = "MPT",output.type = "binary")#
	
	## Convert the shared model to spatial points
	thr_points <- rasterToPoints(binary_breadth,spatial = TRUE)
	
	## Convert spatial points to a dataframe
	thr_df  <- data.frame(thr_points)
	
	## Get Extent of raster to constrain map to region
	exts <- extent(binary_breadth)
	
	## Call world map (can restrict by region, refer to plotting scheme in 06_ENM_Evaluation.R)
	regmap <- map_data("world",xlim = c(exts@xmin,exts@xmax),ylim = c(exts@ymin,exts@ymax))
	
	## Plot using a ggthemes palette.
	threshmaps <- ggplot() +
		geom_polygon(data = regmap, aes(x = long, y = lat, group = group),color = "black", fill = "snow")+
		geom_raster(data = thr_df , aes(x = x, y = y, fill = layer, alpha = 1)) + 
		scale_fill_viridis(option = "A",direction = 1, name = "Prescence \n Threshold") + ## Use this color when plotting suitability scores or make the fill of 'regmap' darker
		# scale_fill_distiller(palette = "RdYlGn", direction = 1, name = "Prescence \n Threshold") +
		scale_alpha(guide = 'none')+
		ggtitle(paste0("Shared Space Projection of ",k," Minimum Presence Threshold Binary Projection"))+ ## think of better name idk right now
		coord_quickmap()
	
	## Save the globally/regionally projected model (change name per needs)
	ggsave(paste0(k,"_Binary_Projection_of_Minimum_Presence_Threshold.png"),	plot = threshmaps ,	path = "rst/maps/shared_space/",	height = 10,	width = 20)
	
	# plot(threshmaps)
}