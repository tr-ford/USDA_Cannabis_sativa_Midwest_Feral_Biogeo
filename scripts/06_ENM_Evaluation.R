## 06_ENM_Evaluation.R
## Script Six
## Tori Ford
### 06-03-23
#### Objective: Test the significance and efficacy of new Maxent models through evaluation statistics and progress across null models
						## This is moreso to compare the proficiency of our model selection parameters:"If the metrics we calculated for our empirical 
						## model were not significantly different from those calculated for a series of null models, 
						## we would have not have high confidence that they meaningfully represent how well our empirical model performed."

library(ENMeval)
library(rJava) ## Please have this installed properly, if not, the null models (if generated with .jar) will not load
library(ggplot2)
library(sf)
library(rnaturalearth)## for alternative plotting
library(viridis)
library(parallel)
library(gtools)
library(ENMTools)
library(ntbox)
library(terra)
library(ggsci)
library(plyr)
library(dplyr)
library(ecospat)
library(scico)

## In this script, we are still operating within the "M"(Go) variable space, where only the clipped rasters, which directly calibrate the model are considered.

## Use the dataframe containing all species information to get your region names, in this case, Continents
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION) ## Name this variable appropriately to your dataset.

## Use the dataframe containing all species information to get your region names, for the aggregate model
aggdf <- read.csv("rst/samples/aggregate/maxent_ready.csv")
regions <- unique(aggdf$TOTAL) ## Name this variable appropriately to your dataset.


# Niche Truncation (https://jasonleebrown.github.io/humboldt/) --------------------------------------------------------
## Only use if comparing different niche spaces.
## Can be completed using the exact same region, but will not inform much

library(devtools)
install_github("jasonleebrown/humboldt")
library(humboldt)

## Can run this analysis with two sampled regions or just one region, if one env1=env2, sp1=sp2

## Read in rasters processed by Maxent model
vif.envs <- stack(mixedsort(sort(list.files(paste0("rst/rasters/Midwest-Feral/"), pattern = "*.asc", full.names = TRUE))))
vif.envs2 <- stack(mixedsort(sort(list.files(paste0("rst/rasters/Midwest/"), pattern = "*.asc", full.names = TRUE))))

## Convert one of the rasters to a point dataframe to sample.  Use any raster input.
env.points <- rasterToPoints(vif.envs[[5]], fun=NULL, spatial=FALSE)
env.points2 <- rasterToPoints(vif.envs2[[5]], fun=NULL, spatial=FALSE)

## Rarefy points to appropriate analysis resolution.  Note it is best to start with climate data that is similar to the desired resolution.  Else this process can take a lot of time.  If climate is exactly the resolution desired (we recommend 10-40km2 for most studies), this step can be skipped.
env.sampling.res <-humboldt.occ.rarefy(env.points, colxy = 1:2, rarefy.dist = 20,  rarefy.units = "km", run.silent.rar = F)
env.sampling.res <-humboldt.occ.rarefy(env.points2, colxy = 1:2, rarefy.dist = 20,  rarefy.units = "km", run.silent.rar = F)

## Subset only the x and y data
env.sampling.res <- env.sampling.res[,1:2]

##Extract values to points from rasters
RAST_VAL <- data.frame(raster::extract(vif.envs, env.sampling.res))

## Merge sampled data to input (If two distinct sampling regions (i.e., native vs introduced) make another envs containing the values for the other region)
envs1 <- cbind(env.sampling.res,RAST_VAL)

## Read in accessions for each region
spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
occs <- spdf[, c("LONG", "LAT")]

## Scrub the rasters from NAs and make sure envs are ported with numericals
envs <- humboldt.scrub.env(envs)

## 'Its highly recommended that you using the function "humboldt.top.env" to select only the important environmental variables in humboldt.doitall. This step can be skipped. If you downloaded tons of environmental data, you should use this step.  '
reduc.vars<- humboldt.top.env(env1=envs,env2=envs,sp1=occs,sp2=occs,rarefy.dist=2.5, rarefy.units="km", env.reso=0.416669,learning.rt1=0.01,learning.rt2=0.01,e.var=(3:19),pa.ratio=4,steps1=30,steps2=30,method="contrib",contrib.greater=5)

## Adjust the number of variables input for e.vars after reduction to only important variables
num.var.e<-ncol(reduc.vars$env2)

zz<-humboldt.g2e(env1=reduc.vars$env1, env2=reduc.vars$env2, sp1=occs, sp2=occs, reduce.env = 0, reductype = "PCA", non.analogous.environments = "NO", env.trim= F, e.var=c(3:num.var.e),  col.env = (3:19), trim.buffer.sp1 = 200, trim.buffer.sp2 = 200, rarefy.dist = 2.5, rarefy.units="km", env.reso=0.41666669, kern.smooth = 1, R = 100, run.silent = F)

scores.env1<-zz$scores.env1[1:2]
scores.env2<-zz$scores.env2[1:2]
scores.env12<- rbind(zz$scores.env1[1:2],zz$scores.env2[1:2])
scores.sp1<-zz$scores.sp1[1:2]
scores.sp2<-zz$scores.sp2[1:2]

## Estimate the Potential Niche Truncation Index
pnt1<- humboldt.pnt.index(scores.env12,scores.env1,scores.sp1,kern.smooth=1,R=100)
pnt2<- humboldt.pnt.index(scores.env12,scores.env2,scores.sp2,kern.smooth=1,R=100)

# If the value is large, there is moderate risk (PNTI= 0.15-0.3) or high risk (PNTI>0.3) that the measured occupied niche does not reflect the species’ fundamental niche due to niche truncation driven by limited available E-space.
# Can use sink() here to capture text output in txt



# Create Null Models to Analyze -------------------------------------------

load(file = paste0("rst/samples/", k, "/ENMeval.RData"))

## Read in region specific, selection method specific optimal model maxent results
results <- read.table(file = paste0("rst/samples/", k, "/AICc_Model_Results.txt"), sep = "\t") %>% as.data.frame()

## Create null models using the results from the optimal model, input the correct fc and rm
mods.null <- ENMnulls(e = en_model, mod.settings = list(fc = "LH", rm = 1), no.iter = 200)

## Save the model as a data object, so y ou do not have to re-run once completed
save(mods.null, file = paste0("rst/samples/", k, "/ENMnulls.RData"))
## Load in the mods.null object if needed.
load(file = paste0("rst/samples/", k, "/ENMnulls.RData"))

## Extract the mods.null results into a table
null.results <- null.emp.results(mods.null)
write.table(null.results, file = paste0("rst/samples/", k, "/Null_Results.txt"), sep = "\t")

## Create a graph of significant tests, save in pdf format
pdf(file = paste0("rst/maps/", k, "/Null_Model_Results.pdf"), width = 12, height = 12)
evalplot.nulls(mods.null, stats = c("or.10p", "auc.val","auc.train","auc.diff","cbi.val"), plot.type = "histogram")
dev.off()

## Create a graph of significant tests, save in svg format
svg(filename = paste0("rst/maps/", k, "/Null_Model_Results.svg"), width = 12, height = 12)
evalplot.nulls(mods.null, stats = c("or.10p", "auc.val","auc.train","auc.diff","cbi.val"), plot.type = "histogram")
dev.off()

## Create a graph of significant tests, save in svg format
png(filename = paste0("rst/maps/", k, "/Null_Model_Results.png"))
evalplot.nulls(mods.null, stats = c("or.10p", "auc.val","auc.train","auc.diff","cbi.val"), plot.type = "histogram")
dev.off()


# Minimal Predicted Area (MPA) Binary  ------------------------------------

for (k in regions) {
	
	
	## Read in accessions for each region
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	occs <- spdf[, c("LONG", "LAT")]
	
	
	# if(k == ""){occs <- subset(occs, LONG > 100)} ## filter out potential problem point for debugging sake
	
	
	## Read in region specific, selection method specific optimal model maxent results
	results <- read.table(file = paste0("rst/samples/", k, "/AICc_Model_Results.txt"), sep = "\t") %>% as.data.frame()
	
	
	## Read in region specific Ecological Niche Model
	load(file = paste0("rst/samples/", k, "/ENMeval.RData"))
	
	
	## Use optimal model results to select optimal model from ENM object, returns raster layer
	pred.model <- eval.predictions(en_model)[[results$tune.args]]
	
	
	# The minimal predicted area (MPA) is the minimal surface obtained by considering all pixels with
	# predictions above a defined probability threshold (e.g. 0.7) that still encompasses 90 percent of the
	# species‘ occurrences (Engler et al. 2004).
	# refer to: https://doi.org/10.1016/j.ecolmodel.2021.109671; when contemplating threshold values.
	k.mpa <- ecospat.mpa(pred.model, Sp.occ.xy = occs, perc = 0.9)
	
	
	## Generate binary map using the MPA as your threshold
	k.bin <- ecospat.binary.model(pred.model, k.mpa)
	
	
	## Convert the model to spatial points
	bin.points <- rasterToPoints(k.bin, spatial = TRUE)
	
	
	## Convert spatial points to a dataframe
	bin.df  <- data.frame(bin.points)
	
	
	## Call world map (can restrict by region, refer to plotting scheme in 06_ENM_Evaluation.R)
	region <- map_data("county" , xlim = c(min(occs$LONG),max(occs$LONG)),ylim = c(min(occs$LAT),max(occs$LAT)))
	
	
	mpa.maps <- ggplot() +
		geom_polygon(data = region, aes(x = long, y = lat, group = group),color = "black", fill = "snow")+
		geom_raster(data = bin.df , aes(x = x, y = y, fill = get(results$tune.args), alpha = 1)) + 
		paletteer::scale_fill_paletteer_c("scico::buda",direction = -1, name = "Prescence \n Threshold") +
		# scale_fill_distiller(palette = "RdYlGn", direction = -1, name = "Minimum \n Training \n Prescence") +
		# scale_fill_viridis(option = "A",direction = 1, name = "Prescence \n Threshold") + ## Use this color when plotting suitability scores or make the fill of 'regmap' darker
		scale_alpha(guide = 'none')+
		ggtitle(paste0("Binary Projection of the Optimal Model of Cannabis Sativa Minimal Predicted Area in NY and the Midwest"))+ 
		# ggtitle(paste0("Binary Projection of the Optimal Model of ",k," Minimal Predicted Area"))+ 
		coord_quickmap()
	
	
	## Save the globally/regionally projected model (change name per needs)
	ggsave(paste0("Binary_Projection_of_Minimal_Predicted_Area.png"),	plot = mpa.maps ,	path = paste0("rst/maps/",k,"/"),	height = 10,	width = 20)
	
}


## DECIDE WHETHER TO DROP THIS OR NOT
# Binary Models (Using the SAME Model Selection Method (Seq/AICc) YOU chose for a shared space projection (Next Script)) -----------------------------------------------------------


## Source function by: AE Melton
source("bin/scripts/functions/ThresholdModel.R")


for (k in regions) {
	
	
	## Read in accessions for each region
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	occs <- spdf[, c("LONG", "LAT")]
	
	
	# if(k == ""){occs <- subset(occs, LONG > 100)} ## filter out potential problem point for debugging sake
	
	
	## Read in region specific, selection method specific optimal model maxent results
	results <- read.table(file = paste0("rst/samples/", k, "/AICc_Model_Results.txt"), sep = "\t") %>% as.data.frame()
	
	
	## Read in region specific Ecological Niche Model
	load(file = paste0("rst/samples/", k, "/ENMeval.RData"))
	
	
	## Use optimal model results to select optimal model from ENM object, returns raster layer
	pred.model <- eval.predictions(en_model)[[results$tune.args]]

	## Turn to terra
	pred.terra <- rast(pred.model)
	
	## Create a binary map using the optimal model
	pred_bin <- ThresholdModel(usr.raster = pred.terra, usr.occs = occs, method = "MPT", output.type = "binary")
	
	
	## Convert the model to spatial points
	bin_points <- rasterToPoints(pred_bin, spatial = TRUE)
	

	## Convert spatial points to a dataframe
	bin_df  <- data.frame(bin_points)
	
	
	## Call world map (can restrict by region, refer to plotting scheme in 06_ENM_Evaluation.R)
	region <- map_data("county" , xlim = c(min(occs$LONG),max(occs$LONG)),ylim = c(min(occs$LAT),max(occs$LAT)))
	
	
	binarymaps <- ggplot() +
		geom_polygon(data = region, aes(x = long, y = lat, group = group),color = "black", fill = "snow")+
		geom_raster(data = bin_df , aes(x = x, y = y, fill = get(results$tune.args), alpha = 1)) + 
		# scale_fill_distiller(palette = "RdYlGn", direction = -1, name = "Minimum \n Training \n Prescence") +
		scale_fill_viridis(option = "A",direction = -1, name = "Prescence \n Threshold") + ## Use this color when plotting suitability scores or make the fill of 'regmap' darker
		scale_alpha(guide = 'none')+
		ggtitle(paste0("Binary Projection of the Optimal Model of ",k," Minimum Suitability"))+ 
		coord_quickmap()
	
	
	## Save the globally/regionally projected model (change name per needs)
	ggsave(paste0("Binary_Projection_of_Optimal_Model.png"),	plot = binarymaps ,	path = paste0("rst/maps/",k,"/"),	height = 10,	width = 20)
}



# Visualize the Models ----------------------------------------------------

## Use the dataframe containing all species information to get your region names, for the aggregate model
aggdf <- read.csv("rst/samples/aggregate/maxent_ready.csv")
regions <- unique(aggdf$TOTAL) ## Name this variable appropriately to your dataset.

## Mapping Sequential (Using "Plasma" "F" Scheme)

for (k in regions) {
	
	## Read in region specific dataframe
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	
	
	## Read in region specific, selection method specific optimal model maxent results
	results <- read.table(file = paste0("rst/samples/", k, "/Sequential_Model_Results.txt"), sep = "\t") %>% as.data.frame()
	
	
	## Read in region specific Ecological Niche Model
	load(file = paste0("rst/samples/", k, "/ENMeval.RData"))
	
	
	## Use optimal model results to select optimal model from ENM object, returns raster layer
	pred.seq <- eval.predictions(en_model)[[results$tune.args]]
	
	## Alternative method of plotting that will also work, needs labels
	# world <- ne_countries(returnclass = "sf")
	# proj4string(pred.seq) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
	# targetCRS <- crs(pred.seq)
	# world_fix <- sf::st_transform(world, targetCRS)
	# 
	# plot(st_geometry(world_fix), col = "grey80", border= "black") #legend=FALSE
	# 
	# plot(pred.seq, axes=FALSE, add = TRUE, legend=FALSE, box = FALSE, col= viridis::mako(99, direction = 1)
	# 		 main=paste0("Habitat Suitability"))

	
	## Convert the optimal model to spatial points
	seq_points <- rasterToPoints(pred.seq, spatial = TRUE)
	
	## Convert spatial points to a dataframe
	seq_df  <- data.frame(seq_points)
	
	
	## filter out potential problem point for debugging sake
	# if(k == ""){spdf <- subset(spdf, LONG > 100)} ## filter out potential problem point for debugging sake
	
	
	## Use ggplot mapping scheme, sourced from natural earth, to call a map of the world, bound by the outermost points in the species dataframe
	# region <- map_data("county", xlim = c(min(spdf$LONG),max(spdf$LONG)),ylim = c(min(spdf$LAT),max(spdf$LAT)))
	region <- map_data("state", xlim = c(min(spdf$LONG),max(spdf$LONG)),ylim = c(min(spdf$LAT),max(spdf$LAT)))
	
	## Create a map of the optimal model, plotted on top of a world map and add points too represent occurrences
	suitability_map <- ggplot() +
		geom_polygon(data = region, aes(x = long, y = lat, group = group),color = "grey10", fill = "grey38")+
		geom_raster(data = seq_df , aes(x = x, y = y, fill = get(results$tune.args),alpha = 1)) + 
		scale_fill_viridis(option = "F",direction = 1, name = "Habitat Suitability") + 
		scale_alpha(guide = 'none')+
		geom_point(data=spdf, aes(x=LONG, y=LAT, color = SPECIES),shape=8,stroke = 0.4,size = 2,alpha = 1, show.legend = TRUE) + #color ="#FFC0CB" ,#355A20
		scale_color_manual(values = c('#355A20')) +
		ggtitle(paste0("Sequential Model Selection - Cannabis sativa Habitat Suitability in ",k))+ 
		coord_quickmap()
	
	# plot(suitability_map) ## debugging
	
	
	## Save the optimal Sequential model
	ggsave(paste0("Sequential_",k, "_Habitat_Suitability.png"),	plot = suitability_map ,	path = paste0("rst/maps/", k, "/"),	height = 10,	width = 20)

}


## Mapping AICc (Using "Viridis" "F" Scheme)

for (k in regions) {
	
	## Read in region specific dataframe
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	
	
	## Read in region specific, selection method specific optimal model maxent results
	results <- read.table(file = paste0("rst/samples/", k, "/AICc_Model_Results.txt"), sep = "\t") %>% as.data.frame()
	
	
	## Read in region specific Ecological Niche Model
	load(file = paste0("rst/samples/", k, "/ENMeval.RData"))
	
	
	## Use optimal model results to select optimal model from ENM object, returns raster layer
	pred.aicc <- eval.predictions(en_model)[[results$tune.args]]
	
	## Alternative method of plotting that will also work, needs labels
	# world <- ne_countries(returnclass = "sf")
	# proj4string(pred.aicc) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
	# targetCRS <- crs(pred.aicc)
	# world_fix <- sf::st_transform(world, targetCRS)
	# 
	# plot(st_geometry(world_fix), col = "grey80", border= "black") #legend=FALSE
	# 
	# plot(pred.aicc, axes=FALSE, add = TRUE, legend=FALSE, box = FALSE, col= viridis::turbo(99, direction = -1), #Region_Crop
	# 		 main=paste0("Habitat Suitability"))
	
	## Convert the optimal model to spatial points
	aicc_points <- rasterToPoints(pred.aicc, spatial = TRUE)
	
	## Convert spatial points to a dataframe
	aicc_df  <- data.frame(aicc_points)
	
	
	## filter out potential problem point for debugging sake
	# if(k == ""){spdf <- subset(spdf, LONG > 100)} 
	
	
	## Use ggplot mapping scheme, sourced from natural earth, to call a map of the world, bound by the outermost points in the species dataframe
	# region <- map_data("county", xlim = c(min(spdf$LONG),max(spdf$LONG)),ylim = c(min(spdf$LAT),max(spdf$LAT)))
	region <- map_data("state", xlim = c(min(spdf$LONG),max(spdf$LONG)),ylim = c(min(spdf$LAT),max(spdf$LAT)))
	
	## Create a map of the optimal model, plotted on top of a world map and add points too represent occurrences
	suitability_map <- ggplot() +
		geom_polygon(data = region, aes(x = long, y = lat, group = group),color = "grey10", fill = "grey38")+
		geom_raster(data = aicc_df , aes(x = x, y = y, fill = get(results$tune.args), alpha = 1)) + 
		scale_fill_viridis(option = "F",direction = 1, name = "Habitat Suitability") +  
		scale_alpha(guide = 'none')+
		geom_point(data=spdf, aes(x=LONG, y=LAT, color = SPECIES),shape=8,stroke = 0.4,size = 2,alpha = 1, show.legend = TRUE) + #color ="#FFC0CB" ,#355A20
		scale_color_manual(values = c('#355A20')) +
		ggtitle(paste0("AICc Model Selection - Cannabis sativa Habitat Suitability in ",k))+ 
		coord_quickmap()
	
	
	# plot(suitability_map) ## debugging
	
	
	## Save the optimal AICc model
	ggsave(paste0("AICc_",k, "_Habitat_Suitability.png"),	plot = suitability_map ,	path = paste0("rst/maps/", k, "/"),	height = 10,	width = 20)
	
}


# library(ecospat)
# library(ENMeval)
# ## Read in region specific dataframe
# spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
# occs <- spdf[, c("LONG", "LAT")]
# 
# 
# ## Read in region specific, selection method specific optimal model maxent results
# results <- read.table(file = paste0("rst/samples/", k, "/AICc_Model_Results.txt"), sep = "\t") %>% as.data.frame()
# 
# 
# ## Read in region specific Ecological Niche Model
# load(file = paste0("rst/samples/", k, "/ENMeval.RData"))
# 
# 
# ## Use optimal model results to select optimal model from ENM object, returns raster layer
# pred.aicc <- eval.predictions(en_model)[[results$tune.args]]
# pred.occs <- raster::extract(pred.aicc,occs)
# pred.aicc <- getValues(pred.aicc)
# pred.aicc <- pred.aicc[!is.na(pred.aicc)]
# min(pred.aicc,pred.occs)
# boyce <- ecospat.boyce(fit = pred.aicc, obs = pred.occs, nclass = 1)
# calc.10p.trainThresh(pred.occs)
