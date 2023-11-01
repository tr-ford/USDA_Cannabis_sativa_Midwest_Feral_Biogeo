## 05_Maxent_ENM.R
## Script Five
## Tori Ford
### 27-02-23
#### Objective: Use ENMeval to generate a series of Maxent models to evaluate for proper fit.

## Optional Java Options, if you are encountering memory issues, this must be changed per your needs
# options(java.parameters = "-Xmx1024m") 

## Script Modified from https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html 
library(ENMeval) # 2.0.4
library(dismo) # 1.3.9
library(raster) # 3.6.13
library(ecospat) # 3.5
library(rJava) # 1.0.6 ## if 'failed in loadNamespace() for 'rJava', type "system("java -version")" in console. Set R version to the same bit version, or download the correct Java program (32/64 bit), this will break Maxent if not loaded properly
library(rasterVis) # 0.51.5
library(latticeExtra) # 0.6.30
library(parallel) # 4.2.2



## Use the dataframe containing all species information to get your region names, in this case, Continents
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION) ## Name this variable appropriately to your dataset.
regions <- regions[-c(1)] ## Remove any previously run models if failure encountered.

## Use the dataframe containing all species information to get your region names, for the aggregate model
aggdf <- read.csv("rst/samples/aggregate/maxent_ready.csv")
regions <- unique(aggdf$TOTAL) ## Name this variable appropriately to your dataset.

## Run maxent for each region defined
for (k in regions) {
	
	# Set a random seed in order to be able to reproduce this analysis.
	set.seed(333)
	
	## Create a directory for your resulting plots(maps)
	dir.create(paste0("rst/maps/", k))
	
	## Read in accessions for each region
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	occs <- spdf[, c("LONG", "LAT")]
	
	# if(k == ""){occs <- subset(occs, LONG > 100)} ## filter out potential problem point for debugging sake
	
	vif_files <- list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE)
	## For aggregate models
	# vif_files <- list.files(paste0("rst/rasters/",k,"/unique/"), pattern = "*.asc", full.names = TRUE)
	
	## stack VIF selected rasters
	envs <- raster::stack(vif_files) 
	## define a working projection
	projection(envs) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
	
	
	## create empty list for rasters
	continuous <- list()
	categorical <- list()
	
	
	## Create a list of categorical and Continuous predictors by filtering variable name using grepl
	for (e in 1:length(names(envs))) {
		
		## List of grepl-able names
		envname <- names(envs[[e]])

		## Subset out rasters per loop
		check <- envs[[e]]
	
		## Sorts rasters considered "continuous" or "categorical" into a list
		## IF YOU HAVE DOWNLOADED EXTRA CATEGORICAL VARIABLES, MAKE SURE THEY HAVE BEEN FILTERED BY GREP
		if(grepl("monthCountByTemp10",envname) == FALSE){continuous[[e]] <- check} else {categorical[[e]] <- check}

	} 
	
	
	## Stack non-categorical rasters, this will be used for MESS and similarity/difference maps
	envs2 <- raster::stack(unlist(continuous))

	## Create a list of categorical variable names, use unlist to filter NULL, save to call in 07 script
	cats <- names(raster::stack(unlist(categorical)))
	saveRDS(cats, file=paste0("rst/samples/",k,"/categoricals.RData"))

	## Create a stack of categorical rasters,use unlist to filter NULL
	catrast <- raster::stack(unlist(categorical))
	
	print("finished Cat Cont")
	
# Multivariate Environmental Similarity Surface, Predictor Rasters vs. Occurrences ---------------------------
	
	
	## Extract continous raster information under occurrence points,the function will not work with categorical variables
	occs.z <- raster::extract(envs2, occs) 
	
	## Calculate environmental similarity of metrics of the predictor variable extent to the reference points, this will likely return high scores due to alpha hull approach
	occs.sim <- similarity(envs2, occs.z) 
	
	## Subset out a similarity minimum
	occs.mess <- occs.sim$similarity_min
	
	## Make occurrence points spatial
	occs.sp <- sp::SpatialPoints(occs) 
	

	## Create an Environemental Similarity map, using a continous scale
	myScale <- seq(cellStats(occs.mess, min), cellStats(occs.mess, max), length.out = 100)
	##Visualise and Save Environmental Similarity
	pdf(file = paste0("rst/maps/", k, "/","Multivariate_environmental_similarity.pdf"), width = 24, height = 24)
	print(rasterVis::levelplot(occs.mess, main = "Environmental similarity", at = myScale, margin = FALSE) + 
		latticeExtra::layer(sp.points(occs.sp, col="black")))
	dev.off() ## Turns plotting device off, even previous plots
	
	
	# Define a color palette for a categorical scale representing each continous predictor
	nb.cols <- length(continuous) ## length of continous variables list
	cols <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(nb.cols) ## create a palette large enough for dataset
	
	# 'This map shows the variable for each grid cell that is most different from the reference'; Visualise and Save
	pdf(file = paste0("rst/maps/", k, "/","Most_different_environmental_Variable.pdf"), width = 24, height = 24)
	print(rasterVis::levelplot(occs.sim$mod, col.regions = cols, main = "Most different variable") + 
		latticeExtra::layer(sp.points(occs.sp, col="black")))
	dev.off() ## Turns plotting device off, even previous plots
	
	# 'This map shows the variable for each grid cell that is most similar to the reference'; Visualise and Save
	pdf(file = paste0("rst/maps/", k, "/","Most_similar_environmental_Variable.pdf"), width = 24, height = 24)
	print(rasterVis::levelplot(occs.sim$mos, col.regions = cols, main = "Most similar variable") + 
		latticeExtra::layer(sp.points(occs.sp, col="black")))
	dev.off() ## Turns plotting device off, even previous plots
	
print("finished MESS")
	
# Partition Occurrences for Evaluation ------------------------------------
	
	
	bg <- dismo::randomPoints(envs2[[1]], n = 10000) %>% as.data.frame() ## Select a random raster to sample background, pick any number within the length of your files
	# bg <- dismo::randomPoints(envs2[[1]], n = 5000) %>% as.data.frame() ## Select a random raster to sample background, pick any number within the length of your files
	colnames(bg) <- colnames(occs)
	
	## Visualise background points using a random raster
	# plot(envs2[[13]])
	# points(bg, pch = 15, cex = 0.2)
	
	## Create block partition by sampling background points against occurrences in lon_lat format
	block <- get.block(occs, bg, orientation = "lon_lat")
	table(block$occs.grp)
	
	## Save 'Spatial block partitions: occurrences' map
	pdf(file = paste0("rst/maps/", k, "/Spatial_Block_Occurrence_Partitions.pdf"), width = 24, height = 24)
	## Visualise occurrence partitions
	print(evalplot.grps(pts = occs, pts.grp = block$occs.grp, envs = envs2) + 
		ggplot2::ggtitle("Spatial block partitions: occurrences"))
	dev.off() ## Turns plotting device off, even previous plots
	
	## Save 'Spatial block partitions: background' map
	pdf(file = paste0("rst/maps/", k, "/Spatial_Block_Background_Partitions.pdf"), width = 24, height = 24)
	## Visualise background partitions
	print(evalplot.grps(pts = bg, pts.grp = block$bg.grp, envs = envs2) + 
		ggplot2::ggtitle("Spatial block partitions: background"))
	dev.off() ## Turns plotting device off, even previous plots
	
	
	## Extract predictor values for occurrence and background points
	occs.z <- cbind(occs, raster::extract(envs, occs))
	bg.z <- cbind(bg, raster::extract(envs, bg))
	
	## Save 'MESS Partition' map
	pdf(file = paste0("rst/maps/", k, "/Partitioned_Multivariate_environmental_similarity.pdf"), width = 36, height = 36)
	## Multivariate environmental similarity (MESS) of Partitions
	# We use the bb.buf (bounding box buffer) argument to zoom in to our study extent.
	print(evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = envs, occs.z = occs.z, 
											bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
											 bb.buf = 7)) #categoricals = cats,
	dev.off() ## Turns plotting device off, even previous plots
	
	## Save 'most_diff' map
	pdf(file = paste0("rst/maps/", k, "/Partitioned_Most_different_environmental_Variable.pdf"), width = 36, height = 36)
	## Most different environemtnal variables
	print(evalplot.envSim.map(sim.type = "most_diff", ref.data = "occs", envs = envs, occs.z = occs.z, 
											bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
											 bb.buf = 7)) #categoricals = cats,
	dev.off() ## Turns plotting device off, even previous plots
	
	## Save 'most_sim' map
	pdf(file = paste0("rst/maps/", k, "/","Partitioned_Most_similar_environmental_Variable.pdf"), width = 36, height = 36)
	## Most different environemtnal variables
	print(evalplot.envSim.map(sim.type = "most_sim", ref.data = "occs", envs = envs, occs.z = occs.z, 
											bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
											 bb.buf = 7)) #categoricals = cats,
	dev.off() ## Turns plotting device off, even previous plots
	
	print("finished Partitions")
	
# Maxent Modeling ---------------------------------------------------------


	## Use the full range of feature classes and regularization multipliers to develop an ensemble of 20 candidate EN Models, using Maxent.jar
	en_model <- ENMevaluate(occs = occs, 
													envs = envs, 
													bg = bg, 
													algorithm = "maxent.jar", 
													partitions = "block", 
													tune.args = list(fc = c("L","LQ","LQH","H","QH","Q","LH"),
													rm = 1:5),
													# categoricals = cats,
													parallel = TRUE,
													numCores = 4)
	## Save your Maxent Model
	save(en_model, file = paste0("rst/samples/", k, "/ENMeval.RData"))
	
	
	## Calculate Niche Overlap between models,which support Schoener’s D, to compare output similarity
	overlap_D <- calc.niche.overlap(en_model@predictions, overlapStat = "D")
	write.table(overlap_D, file = paste0("rst/samples/", k, "/Schoeners_D_Niche_Overlap.txt"), sep = "\t")
	## Calculate Niche Overlap between models,which support Moran’s I, to compare output similarity
	overlap_I <- calc.niche.overlap(en_model@predictions, overlapStat = "I")
	write.table(overlap_I, file = paste0("rst/samples/", k, "/Morans_I_Niche_Overlap.txt"), sep = "\t")
	
	
	# Overall results
	res <- eval.results(en_model)
	## Save model results
	write.table(res, file = paste0("rst/samples/", k, "/Model_Results.txt"), sep = "\t")

	
	# RasterStack of model predictions (for extent of "envs") with names corresponding to tune.args column label.
	## Plot and Save 4 by 5 Margins, Plots all Ecological Niche Models
	model_pred_ras <- eval.predictions(en_model)
	pdf(file = paste0("rst/maps/", k, "/ENMresults.pdf"), width = 24, height = 24)
	raster::plot(model_pred_ras, nc=4, maxnl=35)
	dev.off() ## Turns plotting device off, even previous plots
	
	## Store environmental values under occurrences
	occurance_envs <- eval.occs(en_model)
	write.table(occurance_envs, file = paste0("rst/samples/", k,"/Occurance_Envs_values.txt"), sep = "\t")
	
	## Store environmental values under background points
	background_envs <- eval.bg(en_model)
	write.table(background_envs, file = paste0("rst/samples/", k,"/Background_Envs_values.txt"), sep = "\t")
	
	
# Sequential Method  - Cross Validation Model Selection -------------------
	# From Vignette: Lobo et al. (2008) and others have pointed out that validation AUC is inappropriate as an absolute performance measure of presence-background ENMs, but it is valid to use for relative comparisons of models constructed with the same data.
	
	
	## Selection of an Optimal Model with cross-validation
	# Filters by lowest omission rate and highest AUC
	opt.seq <- res %>% 
		dplyr::filter(or.10p.avg == min(or.10p.avg)) %>% 
		dplyr::filter(auc.val.avg == max(auc.val.avg))
	write.table(opt.seq, file = paste0("rst/samples/", k, "/Sequential_Model_Results.txt"), sep = "\t")
	
	## Select a single model using the parameters of our opt.seq filter
	mod.seq <- eval.models(en_model)[[opt.seq$tune.args]]
	## Save 'mod.seq' as rdata object
	save(mod.seq, file = paste0("rst/samples/", k, "/optimalSeq_Model.RData"))
	
	## Save and Plot 'mod.seq'
	pdf(file = paste0("rst/maps/", k, "/","Sequential_Variable_Contribution.pdf"), width = 24, height = 24)
	plot(mod.seq)
	dev.off()
	
	## Save and Plot dismo reponse curves
	pdf(file = paste0("rst/maps/", k, "/","Sequential_Response_Curves.pdf"), width = 24, height = 24)
	# par(mar = c(3,3,3,3))
	dismo::response(mod.seq)
	dev.off()
	
	## Create a single optimal raster using the parameters of our opt.seq filter
	pred.seq <- eval.predictions(en_model)[[opt.seq$tune.args]]
	## Save and plot 'pred.seq' model 
	pdf(file = paste0("rst/maps/", k, "/","Sequential_Prediction_Model.pdf"), width = 24, height = 24)
	plot(pred.seq)
	dev.off()
	
	## Save and plot 'pred.seq' model plus partitioned points
	pdf(file = paste0("rst/maps/", k, "/","Sequential_Partitioned_Points_Prediction_Model.pdf"), width = 24, height = 24)
	plot(pred.seq)
	points(eval.bg(en_model), pch = 3, col = eval.bg.grp(en_model), cex = 0.5)
	points(eval.occs(en_model), pch = 21, bg = eval.occs.grp(en_model))
	dev.off()
	
	print("finished Sequential Model")

# AICc Method - Model Selection without Cross Validation ------------------

	
	## Selection of an Optimal Model without cross-validation
	# Filter for lowest delta AICc score (exclude NAs), can filter for 0, but closely observe the result, as the model may be completely empty
	# In practice, models with delta AICc scores less than 2 are usually considered statistically equivalent.
	opt.aicc <- res %>% dplyr::filter(delta.AICc == min(na.exclude(delta.AICc))) #%>% filter(delta.AICc == 0)
	write.table(opt.aicc, file = paste0("rst/samples/", k, "/AICc_Model_Results.txt"), sep = "\t")
	
	## Select a single model using the parameters of our opt.aicc filter
	mod.aicc <- eval.models(en_model)[[opt.aicc$tune.args]]
	## Save 'mod.aicc' as rdata object
	save(mod.aicc, file = paste0("rst/samples/", k, "/optimalAICc_Model.RData"))
	
	## Save and Plot 'mod.aicc'
	pdf(file = paste0("rst/maps/", k, "/","AICc_Variable_Contribution.pdf"), width = 24, height = 24)
	plot(mod.aicc)
	dev.off()
	
	## Save and Plot dismo reponse curves
	pdf(file = paste0("rst/maps/", k, "/","AICc_Response_Curves.pdf"), width = 24, height = 24)
	dismo::response(mod.aicc)
	dev.off()
	
	## Create a single optimal raster using the parameters of our opt.seq filter
	pred.aicc <- eval.predictions(en_model)[[opt.aicc$tune.args]]
	## Save and plot 'pred.seq' model 
	pdf(file = paste0("rst/maps/", k, "/","AICc_Prediction_Model.pdf"), width = 24, height = 24)
	plot(pred.aicc)
	dev.off()
	
	## Save and plot 'pred.seq' model plus partitioned points
	pdf(file = paste0("rst/maps/", k, "/","AICc_Partitioned_Points_Prediction_Model.pdf"), width = 24, height = 24)
	plot(pred.aicc)
	points(eval.bg(en_model), pch = 3, col = eval.bg.grp(en_model), cex = 0.5)
	points(eval.occs(en_model), pch = 21, bg = eval.occs.grp(en_model))
	dev.off()
	
	print("finished AICc Model")
	

	
	## Collect metadata of the model for reproducibility across trials, may save for next script
}
