## Code_Scraps.R
## 16-03-23
## For all the code we scrap but may be useful in the future. The non-commits.



# Script 6 ----------------------------------------------------------------

# ## Sequential Model
# for (k in continents) {
# 	
# load(file = paste0("rst/samples/", k, "/ENMeval.RData"))
# assign(paste0(k,"_en_model"),en_model)
# 
# results <- read.table(file = paste0("rst/samples/", k, "/Sequential_Model_Results.txt"), sep = "\t") %>% as.data.frame()
# ## Circle back for results pull, need 
# print(results$fc)
# print(results$rm)
# 
# 
# ## Create a null model using the same parameters as the optimal model, use get to call unique model
# # 'We first run the null simulations with 100 iterations to get a reasonable null distribution for comparisons with the empirical values'
# mod.null <- ENMnulls(en_model, mod.settings = list(fc = results$fc, rm = results$rm), no.iter = 100)#, parallel = TRUE, numCores = 4,parallelType = "doParallel")
# # mod.null <- ENMnulls(model, mod.settings = list(fc = results$fc, rm = results$rm), no.iter = 100, parallel = TRUE, numCores = 2)
# ## If the models "process" but the resulting tables contain NAs or NAn values, rJava is not properly loaded.
# 
# 
# print("modeled")
# assign(paste0(k,"_nulls"),mod.null)
# 
# null_res <- null.emp.results(get(paste0(k,"_nulls")))
# write.table(null_res, file = paste0("rst/samples/", k, "/Model_Null_Performance.txt"), sep = "\t")
# 
# }

## ~(ENMTools is overhauling atm, will check later to see if CRAN repository goes back up.:( )~
# It is back up, as of March 8th 2023, however only devtools::install_github() worked



## Can compare PCA over multiple stacks if needed
# multistack.pca()

## Attempt a raster PCA using ENMTools, doesn't quite work :(
# 
# for (k in continents) {
# 	## Read in VIF selected layers
# 	vif_pred <- list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE)
# 
# 	## Sort them into alphabetical order
# 	vif_pred <- mixedsort(sort(vif_pred))
# 
# 	## Stack the "region" clipped rasters
# 	vif_envs <- terra::rast(vif_pred)
# 
# 	## Calculate a PCA for
# 	vif.pca <- ENMTools::raster.pca(vif_envs,2)
# 
# 	assign(paste0(k,".pca"),vif.pca)
# 	print(paste0(k, " done"))
# }


# Correlation Matrix ------------------------------------------------------


# ## Visualising a Correlation Matrix for Our Environemental Layers
# for (k in continents) {
# 	## Read in VIF selected layers
# 	vifs <- list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE)
# 
# 	## Sort them into alphabetical order
# 	vifs <- mixedsort(sort(vifs))
# 
# 	## Stack the "region" clipped rasters
# 	envs <- terra::rast(vifs)
# 
# 	## Use ENM Tools to generate correlation matrix and plot
# 	env_matrix <- raster.cor.matrix(envs)
# 	env_matrixs <- as.matrix(env_matrix) 
# 	env_melt <- melt(env_matrixs)
# 	
# 	# replace NA with upper triangle matrix
# 	env_matrixs[upper.tri(env_matrixs)] <- NA
# 	
# 	# reduce the corr matrix
# 	env_half_melt <- melt(env_matrixs)
# 	
# 	mirrored_hm <- ggplot(data = env_melt, aes(x=Var1, y=Var2, fill=value)) + 
# 		geom_tile() +
# 		scale_fill_viridis("Pearson's Correlation",option = "C",direction = -1, guide = "colorbar",space = "Lab")+
# 		labs(title="Pearson's Correlation between Environmental Predictors Selected through VIF")+
# 		theme(axis.text.x = element_text(angle = 45, vjust = 1, 
# 																		 size = 8, hjust = 1),				
# 					axis.title.x = element_blank(),
# 					axis.title.y = element_blank())+
# 		coord_fixed()
# 	
# 	ggsave(paste0("Mirrored_VIF_Heatmap.png"),	plot = mirrored_hm ,	path = paste0("rst/maps/", k, "/"),	height = 10,	width = 20)
# 		
# 	hm <- ggplot(data = env_half_melt, aes(x=Var1, y=Var2, fill=value)) + 
# 		geom_tile() +
# 		scale_fill_viridis("Pearson's Correlation",option = "C",direction = -1, guide = "colorbar",space = "Lab",na.value = "snow")+
# 		labs(title="Pearson's Correlation between Environmental Predictors Selected through VIF")+
# 		theme(axis.text.x = element_text(angle = 45, vjust = 1, 
# 																		 size = 8, hjust = 1),
# 					axis.title.x = element_blank(),
# 					axis.title.y = element_blank())+
# 		coord_fixed()
# 	ggsave(paste0("VIF_Heatmap.png"),	plot = hm ,	path = paste0("rst/maps/", k, "/"),	height = 10,	width = 20)
# 	
# }

# 
# # PCA https://www.esri.com/arcgis-blog/products/arcgis-pro/imagery/multidimensional-pca/ ---------------------------------------------------------------------
# 
# 
# ## Attempt a raster PCA using ntBox (niche tools)
# ## If you have classified your categorical variables using characters (low/high, rather than 1/4), instead of using PCA, ... 
# ## consider doing an analysis with MFA - Multiple Factor Analysis (refer: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/)
# for (k in regions) {
# 	
# 	## Create directories for PCs and Raster PCs output
# 	dir.create(paste0("rst/PCs"))
# 	dir.create(paste0("rst/PCs/",k))
# 	
# 	## Read, sort and stack VIF selected layers
# 	vif_envs <- raster::stack(mixedsort(sort(list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE))))
# 	test_vifs <- vif_envs
# 	test_vifs[is.na(test_vifs)] <- -111
# 	
# 	## Read in, sort and stack "shared space" layers
# 	all_envs <- raster::stack(mixedsort(sort(list.files(paste0("raw/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE))))
# 	
# 	## Generate a list of names of VIF selected rasters 
# 	vifs <- names(vif_envs)
# 	
# 	## Subset vif selected rasters from Global/Shared Space Environmental Predictors
# 	rast_subset <- raster::subset(all_envs,vifs)
# 	
# 	## Calculate a PCA for M variables (accessible area) vs G variables (shared projection)
# 	extr.pca <- ntbox::spca(layers_stack = test_vifs,layers_to_proj = rast_subset, pca_obj = NULL, # get(paste0(k,".pca"))
# 													sv_dir= paste0("rst/PCs/",k),layers_format=NULL)
# 	print(paste0(k, " done"))
# 	
# 	assign(paste0(gsub("-","_",k),".m.g.pca"),extr.pca)
# 	print("assigned")
# 	
# 	## Following code (relative loadings) from "din" on stackoverflow: https://stackoverflow.com/questions/43407859/how-do-i-find-the-link-between-principal-components-and-raw-datas-variables
# 	pcas <- extr.pca$pc_results
# 	loading <- pcas$rotation
# 	loadings_relative <- t(t(abs(loading))/rowSums(t(abs(loading))))*100
# 	
# 	write.csv(loadings_relative, paste0("rst/PCs/",k,"/Projection_PC_loadings.csv"), row.names = TRUE)
# 	print("loadings dones")
# 	
# }
# 
# for (k in regions) {
# 	
# 	# ## Create directories for PCs and Raster PCs output
# 	# dir.create(paste0("rst/PCs"))
# 	# dir.create(paste0("rst/PCs/",k))
# 	
# 	## Read, sort and stack VIF selected layers
# 	vif_envs <- raster::stack(mixedsort(sort(list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE))))
# 	
# 	## Read in population df
# 	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
# 	
# 	##### Extract value for each point
# 	ptExtracted <- raster::extract(vif_envs, spdf[3:2])
# 	
# 	#### Convert to data frame
# 	ptExtracteddf <- as.data.frame(ptExtracted)
# 	
# 	#### Add species name
# 	ptExtracteddf <- ptExtracteddf %>%
# 		dplyr::mutate(name = as.character(spdf$SPECIES), x = spdf$LONG, y = spdf$LAT)
# 	
# 	## 
# 	vif_names <- names(vif_envs)
# 	
# 	#### Drop any NA
# 	ptExtracteddf <- ptExtracteddf %>% 
# 		tidyr::drop_na(all_of(vif_names))
# 	
# 	## PCA 
# 	### Create two dataframes.
# 	num <- ncol(ptExtracted)
# 	spec_num <- num+1
# 	data.bioclim <- ptExtracteddf[, 1:num]
# 	data.species <- ptExtracteddf[, (spec_num):ncol(ptExtracteddf)]
# 	
# 	## Remove any constant/zero colunms
# 	data.bioclim <- data.bioclim[ , which(apply(data.bioclim, 2, var) != 0)]
# 	
# 	### Using only the bioclim columns to run the principal components analysis.
# 	data.pca <- prcomp(data.bioclim, scale. = TRUE) 
# 	
# 	loadings <- data.pca$rotation
# 	summary(loadings)
# 	
# 	##### There are two options to convert the loading to show the relative contribution, 
# 	##### they both give the same answer so either can be used.
# 	loadings_relative_A <- t(t(abs(loadings))/rowSums(t(abs(loadings))))*100
# 	loadings_relative_A
# 	
# 	theme <- theme(panel.background = element_blank(),
# 								 panel.border=element_rect(fill=NA),
# 								 panel.grid.major = element_blank(),
# 								 panel.grid.minor = element_blank(),
# 								 strip.background=element_blank(),
# 								 axis.ticks=element_line(colour="black"),
# 								 plot.margin=unit(c(1,1,1,1),"line"), 
# 								 axis.text = element_text(size = 12), 
# 								 legend.text = element_text(size = 12), 
# 								 legend.title = element_text(size = 12),
# 								 text = element_text(size = 12))
# 	
# 	##### Set colors
# 	pal <- pal_locuszoom()(4)
# 	
# 	g <- ggbiplot(data.pca, obs.scale = 1, var.scale = 1, 
# 								groups = data.species$name, ellipse = TRUE, circle = TRUE) +
# 		scale_color_manual(name = '', values = pal) +
# 		theme(legend.direction = 'vertical', legend.position = 'bottom', 
# 					legend.text = element_text(size = 12, face = "italic")) +
# 		theme
# 	plot(g)
# 	
# } 

# Script Sevenn -----------------------------------------------------------


# ## Convert the shared model to spatial points
# bin_points <- rasterToPoints(get(paste0(k,"_bin")), spatial = TRUE)
# 
# ## Convert spatial points to a dataframe
# bin_df  <- data.frame(bin_points)
# 
# ## Call world map (can restrict by region, refer to plotting scheme in 06_ENM_Evaluation.R)
# regmap <- map_data("world")
# 
# binarymaps <- ggplot() +
# 	geom_polygon(data = regmap, aes(x = long, y = lat, group = group),color = "black", fill = "snow")+
# 	geom_raster(data = bin_df , aes(x = x, y = y, fill = layer, alpha = 1)) + 
# 	scale_fill_distiller(palette = "BuPu", direction = 1, name = "Minimum \n Training \n Prescence") +
# 	scale_alpha(guide = 'none')+
# 	ggtitle(paste0("Shared Space Projection of ",k," Minimum Training Prescence's Binary Projection"))+ ## think of better name idk right now
# 	coord_quickmap()
# 
# plot(binarymaps)
## Save the globally/regionally projected model (change name per needs)
# ggsave(paste0(k,"_Binary_Projection_of_Suitability_in_Shared_Space.png"),	plot = binarymaps ,	path = "rst/maps/shared_space/",	height = 10,	width = 20)


# Script Eight ------------------------------------------------------------

# 
# mpa.maps <- ggplot() +
# 	geom_polygon(data = region, aes(x = long, y = lat, group = group),color = "black", fill = "snow")+
# 	geom_raster(data = bin.df , aes(x = x, y = y, fill = layer, alpha = 1)) + 
# 	paletteer::scale_fill_paletteer_c("scico::buda",direction = -1, name = "Prescence \n Threshold") +
# 	# scale_fill_distiller(palette = "RdYlGn", direction = -1, name = "Minimum \n Training \n Prescence") +
# 	# scale_fill_viridis(option = "A",direction = 1, name = "Prescence \n Threshold") + ## Use this color when plotting suitability scores or make the fill of 'regmap' darker
# 	scale_alpha(guide = 'none')+
# 	ggtitle(paste0("Binary Projection of the Optimal Model of Cannabis Sativa Minimal Predicted Area in NY and the Midwest"))+ 
# 	# ggtitle(paste0("Binary Projection of the Optimal Model of ",k," Minimal Predicted Area"))+ 
# 	coord_quickmap()

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
source("bin/scripts/functions/ThresholdModel.R")
dir.create("rst/samples/future")
dir.create(paste0("rst/samples/future/",folder))

for (k in regions) {
	Gp.curr <- rast(paste0("rst/rasters/shared_space/",k,"_Shared_Space_Projection.grd"))
	Gp.proj <- rast(paste0("rst/rasters/future/",folder,"/shared_space/",k,"_Climate_Projection.grd"))
	
	occs.df <- read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	occs.ll <- occs.df[, c("LONG", "LAT")]
	
	Gp.curr.binary <- ThresholdModel(usr.raster = Gp.curr,usr.occs = occs.ll,method = "MPT", output.type = "binary")
	Gp.proj.binary <- ThresholdModel(usr.raster = Gp.proj,usr.occs = occs.ll,method = "MPT", output.type = "binary")
	
	range <- BIOMOD_RangeSize(Gp.curr.binary,Gp.proj.binary)
	
	range.df <- range$Compt.By.Models %>% as.data.frame()
	write.csv(range.df,paste0("rst/samples/future/",folder,"/", k, "_range_change.csv"),row.names = FALSE)
	
	plots <- bm_PlotRangeSize(bm.range = range,do.count = TRUE,do.perc = TRUE,do.maps = TRUE,do.plot = TRUE,row.names = c("layer"))
	
	ggsave(paste0(k,"_percentage_range_change.png"),	plot = plots$plot.perc ,	path = paste0("rst/maps/future/",folder,"/shared_space/"),	height = 10,	width = 20)
	
	rangerast <- raster(plots$tab.maps)
	
	## Convert the shared model to spatial points
	range_points <- rasterToPoints(rangerast, spatial = TRUE)
	
	## Convert spatial points to a dataframe
	range_df  <- data.frame(range_points) %>%
		mutate(layer = factor(layer))
	
	## Get Extent of raster to constrain map to region
	exts <- extent(rangerast)
	
	## Call world map (can restrict by region, refer to plotting scheme in 06_ENM_Evaluation.R)
	region <- map_data("world",xlim = c(exts@xmin,exts@xmax),ylim = c(exts@ymin,exts@ymax))
	
	## Plot your comparison maps
	comptest <- ggplot() +
		geom_polygon(data = region, aes(x = long, y = lat, group = group),color = "snow", fill = "grey28")+
		# geom_polygon(data = regmap, aes(x = long, y = lat, group = group),color = "grey18", fill = "grey30")+
		geom_raster(data = range_df, aes(x = x, y = y, fill = layer, alpha = 1)) + ## figure out fill
		scale_alpha(guide = 'none')+
		# geom_point(data=spdf, aes(x=LONG, y=LAT, color = SPECIES),shape=4,stroke = 0.4,size = 0.5,alpha = 1, show.legend = TRUE) + #color ="#FFC0CB" ,#355A20
		# scale_color_manual(values = c('C.sativa' = 'green')) +
		scale_fill_manual(values = viridis::viridis(4), breaks = -2:1,
											labels = c("loss", "remain occupied", "remain unoccupied", "gain"), name = "Habitat Suitability") +
		ggtitle(paste0("Habitat Suitability Comparison - at a Minimum Presence threshold - of ",k," C. sativa between a Current Model and Climatic Model ", models, " during ", time_period," and SSP ",ssps))+ 
		coord_sf(crs = crs(Gp.curr.binary))
	ggsave(paste0(k,"_MPT_Range_Comparison.png"),	plot = comptest ,path = paste0("rst/maps/future/",folder,"/shared_space/"),	height = 10,	width = 20)
	
}

Gp.one <- raster(paste0("rst/rasters/future/",folder,"/shared_space/Oceania_Global_Climate_Projection.grd"))
Gp.two <- raster(paste0("rst/rasters/future/",folder,"/shared_space/Africa_Global_Climate_Projection.grd"))

one.df <- read.csv()
one.occs <- one.df[, c("LONG", "LAT")]
Gp.one.binary <- ThresholdModel(usr.raster = Gp.one,usr.occs = one.occs,method = "MPT", output.type = "binary")

two.df<- read.csv()
two.occs <- two.df[, c("LONG", "LAT")]
Gp.two.binary <- ThresholdModel(usr.raster = Gp.two,usr.occs = two.occs,method = "MPT", output.type = "binary")

range <- BIOMOD_RangeSize(Gp.one.binary,Gp.two.binary)

bm_PlotRangeSize(range,do.count = TRUE,do.perc = TRUE,do.maps = TRUE,do.plot = TRUE,row.names = c("layer"))

rangerast <- raster(range$Diff.By.Pixel)
## Convert the shared model to spatial points
range_points <- rasterToPoints(rangerast, spatial = TRUE)

## Convert spatial points to a dataframe
range_df  <- data.frame(range_points) %>%
	mutate(layer = factor(layer))

## Call world map 
region <- map_data("world")

## Plot your comparison maps
comptest <- ggplot() +
	geom_polygon(data = region, aes(x = long, y = lat, group = group),color = "snow", fill = "grey28")+
	# geom_polygon(data = regmap, aes(x = long, y = lat, group = group),color = "grey18", fill = "grey30")+
	geom_raster(data = range_df, aes(x = x, y = y, fill = layer, alpha = 1)) + ## figure out fill
	scale_alpha(guide = 'none')+
	# geom_point(data=spdf, aes(x=LONG, y=LAT, color = SPECIES),shape=4,stroke = 0.4,size = 0.5,alpha = 1, show.legend = TRUE) + #color ="#FFC0CB" ,#355A20
	# scale_color_manual(values = c('C.sativa' = 'green')) +
	scale_fill_manual(values = viridis::viridis(4), breaks = -2:1,
										labels = c("loss", "remain occupied", "remain unoccupied", "gain"), name = "Habitat Suitability") +
	ggtitle(paste0("Projected Global Habitat Suitability Comparison - at a ",threshold.type," threshold - between Populations of C. sativa: ",name.Gp.one, " and ", name.Gp.two))+ 
	coord_sf(crs = crs(Gp.one.binary))
ggsave(".png",	plot = comptest ,path = paste0("rst/maps/future/",folder,"shared_space/"),	height = 10,	width = 20)




