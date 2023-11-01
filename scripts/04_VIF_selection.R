## 04_VIF_selection.R
## Script Four
## Tori Ford
### 16-02-23
#### Objective: Use VIF to select layers that generate the least amount of multicollinearity in the model per Region.

library(raster) # 3.6.13
library(gtools) # 3.9.4
library(usdm) # 1.1.18


## Use the dataframe containing all species information to get your region names
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION) ## Name this variable appropriately to your dataset.

## Use the dataframe containing all species information to get your region names, for the aggregate model
aggdf <- read.csv("rst/samples/aggregate/maxent_ready.csv")
regions <- unique(aggdf$TOTAL) ## Name this variable appropriately to your dataset.


## Create Raster Directory
## Only needed once per project
dir.create(paste0("rst/rasters"))


# Variable Selection using VIF --------------------------------------------


for (k in regions) {
	
	## Pull clipped raster per continent
	list <- list.files(paste0("raw/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE)
	
	## Sort them into alphabetical order
	list <- mixedsort(sort(list))
	
	## Stack the "region" clipped rasters
	stack <- raster::stack(list)
	
	## All of the following steps were derived from the USDM Vignette: https://cran.r-project.org/web/packages/usdm/usdm.pdf, see 'vif' and 'exclude'.
	#calculate VIFs using a threshold of 10
	set.seed(521) # Testing reproducibility with set seed
	vifstack <- vifstep(stack, th=10) #USDM Vignette: "A VIF greater than 10 is a signal that the model has a collinearity problem".
	print("step complete")
	
	## Get correlation matrix and save
	cor_table <- as.data.frame(vifstack@corMatrix)
	write.csv(cor_table, paste0("rst/samples/",k,"/VIF_Correlation_Matrix.csv"))
	
	## Get result table with VIF scores for kept variables
	vif_results <- as.data.frame(vifstack@results)
	write.csv(vif_results, paste0("rst/samples/",k,"/VIF_Results.csv"))
	
	## Use the 'exclude' function to exclude rasters past threshold and stack the remainder. 
	in_vif <- exclude(stack,vifstack)
	print("layers excluded")
	
	## Also exclude nodata values
	NaN_DF <- vifstack@results[!is.na(vifstack@results$VIF),]
	in_vif <- raster::subset(in_vif,NaN_DF$Variables)
	
	## Only needed once per project
	## Create Continent Directory in Raster Directory
	dir.create(paste0("rst/rasters/",k))
	
	# Complete a file copy for each of the rasters not excluded, 'in_vif'
	for(i in 1:length(names(in_vif))){
		name <- names(in_vif)[i]
		print(name)

		from <- paste0("raw/rasters/",k,"/", name, ".asc") ## Double check how your ascii's from previous script were saved
		to <- paste0("rst/rasters/",k,"/", name, ".asc")

		file.copy(from, to,
							overwrite = TRUE, recursive = FALSE,
							copy.mode = TRUE)
	}
}



# Run after Models have been generated and evaluated ----------------------

## NEXT 3 Sections are for post-modeling, they are here because they will determine if you need to rerun from this step. :)

## Aggregate Unique Variables (ONLY RUN THIS IF YOU HAVE ALREADY MODELED THE POPULATIONS) ----------------------------------------------

library(ENMeval)
library(reshape2)

## read in All regions to that were included in the aggregate evironmental predictors
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION)

keep.names <- c()

for (k in regions) {
	
	## Read in region specific, selection method specific optimal model maxent results
	results <- read.table(file = paste0("rst/samples/", k, "/AICc_Model_Results.txt"), sep = "\t") %>% as.data.frame()
	
	opt <- results$tune.args
	
	## Read in region specific Ecological Niche Model
	load(file = paste0("rst/samples/", k, "/ENMeval.RData"))
	
	## Load Variable Importance for All Previous Models Generated
	imp <- eval.variable.importance(en_model)
	
	## Select the names of the most important variables for the optimal model, return the
	var.imp <- imp[grep(opt, names(imp))] %>% as.data.frame()
	
	## Filter out any predictors contributing nothing to the model
	keep.var <- var.imp %>%
		filter(var.imp[c(2)] != 0)
	
	## Make a list of the names that contribute variable importance to the optimal model
	keep.names[[k]] <- keep.var[c(1)] %>% as.data.frame()
	# keep.names[[k]] <- keep.var[c(1)] %>% as.matrix()
}

## Convert list into dataframe containing the important environemental predictors for each population
keep.df <- unlist(keep.names) %>% as.data.frame()

## Convert to character vector of unique names
keep.names <- unique(keep.df[, c(1)])

## Create a folder for the unique variables within the existing 'aggregate' folder
dir.create("rst/rasters/aggregate/unique")

## Copy the rasters to a "unique" folder
for(i in 1:length(keep.names)){
	name <- keep.names[i]
	print(name)
	
	from <- paste0("rst/rasters/aggregate/", name, ".asc") ## Double check how your ascii's from previous script were saved
	to <- paste0("rst/rasters/aggregate/unique/", name, ".asc")
	
	file.copy(from, to,
						overwrite = TRUE, recursive = FALSE,
						copy.mode = TRUE)
}



## PCA (On Variables Important to the Optimal Model) -----------------------


library(ENMeval)
library(reshape2)
library(vegan)
library(dplyr)
library(tidyr)
library(tibble)

## read in All regions to that were included in the aggregate evironmental predictors
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION)

for (k in regions) {
	
	## Read in region specific, selection method specific optimal model maxent results
	results <- read.table(file = paste0("rst/samples/", k, "/AICc_Model_Results.txt"), sep = "\t") %>% as.data.frame()
	
	opt <- results$tune.args
	
	## Read in region specific Ecological Niche Model
	load(file = paste0("rst/samples/", k, "/ENMeval.RData"))
	
	## Load Variable Importance for All Previous Models Generated
	imp <- eval.variable.importance(en_model)
	
	## Select variables important to the optimal model
	var.imp <- imp[grep(opt, names(imp))] %>% as.data.frame()
	
	## Rename second column (inconsistent across models)
	var.imp <- dplyr::rename(var.imp, varimp = 2)
	var.imp <- dplyr::rename(var.imp, varname = 1)
	
	
	## Filter out 00s
	keep.var <- var.imp %>%
		filter(var.imp$varimp != 0)
	
	## Sort the variables in order of contribution
	var.order<- keep.var[order(keep.var$varimp, decreasing = T),]
	
	## Select the top 10 contributing variables
	var.order <- var.order %>%
		slice(1:10)
	write.csv(var.order, paste0("rst/samples/",k,"/Top_10_Variable_Contribution.csv"), row.names = FALSE)
	
	
	## Load in all vif rasters
	vif.envs <- stack(list.files(paste0("rst/rasters/",k,"/"), pattern = "*.asc", full.names = TRUE))
	
	## Subset out top 10 contributors
	var.envs <- raster::subset(vif.envs, var.order$varname)

	## Read in accessions for each region
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	occs <- spdf[, c("LONG", "LAT","SPECIES")]
	
	## Extract the environmental predictors under occurrences
	occs.extr <- raster::extract(var.envs,occs[1:2]) %>% as.data.frame()
	
	## Drop any Nan (or any other problems)
	occs.extr <-	occs.extr %>% drop_na()

	## Conduct a PCA analysis (Tutorial: https://www.davidzeleny.net/anadat-r/doku.php/en:pca_examples)
	PCA <- rda(occs.extr, scale = TRUE)  # the argument scale standardizes the variables

	## Generate standardized correlation of each variable to each axis (loadings)
	loadings <- scores (PCA, display = 'species', scaling = 0)
	
	## Generate a list of variable correlation in descending order from most correlated, save PC1/2
	PC1 <- sort (abs (loadings[,1]), decreasing = TRUE) %>% as.data.frame()
	write.table(PC1, file = paste0("rst/samples/", k, "/PC1_loadings.txt"), sep = "\t")
	PC2 <- sort (abs (loadings[,2]), decreasing = TRUE) %>% as.data.frame()
	write.table(PC2, file = paste0("rst/samples/", k, "/PC2_loadings.txt"), sep = "\t")
	
	## Simple plot of the PCA
	PCA.biplot <- biplot(PCA,
				 display = c("sites", "species"), 
				 type = c("text", "points"))	
	
	## Complex PCA Plotting (tutorial: https://rpubs.com/an-bui/vegan-cheat-sheet)
	PCAscores <- scores(PCA, display = "site") %>% 
		as.data.frame() %>% 
		rownames_to_column("site") 
	
	PCAvect <- scores(PCA, display = "species") %>% 
		as.data.frame()
	
	plot_PCA <- ggplot() +
		geom_point(data = PCAscores, aes(x = PC1, y = PC2)) +
		scale_color_manual(values = viridis::viridis(10)) +
		geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
		geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
		geom_segment(data = PCAvect, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
		geom_text(data = PCAvect, aes(x = PC1, y = PC2, label = rownames(PCAvect))) +
		# clean_background +
		labs(title = "Principal Components Analysis") #x = "PC1 (%)",y = "PC2 (%)",
	
	ggsave(paste0("PCA_Plot.png"),	plot = plot_PCA ,	path = paste0("rst/maps/", k, "/"),	height = 10,	width = 20)
	
}



## PCA (With all variables, What is correlated to the top 10?) -----------------------

library(ENMTools)
library(reshape2)
library(vegan)
library(dplyr)
library(tidyr)
library(tibble)

for (k in regions) {
	
	## Run a correlation plot of the variables
	totalenvs <- terra::rast(mixedsort(sort(list.files(paste0("raw/rasters/shared_space/"), pattern = "*.grd", full.names = TRUE))))
	
	## Set NAs to value
	totalenvs[is.na(totalenvs)] <- -999
	
	## Compute Pearson's correlation
	cor.plt <- raster.cor.plot(totalenvs, method = "pearson")
	
	pdf(file = paste0("rst/maps/Pearson_Correlation_Plot.pdf"), width = 36, height = 36)
	
	cor.plt
	
	dev.off()

	## Read in accessions for each region
	spdf <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	occs <- spdf[, c("LONG", "LAT","SPECIES")]
	
	## Extract the environmental predictors under occurrences
	occs.extr <- raster::extract(totalenvs,occs[1:2]) %>% as.data.frame()
	
	## Drop any Nan (or any other problems)
	occs.extr <-	occs.extr %>% tidyr::drop_na()
	
	## Conduct a PCA analysis (Tutorial: https://www.davidzeleny.net/anadat-r/doku.php/en:pca_examples)
	PCA <- rda(occs.extr, scale = TRUE)  # the argument scale standardizes the variables
	
	## Generate standardized correlation of each variable to each axis (loadings)
	loadings <- scores (PCA, display = 'species', scaling = 0)
	
	## Generate a list of variable correlation in descending order from most correlated, save PC1/2
	PC1 <- sort (abs (loadings[,1]), decreasing = TRUE) %>% as.data.frame()
	write.table(PC1, file = paste0("rst/samples/PC1_loadings.txt"), sep = "\t")
	PC2 <- sort (abs (loadings[,2]), decreasing = TRUE) %>% as.data.frame()
	write.table(PC2, file = paste0("rst/samples/PC2_loadings.txt"), sep = "\t")
	
	## Complex PCA Plotting (tutorial: https://rpubs.com/an-bui/vegan-cheat-sheet)
	PCAscores <- scores(PCA, display = "site") %>% 
		as.data.frame() %>% 
		rownames_to_column("site") 
	
	PCAvect <- scores(PCA, display = "species") %>% 
		as.data.frame()
	
	plot_PCA <- ggplot() +
		geom_point(data = PCAscores, aes(x = PC1, y = PC2)) +
		scale_color_manual(values = viridis::viridis(10)) +
		geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
		geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
		geom_segment(data = PCAvect, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
		geom_text(data = PCAvect, aes(x = PC1, y = PC2, label = rownames(PCAvect))) +
		# clean_background +
		labs(title = "Principal Components Analysis") #x = "PC1 (%)",y = "PC2 (%)",
	
	ggsave(paste0("PCA_Plot.png"),	plot = plot_PCA ,	path = paste0("rst/maps/"),	height = 16,	width = 24)
	
}
