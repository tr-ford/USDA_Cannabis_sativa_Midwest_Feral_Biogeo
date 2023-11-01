## map_making.R
## Script for Making Nice Presentation-Ready Maps
## Tori Ford
## Written on: 28/03/2023

library(leaflet)
library(geojsonio)
library(mapview)
library(ENMeval)
library(raster)
library(terra)
library(dplyr)
library(viridis)
library(ggplot2)
library(sf)


## Read in a geojson of the region you plan to map
xy <- geojsonio::geojson_read("raw/germplasm_sources/grab_agmarket/gz_2010_us_040_00_500k.json", what = "sp")

## Subset out the region you have your raster extent to
nyc <- xy[xy$STATE == 36, ]

## Read in occurrence data
occs <- read.csv("raw/germplasm_sources/grab_agmarket/agmarkets_ny_data_forZach&Tori.csv")

## Create a leaflet of your region
nyc_leaf <- leaflet(nyc) %>% addTiles()

## Create a custom icon (not necessary)
canna = makeIcon(
	iconUrl = "https://icons.iconarchive.com/icons/pictogrammers/material/128/cannabis-icon.png",
	iconWidth = 20,
	iconHeight = 20
)


## Read in region specific, selection method specific optimal model maxent results
results <- read.table("rst/samples/NY-Trial/AICc_Model_Results.txt", sep = "\t") %>% as.data.frame()

## Read in region specific Ecological Niche Model
load("rst/samples/NY-Trial/ENMeval.RData")


## Use optimal model results to select optimal model from ENM object, returns raster layer
nyraster <- eval.predictions(en_model)[[results$tune.args]]

## Project the suitability raster ontop of the leaflet map, add icons for occurrence points
nyc_leafs <- nyc_leaf %>% addMarkers(data = occs,~Longitude, ~Latitude, icon = canna)%>% 
	addRasterImage(nyraster, colors = viridis_pal(option = "G")(4), opacity = 0.5) %>%
	addPolygons(data = nyc, # borders of all counties
							color = "blue", 
							fill = NA, 
							weight = 1.5)

## Save a png of the map, this may be low quality
mapshot(nyc_leafs,file = "rst/maps/ENM_Grab_AgMarkets_Plot.png")

## Save a html of the map, this is a very high quality, interactive map
mapshot(nyc_leafs, url = "rst/maps/ENM_Grab_AgMarkets_Plot.html")


#https://r-spatial.github.io/mapview/reference/mapView.html
## USe for possible raster method
## Use the dataframe containing all species information to get your region names, in this case, Continents
alldf <- read.csv("rst/samples/prelim_accessions.csv")
regions <- unique(alldf$REGION) ## Name this variable appropriately to your dataset.

for (k in regions) {
	xy <- geojsonio::geojson_read("raw/germplasm_sources/grab_agmarket/gz_2010_us_040_00_500k.json", what = "sp")
	xy_leaf <- leaflet(xy) %>% addTiles()
	
	canna = makeIcon(
		iconUrl = "https://icons.iconarchive.com/icons/pictogrammers/material/128/cannabis-icon.png",
		iconWidth = 10,
		iconHeight = 10
	)
	
	# share_rast <- setMinMax(rast(paste0("rst/rasters/shared_space/",k, "_Shared_Space_Projection.grd")))
	# 
	# rasters <- raster(share_rast)
	rasters <- raster(paste0("rst/rasters/shared_space/",k, "_Shared_Space_Projection.grd"))
	
	occs <-	read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))
	
	pal <- viridis_pal(option = "C")(4)
	
	pal2 <- leaflet::colorBin(palette = viridis_pal(option = "C")(8),
														bins = 8,
														domain = getValues(rasters),
														na.color = NA)
	
	leafs <- xy_leaf %>% addMarkers(data = occs,~LONG, ~LAT, icon = canna)%>% 
		addRasterImage(rasters, colors = pal, opacity = 0.5) %>%
		addPolygons(data = xy, # borders of all counties
								color = "grey", 
								fill = NA, 
								weight = 1.5) %>% addScaleBar() %>%
		addLegend(pal = pal2, values = getValues(rasters), title = "Habitat Suitability",labFormat = labelFormat())
	mapshot(leafs, url = paste0("rst/maps/shared_space/",k,"_ENM_NA_Projection.html"))
	
}




# NEW YORK ----------------------------------------------------------------


## Read in region specific, selection method specific optimal model maxent results
results <- read.table("rst/samples/NY-Trial/AICc_Model_Results.txt", sep = "\t") %>% as.data.frame()

## Read in region specific Ecological Niche Model
load("rst/samples/NY-Trial/ENMeval.RData")


## Use optimal model results to select optimal model from ENM object, returns raster layer
nyraster <- eval.predictions(en_model)[[results$tune.args]]

terras <- rast(nyraster)%>% setMinMax()

nyraster <- raster(terras)

nyc_shp <- vect("raw/shapefiles/new_york.shp")

terras_c <- terra::crop(terras, ext(nyc_shp))
terras_m <- mask(terras_c, nyc_shp)

nyraster <- raster(terras_m)

nyraster_proj <- projectRaster(nyraster, crs = usmap::usmap_crs()@projargs)

## Convert the optimal model to spatial points
ny_points <- rasterToPoints(nyraster_proj, spatial = TRUE)

## Convert spatial points to a dataframe
ny_df  <- data.frame(ny_points)

ny_df$layer <- ny_df$fc.LQH_rm.5

library(usmap)
library(ggplot2)
library(svglite)
library(ggimage)

set.seed(230)

ny_title <- expression(paste("Predicted Habitat Suitability of ", italic("Cannabis sativa"), " in New York State"))

nymaps <- plot_usmap("counties",include = c("NY"), labels = TRUE, label_color = "blue",
					 fill = "grey15", alpha = 0.25, linewidth = 0.75) +
	geom_raster(data = ny_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
	scale_fill_viridis(option = "H",direction = -1, name = "Habitat Suitability",labels = scales::percent,
										 position = "top")+
	scale_alpha(guide = 'none') +
	labs(title = ny_title,
			 subtitle = "Source: Ag & Markets 2018 through 2022",
			 size = "Magnitude") +
	# geom_point(data=spdf, aes(x=LONG, y=LAT, color = SPECIES),shape=8,stroke = 0.4,size = 2,alpha = 1, show.legend = TRUE) + #color ="#FFC0CB" ,#355A20
	# scale_color_manual(values = c('#355A20')) +
	theme(legend.position = "top",legend.justification='left',
				legend.title = element_text(size=14), legend.key.height = unit(1, 'cm'),
				legend.key.size = unit(2, 'cm'),plot.title=element_text(size=20),
				plot.subtitle = element_text(size = 16),
				plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())
	
ggsave(file="NY_Suitability_Map.svg", plot=nymaps,path = "rst/maps/NY-Trial/", width=24, height=16)



# FERALS ------------------------------------------------------------------


## Read in region specific, selection method specific optimal model maxent results
results <- read.table("rst/samples/Midwest-Feral/AICc_Model_Results.txt", sep = "\t") %>% as.data.frame()

## Read in region specific Ecological Niche Model
load("rst/samples/Midwest-Feral/ENMeval.RData")


## Use optimal model results to select optimal model from ENM object, returns raster layer
mwraster <- eval.predictions(en_model)[[results$tune.args]]

mwraster_proj <- projectRaster(mwraster, crs = usmap::usmap_crs()@projargs)

## Convert the optimal model to spatial points
mw_points <- rasterToPoints(mwraster_proj, spatial = TRUE)

## Convert spatial points to a dataframe
mw_df  <- data.frame(mw_points)

mw_df$layer <- mw_df$fc.Q_rm.3


library(usmap)
library(ggplot2)
library(svglite)
library(ggimage)

set.seed(230)

mw_title <- expression(paste("Predicted Habitat Suitability of ", italic("Cannabis sativa"), " in the Midwestern United States"))

mwmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "blue",
										 fill = "grey15", alpha = 0.25, linewidth = 0.75) +
	geom_raster(data = mw_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
	scale_fill_viridis(option = "H",direction = -1, name = "Habitat Suitability",labels = scales::percent,
										 position = "top")+
	scale_alpha(guide = 'none') +
	labs(title = mw_title,
			 subtitle = "Source: Ademola Aina 2022 Feral Hemp Collections",
			 size = "Magnitude") +
	theme(legend.position = "top",legend.justification='left',
				legend.title = element_text(size=14), legend.key.height = unit(1, 'cm'),
				legend.key.size = unit(2, 'cm'),plot.title=element_text(size=20),
				plot.subtitle = element_text(size = 16),
				plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())

ggsave(file="MW_Suitability_Map.svg", plot=mwmaps,path = "rst/maps/Midwest-Feral/", width=24, height=16)


## Create maps for projection into the midwest proper


## Read in the raster projected to the midwest
mw_proj <- raster("rst/rasters/shared_space/Midwest-Feral_Midwest_Projection.grd")

## Project that raster into the same crs as the map
mwraster_proj <- projectRaster(mw_proj, crs = usmap::usmap_crs()@projargs)

## Convert the optimal model to spatial points
mw_points <- rasterToPoints(mwraster_proj, spatial = FALSE)

## Convert spatial points to a dataframe
mw_df  <- data.frame(mw_points)


library(usmap)
library(ggplot2)
library(svglite)
library(ggimage)

set.seed(230)

mw_title <- expression(paste("Predicted Habitat Suitability of ", italic("Cannabis sativa"), " in the Midwestern United States"))

mwmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "orchid3",
										 fill = "grey15", alpha = 0.25, linewidth = 0.75) +
	geom_raster(data = mw_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
	scale_fill_viridis(option = "H",direction = -1, name = "Habitat Suitability",labels = scales::percent,
										 position = "top")+
	scale_alpha(guide = 'none') +
	labs(title = mw_title,
			 subtitle = "Source: Ademola Aina 2022 Feral Hemp Collections",
			 size = "Magnitude") +
	theme(legend.position = "top",legend.justification='left',
				legend.title = element_text(size=14), legend.key.height = unit(1, 'cm'),
				legend.key.size = unit(2, 'cm'),plot.title=element_text(size=20),
				plot.subtitle = element_text(size = 16),
				plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())

ggsave(file="MW_Total_Suitability_Map.svg", plot=mwmaps,path = "rst/maps/Midwest-Feral/", width=24, height=16)


## Create a map for the MOP Analyses


## Read in the raster projected to the midwest
mw_proj <- raster("rst/rasters/shared_space/Midwest_mobility_oriented_parity_analysis.grd")

## Project that raster into the same crs as the map
mwraster_proj <- projectRaster(mw_proj, crs = usmap::usmap_crs()@projargs)

## Convert the optimal model to spatial points
mw_points <- rasterToPoints(mwraster_proj, spatial = TRUE)

## Convert spatial points to a dataframe
mw_df  <- data.frame(mw_points)

## Correct 'layer' labels
mw_df$layer <- mw_df$hwsb_cation_exchange_capacity_of_the_clay_fraction_in_the_subsoil

library(usmap)
library(ggplot2)
library(svglite)
library(ggimage)

set.seed(230)

mw_title <- expression(paste("Mobility-Oriented Parity (MOP) Analysis of Midwest Feral ", italic("Cannabis sativa"), " to the Midwestern United States"))

mwmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "slateblue4",
										 fill = "grey15", alpha = 0.25, linewidth = 0.75) +
	geom_raster(data = mw_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
	scale_fill_viridis(option = "H",direction = -1, name = "Extrapolation",labels = c("high","low"),
										 position = "top", breaks = c(0,1), limits = c(0,1))+
	scale_alpha(guide = 'none') +
	labs(title = mw_title,
			 # subtitle = "Source: Ademola Aina 2022 Feral Hemp Collections",
			 size = "Magnitude") +
	theme(legend.position = "top",legend.justification='left',
				legend.title = element_text(size=14), legend.key.height = unit(1, 'cm'),
				legend.key.size = unit(2, 'cm'),plot.title=element_text(size=20),
				plot.subtitle = element_text(size = 16),legend.text=element_text(size=10),
				plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())

ggsave(file="MW_Mobility-Oriented_Parity_Analysis.svg", plot=mwmaps,path = "rst/maps/shared_space/", width=24, height=16)



# AGGREGATE ---------------------------------------------------------------

## Read in region specific, selection method specific optimal model maxent results
results <- read.table("rst/samples/aggregate/AICc_Model_Results - Unique.txt", sep = "\t") %>% as.data.frame()

## Read in region specific Ecological Niche Model
load("rst/samples/aggregate/ENMeval.RData")


## Use optimal model results to select optimal model from ENM object, returns raster layer
aggraster <- eval.predictions(en_model)[[results$tune.args]]

aggraster_proj <- projectRaster(aggraster, crs = usmap::usmap_crs()@projargs)

## Convert the optimal model to spatial points
agg_points <- rasterToPoints(aggraster_proj, spatial = TRUE)

## Convert spatial points to a dataframe
agg_df  <- data.frame(agg_points)

agg_df$layer <- agg_df$fc.LH_rm.2


library(usmap)
library(ggplot2)
library(svglite)
library(ggimage)

set.seed(230)

agg_title <- expression(paste("Predicted Habitat Suitability of ", italic("Cannabis sativa"), " in the Midwestern United States and New York State"))

aggmaps <- plot_usmap("states",include = c(.midwest_region,.northeast_region), labels = TRUE, label_color = "blue",
										 fill = "grey15", alpha = 0.25, linewidth = 0.75) +
	geom_raster(data = agg_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
	scale_fill_viridis(option = "H",direction = -1, name = "Habitat Suitability",labels = scales::percent,
										 position = "top")+
	scale_alpha(guide = 'none') +
	labs(title = agg_title,
			 subtitle = "Midwest Occurrences Source: Ademola Aina 2022 Feral Hemp Collections \n New York Occurrences Source: Ag & Markets 2018 through 2022",
			 size = "Magnitude") +
	theme(legend.position = "top",legend.justification='left',
				legend.title = element_text(size=14), legend.key.height = unit(1, 'cm'),
				legend.key.size = unit(2, 'cm'),plot.title=element_text(size=20),
				plot.subtitle = element_text(size = 16),legend.text=element_text(size=10),
				plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())

ggsave(file="AGG_Suitability_Map.svg", plot=aggmaps,path = "rst/maps/aggregate/", width=22, height=12)


# MIDWEST PROPER ---------------------------------------------------------------

## Use optimal model results to select optimal model from ENM object, returns raster layer
mwtraster <- raster("rst/rasters/shared_space/Midwest_Shared_Space_Projection.grd")

mwtraster_proj <- projectRaster(mwtraster, crs = usmap::usmap_crs()@projargs)

## Convert the optimal model to spatial points
mwt_points <- rasterToPoints(mwtraster_proj, spatial = TRUE)

## Convert spatial points to a dataframe
mwt_df  <- data.frame(mwt_points)


library(usmap)
library(ggplot2)
library(svglite)
library(ggimage)

set.seed(230)

mwt_title <- expression(paste("Predicted Habitat Suitability of ", italic("Cannabis sativa"), " in the Midwestern United States"))

mwtmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "white",
											fill = "grey15", alpha = 0.25, linewidth = 0.75, cex = 10) +
	geom_raster(data = mwt_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
	scale_fill_viridis(option = "H",direction = -1, name = "Habitat Suitability",labels = scales::percent,
										 position = "top")+
	scale_alpha(guide = 'none') +
	labs(title = mwt_title,
			 # subtitle = "Occurrences Sourced from 13 Repositories, 10 Digitized, 3 University-Level Project Contributions",
			 size = "Magnitude") +
	theme(legend.position = "top",legend.justification='left',
				legend.title = element_text(size=16), legend.key.height = unit(1.2, 'cm'), legend.key.width = unit(4, 'cm'),
				legend.key.size = unit(2, 'cm'),plot.title=element_text(size=26),
				plot.subtitle = element_text(size = 14),legend.text=element_text(size=14),
				plot.background = element_rect(fill = 'white', colour = NA),legend.background=element_blank())

ggsave(file="MWT_Suitability_Map.svg", plot=mwtmaps,path = "rst/maps/shared_space/", width=18, height=12)



# MIDWEST FERALS PROPER ---------------------------------------------------------------

## Use optimal model results to select optimal model from ENM object, returns raster layer
mwfraster <- raster("rst/rasters/shared_space/Midwest-Feral_Shared_Space_Projection.grd")

mwfraster_proj <- projectRaster(mwfraster, crs = usmap::usmap_crs()@projargs)

## Convert the optimal model to spatial points
mwf_points <- rasterToPoints(mwfraster_proj, spatial = TRUE)

## Convert spatial points to a dataframe
mwf_df  <- data.frame(mwf_points)


library(usmap)
library(ggplot2)
library(svglite)
library(ggimage)

set.seed(230)

mwf_title <- expression(paste("Predicted Habitat Suitability of Midwest Feral ", italic("Cannabis sativa"), " (Collected by Ademola Aina) in the Midwestern United States"))

mwfmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "white",
											fill = "grey15", alpha = 0.25, linewidth = 0.75, cex = 10) +
	geom_raster(data = mwf_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
	scale_fill_viridis(option = "H",direction = -1, name = "Habitat Suitability",labels = scales::percent,
										 position = "top")+
	scale_alpha(guide = 'none') +
	labs(title = mwf_title,
			 # subtitle = "Occurrences Sourced from 13 Repositories, 10 Digitized, 3 University-Level Project Contributions",
			 size = "Magnitude") +
	theme(legend.position = "top",legend.justification='left',
				legend.title = element_text(size=12), legend.key.height = unit(1, 'cm'),
				legend.key.size = unit(2, 'cm'),plot.title=element_text(size=18),
				plot.subtitle = element_text(size = 14),legend.text=element_text(size=10),
				plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())

ggsave(file="MWF_Suitability_Map.svg", plot=mwfmaps,path = "rst/maps/shared_space/", width=22, height=12)


# FUTURE ---------------------------------------------------------------


## Use optimal model results to select optimal model from ENM object, returns raster layer
climraster <- raster("rst/rasters/future/GISS-E2-1-H_2021-2040_126/shared_space/Midwest_Climate_Projection.grd")

climraster_proj <- projectRaster(climraster, crs = usmap::usmap_crs()@projargs)

## Convert the optimal model to spatial points
clim_points <- rasterToPoints(climraster_proj, spatial = TRUE)

## Convert spatial points to a dataframe
clim_df  <- data.frame(clim_points)


library(usmap)
library(ggplot2)
library(svglite)
library(ggimage)

set.seed(230)

clim_title <- expression(paste("Predicted Habitat Suitability of ", italic("Cannabis sativa"), " in the Midwestern United States from 2021 to 2040, "))

climmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "white",
											fill = "grey15",col = "black", alpha = 0.25, linewidth = 0.75, cex = 10) +
	geom_raster(data = clim_df , aes(x = x, y = y, fill = layer,alpha = 2)) +
	scale_fill_viridis(option = "H",direction = -1, name = "Habitat Suitability",labels = scales::percent,
										 position = "top")+
	scale_alpha(guide = 'none') +
	labs(title = clim_title,
			 subtitle = "using the GISS-E2-1-H Climate Model in Shared Socioeconomic Pathway 585",
			 size = "Magnitude") +
	theme(legend.position = "top",legend.justification='left',
				legend.title = element_text(size=12), legend.key.height = unit(1, 'cm'),
				legend.key.size = unit(2, 'cm'),plot.title=element_text(size=18),
				plot.subtitle = element_text(size = 14),legend.text=element_text(size=10),
				plot.background = element_rect(fill = 'grey', colour = NA),legend.background=element_blank())

ggsave(file="MW_Suitability_Map_in_GISS-E2-1-H_during_2061-2080_ssp585.svg", plot=climmaps,path = "rst/maps/future/", width=22, height=12)


# Occurrence Plot ---------------------------------------------------------

library(usmap)
library(ggplot2)
library(svglite)
library(ggimage)
library(sf)
library(ggrepel)


data.occs <- read.csv(paste0("rst/samples/", k,"/maxent_ready.csv"))

occs <- usmap_transform(data.occs, input_names = c("LONG","LAT"), output_names = c("long","lat"))

set.seed(230)

occs_title <- expression(paste("Occurrence Records of ", italic("Cannabis sativa"), " in the Midwestern United States"))

occsmaps <- plot_usmap("states",include = c(.midwest_region), labels = TRUE, label_color = "black",
											fill = "grey15", alpha = 0.25, linewidth = 0.75, cex = 10) +
	geom_point(data = occs,aes(x = long, y = lat , col =SOURCE)) +
	scale_colour_manual(values = viridis(8, option = "H"), name = "Occurrence Source") +
	scale_alpha(guide = 'none') +
	labs(title = occs_title,
			 # subtitle = "Occurrences Sourced from 13 Repositories, 10 Digitized, 3 University-Level Project Contributions",
			 size = "Magnitude") +
	theme(legend.position = "top",legend.justification='left',
				legend.title = element_text(size=16), legend.key.height = unit(1.2, 'cm'), legend.key.width = unit(4, 'cm'),
				legend.key.size = unit(4, 'cm'),plot.title=element_text(size=26),
				plot.subtitle = element_text(size = 14),legend.text=element_text(size=14),
				plot.background = element_rect(fill = 'white', colour = NA),legend.background=element_blank()) +
	guides(colour = guide_legend(override.aes = list(size=8)))

ggsave(file="Occurrence_Map.svg", plot=occsmaps,path = "rst/maps/shared_space/", width=18, height=12)
