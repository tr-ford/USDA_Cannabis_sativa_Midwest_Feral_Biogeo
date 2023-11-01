## 02_clean_data.R
## Script Two
## Tori Ford, Zachary Stansell
### 01-02-23
### Revised 07-02-23
#### Objective: Clean raw accession data to standardize format and spatially correct.

## Load Packages
library(dplyr) # 1.1.0
library(tidyverse) # 2.0.0
library(tidyr) # 1.3.0
library(spThin) # 0.2.0
library(maps) # 3.4.1
library(ggplot2) # 3.4.1
library(tidygeocoder) # 1.0.5
library(CoordinateCleaner) # 2.0.20
library(countrycode) # 1.4.0
library(stringr) # 1.5.0
library(rgdal) # 1.6.5
library(rgeos) # 0.6.2
library(leaflet) # 2.1.1
library(mapview) # 2.11.0
library(htmltools) # 0.5.4


### Think of some steps to make this easier
### May treat this as a large function that we can select per

## Call all listed csv, call the 6 working columns needed for maxent: year, PI, species name, country, Lat, Long. Save them higher in the directory


# Standardize Across Repositories -----------------------------------------

## MAKE SURE YOUR OCCURRENCES ARE IN CSV FORMAT 

## Set repository
rep <- "agmarket"

## Will eventually be functional as a loop, for the time being, use line by line
## Also may make an appended file for each repository
for(k in 1){
# ALA

## you can do this a little cleaner if you prefer 
#e.g., create fewer objects in workspace-- no need to make ala_accession and ala_accession2

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	ala_accessions <-
		read.csv("raw/germplasm_sources/ala/update_ala-cannabis.csv") %>%
		filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
		filter(decimalLongitude != 0.00 & decimalLatitude != 0.00) #219
		
	

	## Filter out Institutional records and outliers
	ala_accessions <- cc_inst(ala_accessions, 
												lon = "decimalLongitude", 
												lat = "decimalLatitude", 
												species = "scientificName") # 2 records removed
	ala_accessions <- cc_outl(ala_accessions, 
												lon = "decimalLongitude", 
												lat = "decimalLatitude", 
												species = "scientificName") # 19 records removed

	## Round the points to 2 digits, which should capture up to 1.1 km, 
	## this is roughly the resolution in km so it will be fine with thinning. 
	# a little cleaner
	
	ala_accessions <- 
		ala_accessions %>%
		mutate(
			decimalLatitude = round(decimalLatitude, digits = 3),
			decimalLongitude = round(decimalLongitude, digits = 3)
		)
	
	## Remove duplicates
	ala_accessions <- 
		ala_accessions %>%
		distinct( decimalLatitude, decimalLongitude, .keep_all = TRUE)  #142 # i got 139 here # 139 is correct
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	ala_accessions <- ala_accessions %>%
		reverse_geocode(
			lat = decimalLatitude,
			long = decimalLongitude,
			method = 'arcgis',
			full_results = TRUE
		)

	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")

	# i can't repeat this :(
	# i got this error..
	# Error in fortify(data) : object 'country' not found
		# tmf: fixed, the data it needed was the ala dataframe not country, syntax error on my part.
	# is there another package in your local library?
	# 
	world_map <- ggplot() +
		world +
		geom_point(
			ala_accessions,
			mapping = aes(x = decimalLongitude,
										y = decimalLatitude ,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)

########## STOPPED here. zjs


## Convert country code data to country names
ala_accessions$country <-
	countrycode(ala_accessions$CountryCode, 
							origin = 'iso3c', 
							destination = "country.name")

# ## Add locality information for one point, can skip this step and manually fix csv.
# ala_accessions2[157,"country"] <- "Papua New Guinea"

## Select colums to pull information
ala <- ala_accessions %>%
	dplyr::select(
		LAT = decimalLatitude,
		LONG = decimalLongitude,
		PI = recordID,
		COUNTRY = country,
		YEAR = year
	) %>%
	tibble::add_column(SOURCE = "ala", .before = "PI")


## Write out CSV of semi-clean data. 
write.csv(apply(ala,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# BIEN
	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	bien_accessions <-
		read.csv("raw/germplasm_sources/bien/update_bien-cannabis.csv") %>%
		filter(!is.na(longitude) & !is.na(latitude)) %>%
		filter(longitude != 0.00 & latitude != 0.00) #2 records
	
	
	## Correct the year format
	bien_accessions$year <- substr(bien_accessions$date_collected, 1, 4)
	
	
	## Filter out Institutional records and outliers
	bien_accessions <- cc_inst(bien_accessions, 
														 lon = "longitude", 
														 lat = "latitude", 
														 species = "scrubbed_species_binomial") # 0 records removed
	bien_accessions <- cc_outl(bien_accessions, 
														 lon = "longitude", 
														 lat = "latitude", 
														 species = "scrubbed_species_binomial") # 0 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	bien_accessions <- 
		bien_accessions %>%
		mutate(
			latitude = round(latitude, digits = 3),
			longitude = round(longitude, digits = 3)
		)
	
	
	## Remove duplicates
	bien_accessions <- 
		bien_accessions %>%
		distinct( latitude, longitude, .keep_all = TRUE)  #2
	
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	bien_accessions <- bien_accessions %>%
		reverse_geocode(
			lat = latitude,
			long = longitude,
			method = 'arcgis',
			full_results = TRUE
		) 
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			bien_accessions,
			mapping = aes(x = longitude,
										y = latitude ,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	bien_accessions$country <-
		countrycode(bien_accessions$CountryCode, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Select colums to pull information
	bien <- bien_accessions %>%
		dplyr::select(
			LAT = latitude,
			LONG = longitude,
			PI = recorded_by,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "bien", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(bien,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# Cornell Hemp

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	cornell_accessions <-
		read.csv("raw/germplasm_sources/cornellHemp/cornell.csv") %>%
		filter(!is.na(Longitude) & !is.na(Latitude)) %>%
		filter(Longitude != 0.00 & Latitude != 0.00) #56 records
	
	
	## Coerce name column (for inst and outl filter)
	cornell_accessions$species <- "Cannabis sativa"
	
	
	## Filter out Institutional records and outliers
	cornell_accessions <- cc_inst(cornell_accessions, 
															lon = "Longitude", 
															lat = "Latitude", 
															species = "species") # 0 records removed
	cornell_accessions <- cc_outl(cornell_accessions, 
															lon = "Longitude", 
															lat = "Latitude", 
															species = "species") # 0 records removed
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	cornell_accessions <- 
		cornell_accessions %>%
		mutate(
			Latitude = round(Latitude, digits = 3),
			Longitude = round(Longitude, digits = 3)
		)
	
	
	## Remove duplicates
	cornell_accessions <- 
		cornell_accessions %>%
		distinct( Latitude, Longitude, .keep_all = TRUE) #42
	
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	cornell_accessions <- cornell_accessions %>%
		reverse_geocode(
			lat = Latitude,
			long = Longitude,
			method = 'arcgis',
			full_results = TRUE
		) 
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			cornell_accessions,
			mapping = aes(x = Longitude,
										y = Latitude ,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	cornell_accessions$country <-
		countrycode(cornell_accessions$CountryCode, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Select colums to pull information
	cornell <- cornell_accessions %>%
		dplyr::select(
			LAT = Latitude,
			LONG = Longitude,
			PI = Accession,
			COUNTRY = country,
			YEAR = Year
		) %>%
		tibble::add_column(SOURCE = "cornell", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(cornell,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# CRIA

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	cria_accessions <-
		read.csv("raw/germplasm_sources/cria/update_cria-cannabis.csv") %>%
		filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
		filter(decimalLongitude != 0.00 & decimalLatitude != 0.00) #113 records
	
	
	## Coerce country name to be iso3c compliant
	cria_accessions$country <- str_replace(cria_accessions$country, "Brasil", "Brazil")
	
	
	## Filter out Institutional records and outliers
	cria_accessions <- cc_inst(cria_accessions, 
																 lon = "decimalLongitude", 
																 lat = "decimalLatitude", 
																 species = "genus") # 0 records removed
	cria_accessions <- cc_outl(cria_accessions, 
																 lon = "decimalLongitude", 
																 lat = "decimalLatitude", 
																 species = "genus") # 0 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	cria_accessions <- 
		cria_accessions %>%
		mutate(
			decimalLatitude = round(decimalLatitude, digits = 3),
			decimalLongitude = round(decimalLongitude, digits = 3)
		)
	
	
	## Remove duplicates
	cria_accessions <- 
		cria_accessions %>%
		distinct( decimalLatitude, decimalLongitude, .keep_all = TRUE) #48
	
	
	## Visualize using a map
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			cria_accessions,
			mapping = aes(x = decimalLongitude,
										y = decimalLatitude ,
										color = country),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Select colums to pull information
	cria <- cria_accessions %>%
		dplyr::select(
			LAT = decimalLatitude,
			LONG = decimalLongitude,
			PI = recordedBy,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "cria", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(cria,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# EURISCO

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	eurisco_accessions <-
		read.csv("raw/germplasm_sources/eurisco/update_eurisco-cannabis.csv") %>%
		filter(!is.na(DECLATITUDE) & !is.na(DECLONGITUDE)) %>%
		filter(DECLATITUDE != 0.00 & DECLONGITUDE != 0.00) #494 records
	
	
	## Filter out Institutional records and outliers
	eurisco_accessions <- cc_inst(eurisco_accessions, 
															lon = "DECLONGITUDE", 
															lat = "DECLATITUDE", 
															species = "SPECIES") # 11 records removed
	eurisco_accessions <- cc_outl(eurisco_accessions, 
															lon = "DECLONGITUDE", 
															lat = "DECLATITUDE", 
															species = "SPECIES") # 12 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	eurisco_accessions <- 
		eurisco_accessions %>%
		mutate(
			DECLATITUDE = round(DECLATITUDE, digits = 3),
			DECLONGITUDE = round(DECLONGITUDE, digits = 3)
		)
	
	
	## Remove duplicates
	eurisco_accessions <- 
		eurisco_accessions %>%
		distinct( DECLATITUDE, DECLONGITUDE, .keep_all = TRUE) #166
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			eurisco_accessions,
			mapping = aes(x = DECLONGITUDE,
										y = DECLATITUDE ,
										color = ORIGCTY),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	eurisco_accessions$country <-
		countrycode(eurisco_accessions$ORIGCTY, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Fixed error caused by YUG country code
	eurisco_accessions <- eurisco_accessions %>% 
		mutate(country = ifelse(ORIGCTY == 'YUG',"Slovenia",country))
	
	## Select colums to pull information
	eurisco <- eurisco_accessions %>%
		dplyr::select(
			LAT = DECLATITUDE,
			LONG = DECLONGITUDE,
			PI = ACCENUMB,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "eurisco", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(eurisco,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# FinBIF

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	finbif_accessions <-
		read.csv("raw/germplasm_sources/finbif/update_finbif-cannabis.csv") %>%
		filter(!is.na(lat_wgs84) & !is.na(lon_wgs84)) %>%
		filter(lat_wgs84 != 0.00 & lon_wgs84 != 0.00) #327 records
	
	
	## Correct the year format
	finbif_accessions$year <- substr(finbif_accessions$date_time, 1, 4)
	
	
	## Filter out Institutional records and outliers
	finbif_accessions <- cc_inst(finbif_accessions, 
																 lon = "lon_wgs84", 
																 lat = "lat_wgs84", 
																 species = "scientific_name") # 1 records removed
	finbif_accessions <- cc_outl(finbif_accessions, 
																 lon = "lon_wgs84", 
																 lat = "lat_wgs84", 
																 species = "scientific_name") # 1 records removed
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	finbif_accessions <- 
		finbif_accessions %>%
		mutate(
			lat_wgs84 = round(lat_wgs84, digits = 2),
			lon_wgs84 = round(lon_wgs84, digits = 2)
		)
	
	
	## Remove duplicates
	finbif_accessions <- 
		finbif_accessions %>%
		distinct( lat_wgs84, lon_wgs84, .keep_all = TRUE) #194
	
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	finbif_accessions <- finbif_accessions %>%
		reverse_geocode(
			lat = lat_wgs84,
			long = lon_wgs84,
			method = 'arcgis',
			full_results = TRUE
		) 
	
	
	## Exclude row with no country code as they are in ocean, through hand verification
	finbif_accessions <- finbif_accessions[-which(finbif_accessions$CountryCode == ""), ] #183
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			finbif_accessions,
			mapping = aes(x = lon_wgs84,
										y = lat_wgs84 ,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	finbif_accessions$country <-
		countrycode(finbif_accessions$CountryCode, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	
	## Select colums to pull information
	finbif <- finbif_accessions %>%
		dplyr::select(
			LAT = lat_wgs84,
			LONG = lon_wgs84,
			PI = record_id,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "finbif", .before = "PI")
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(finbif,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# GBIF

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	gbif_accessions <-
		read.csv("raw/germplasm_sources/gbif/mw_update_gbif-cannabis.csv") %>%
		filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
		filter(decimalLatitude != 0.00 & decimalLongitude != 0.00) #14408 records
	
	
	## Filter out Institutional records and outliers
	gbif_accessions <- cc_inst(gbif_accessions, 
														 lon = "decimalLongitude", 
														 lat = "decimalLatitude", 
														 species = "scientificName") # 33 records removed
	gbif_accessions <- cc_outl(gbif_accessions, 
														 lon = "decimalLongitude", 
														 lat = "decimalLatitude", 
														 species = "scientificName") # 42 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	gbif_accessions <- 
		gbif_accessions %>%
		mutate(
			decimalLatitude = round(decimalLatitude, digits = 3),
			decimalLongitude = round(decimalLongitude, digits = 3)
		)
	
	
	## Remove duplicates
	gbif_accessions <- 
		gbif_accessions %>%
		distinct( decimalLatitude, decimalLongitude, .keep_all = TRUE) #9729
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			gbif_accessions,
			mapping = aes(x = decimalLongitude,
										y = decimalLatitude ,
										color = continent),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Select colums to pull information
	gbif <- gbif_accessions %>%
		dplyr::select(
			LAT = decimalLatitude,
			LONG = decimalLongitude,
			PI = key,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "gbif", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(gbif,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# Genesys 

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	genesys_accessions <-
		read.csv("raw/germplasm_sources/genesys/update_genesys-cannabis.csv") %>%
		filter(!is.na(latitude) & !is.na(longitude)) %>%
		filter(latitude != 0.00 & longitude != 0.00) #489 records
	
	
	## Filter out Institutional records and outliers
	genesys_accessions <- cc_inst(genesys_accessions, 
															lon = "longitude", 
															lat = "latitude", 
															species = "taxonomy.currentTaxonomySpecies.genusSpecies") # 11 records removed
	genesys_accessions <- cc_outl(genesys_accessions, 
															lon = "longitude", 
															lat = "latitude", 
															species = "taxonomy.currentTaxonomySpecies.genusSpecies") # 10 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	genesys_accessions <- 
		genesys_accessions %>%
		mutate(
			latitude = round(latitude, digits = 3),
			longitude = round(longitude, digits = 3)
		)
	
	
	## Remove duplicates
	genesys_accessions <- 
		genesys_accessions %>%
		distinct( latitude, longitude, .keep_all = TRUE) #163
	
	
	## Convert country code data to country names
	genesys_accessions$country <-
		countrycode(genesys_accessions$origCty, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Fixed error caused by YUG country code (rows by column)
	genesys_accessions <- genesys_accessions %>% 
		mutate(country = ifelse(origCty == 'YUG',"Slovenia",country))
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			genesys_accessions,
			mapping = aes(x = longitude,
										y = latitude ,
										color = country),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Select columns to pull information
	genesys <- genesys_accessions %>%
		dplyr::select(
			LAT = latitude,
			LONG = longitude,
			PI = accessionNumber,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "genesys", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(genesys,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# GRIN

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	grin_accessions <-
		read.csv("raw/germplasm_sources/grin/update_grin-cannabis.csv") %>%
		filter(!is.na(latitude) & !is.na(longitude)) %>%
		filter(latitude != 0.00 & longitude != 0.00) #101 records
	
	
	## Correct year
	grin_accessions$year <- str_sub(grin_accessions$SOURCE.DATE,-4,-1)
	
	
	## Filter out Institutional records and outliers
	grin_accessions <- cc_inst(grin_accessions, 
														 lon = "longitude", 
														 lat = "latitude", 
														 species = "TAXONOMY") # 0 records removed
	grin_accessions <- cc_outl(grin_accessions, 
														 lon = "longitude", 
														 lat = "latitude", 
														 species = "TAXONOMY") # 1 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	grin_accessions <- 
		grin_accessions %>%
		mutate(
			latitude = round(latitude, digits = 3),
			longitude = round(longitude, digits = 3)
		)
	
	
	## Remove duplicates
	grin_accessions <- 
		grin_accessions %>%
		distinct( latitude, longitude, .keep_all = TRUE) #78
	
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	grin_accessions <- grin_accessions %>%
		reverse_geocode(
			lat = latitude,
			long = longitude,
			method = 'arcgis',
			full_results = TRUE
		) 
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			grin_accessions,
			mapping = aes(x = longitude,
										y = latitude ,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	grin_accessions$country <-
		countrycode(grin_accessions$CountryCode,
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Select columns to pull information
	grin <- grin_accessions %>%
		dplyr::select(
			LAT = latitude,
			LONG = longitude,
			PI = ACCESSION,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "grin", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(grin,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# iNaturalist

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	inat_accessions <-
		read.csv("raw/germplasm_sources/inat/update_inat-cannabis.csv") %>%
		filter(!is.na(latitude) & !is.na(longitude)) %>%
		filter(latitude != 0.00 & longitude != 0.00) #5013 records
	
	
	## Filter out Institutional records and outliers
	inat_accessions <- cc_inst(inat_accessions, 
																 lon = "longitude", 
																 lat = "latitude", 
																 species = "scientific_name") # 6 records removed
	inat_accessions <- cc_outl(inat_accessions, 
																 lon = "longitude", 
																 lat = "latitude", 
																 species = "scientific_name") # 6 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	inat_accessions <- 
		inat_accessions %>%
		mutate(
			latitude = round(latitude, digits = 3),
			longitude = round(longitude, digits = 3)
		)
	
	
	## Remove duplicates
	inat_accessions <- 
		inat_accessions %>%
		distinct( latitude, longitude, .keep_all = TRUE) #4039
	
	## Remove obscured localities
	inat_accessions <- filter(inat_accessions, coordinates_obscured != "true")
	
	## Search the "notes" column for any mentions of cultivation, remove those cultivated observations (if elligible for your projcet)
	inat_accessions <- inat_accessions[-c(2144,2979,3748,4058,4308),]
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	inat_accessions <- inat_accessions %>%
		reverse_geocode(
			lat = latitude,
			long = longitude,
			method = 'arcgis',
			full_results = TRUE
		) 
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			inat_accessions,
			mapping = aes(x = longitude,
										y = latitude ,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	inat_accessions$country <-
		countrycode(inat_accessions$CountryCode, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Select colums to pull information
	inat <- inat_accessions %>%
		dplyr::select(
			LAT = latitude,
			LONG = longitude,
			PI = id,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "inat", .before = "PI")
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(inat,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# KSU

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	ksu_accessions <-
		read.csv("raw/germplasm_sources/ksu/update_ksu-cannabis.csv") %>%
		filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
		filter(LATITUDE != 0.00 & LONGITUDE != 0.00) #376 records
	
	
	## Filter out Institutional records and outliers
	ksu_accessions <- cc_inst(ksu_accessions, 
															lon = "LONGITUDE", 
															lat = "LATITUDE", 
															species = "TYPE") # 0 records removed
	ksu_accessions <- cc_outl(ksu_accessions, 
															lon = "LONGITUDE", 
															lat = "LATITUDE", 
															species = "TYPE") # 0 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	ksu_accessions <- 
		ksu_accessions %>%
		mutate(
			LATITUDE = round(LATITUDE, digits = 3),
			LONGITUDE = round(LONGITUDE, digits = 3)
		)
	
	
	## Remove duplicates
	ksu_accessions <- 
		ksu_accessions %>%
		distinct( LATITUDE, LONGITUDE, .keep_all = TRUE) #
	
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	ksu_accessions <- ksu_accessions %>%
		reverse_geocode(
			lat = LATITUDE,
			long = LONGITUDE,
			method = 'arcgis',
			full_results = TRUE
		) 
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			ksu_accessions,
			mapping = aes(x = LONGITUDE,
										y = LATITUDE ,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	ksu_accessions$country <-
		countrycode(ksu_accessions$CountryCode, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Select colums to pull information
	ksu <- ksu_accessions %>%
		dplyr::select(
			LAT = LATITUDE,
			LONG = LONGITUDE,
			PI = Entry,
			COUNTRY = country,
			YEAR = YEAR
		) %>%
		tibble::add_column(SOURCE = "ksu", .before = "PI")
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(ksu,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# Mongolia

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	mongolia_accessions <-
		read.csv("raw/germplasm_sources/mongolia/mongolia.csv") %>%
		filter(!is.na(lat) & !is.na(long)) %>%
		filter(lat != 0.00 & long != 0.00) #3 records
	
	
	## Filter out Institutional records and outliers
	mongolia_accessions <- cc_inst(mongolia_accessions, 
															lon = "long", 
															lat = "lat", 
															species = "taxa") # 0 records removed
	mongolia_accessions <- cc_outl(mongolia_accessions, 
															lon = "long", 
															lat = "lat", 
															species = "taxa") # 0 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	mongolia_accessions <- 
		mongolia_accessions %>%
		mutate(
			lat = round(lat, digits = 2),
			long = round(long, digits = 2)
		)
	
	
	## Remove duplicates
	mongolia_accessions <- 
		mongolia_accessions %>%
		distinct( lat, long, .keep_all = TRUE) #3
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			mongolia_accessions,
			mapping = aes(x = long,
										y = lat,
										color = country),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Select colums to pull information
	mongolia <- mongolia_accessions %>%
		dplyr::select(
			LAT = lat,
			LONG = long,
			PI = name,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "mongolia", .before = "PI")
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(mongolia,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# Smithsonian

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	smithsonian_accessions <-
		read.csv("raw/germplasm_sources/smithsonian/cannabis.csv") %>%
		filter(!is.na(lat) & !is.na(long)) %>%
		filter(lat != 0.00 & long != 0.00) #97 records
	
	
	## Filter out Institutional records and outliers
	smithsonian_accessions <- cc_inst(smithsonian_accessions, 
																	lon = "long", 
																	lat = "lat", 
																	species = "species") # 1 records removed
	smithsonian_accessions <- cc_outl(smithsonian_accessions, 
																	lon = "long", 
																	lat = "lat", 
																	species = "species") # 10 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	smithsonian_accessions <- 
		smithsonian_accessions %>%
		mutate(
			lat = round(lat, digits = 3),
			long = round(long, digits = 3)
		)
	
	
	## Remove duplicates
	smithsonian_accessions <- 
		smithsonian_accessions %>%
		distinct( lat, long, .keep_all = TRUE) #61
	
	## Search the "notes" column for any mentions of cultivation, remove those cultivated observations
	smithsonian_accessions <- filter(smithsonian_accessions, cultivated != "Yes")
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			smithsonian_accessions,
			mapping = aes(x = long,
										y = lat,
										color = geo1),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Select colums to pull information
	smithsonian <- smithsonian_accessions %>%
		dplyr::select(
			LAT = lat,
			LONG = long,
			PI = barcode,
			COUNTRY = geo1,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "smithsonian", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(smithsonian,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# USDA


	## Load in the raw dataframe, parse out NA coordinates and 00s
	usda_accessions <-
		read.csv("raw/germplasm_sources/usda/historic.usda.plant.introductions.csv",na.strings = "NA") %>%
		filter(!is.na(Lat) & !is.na(Long)) %>%
		filter(Lat != 0.00 & Long != 0.00) #290 records
	
	
	## Filter out Institutional records and outliers
	usda_accessions <- cc_inst(usda_accessions, 
																		 lon = "Long", 
																		 lat = "Lat", 
																		 species = "Species") # 0 records removed
	usda_accessions <- cc_outl(usda_accessions, 
																		 lon = "Long", 
																		 lat = "Lat", 
																		 species = "Species") # 0 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	usda_accessions <- 
		usda_accessions %>%
		mutate(
			Lat = round(Lat, digits = 3),
			Long = round(Long, digits = 3)
		)
	
	
	## Remove duplicates
	usda_accessions <- 
		usda_accessions %>%
		distinct( Lat, Long, .keep_all = TRUE) #195
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			usda_accessions,
			mapping = aes(x = Long,
										y = Lat,
										color = Country),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Select colums to pull information
	usda <- usda_accessions %>%
		dplyr::select(
			LAT = Lat,
			LONG = Long,
			PI = PI,
			COUNTRY = Country,
			YEAR = Year
		) %>%
		tibble::add_column(SOURCE = "usda", .before = "PI")

	
	## Write out CSV of semi-clean data. 
	write.csv(apply(usda,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



# vPlants

	
	## Load in the raw dataframe, parse out NA coordinates and 00s
	vplants_accessions <-
		read.csv("raw/germplasm_sources/vPlants/occurrences.csv") %>%
		filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
		filter(decimalLatitude != 0.00 & decimalLongitude != 0.00) #563 records
	
	
	## Filter out Institutional records and outliers
	vplants_accessions <- cc_inst(vplants_accessions, 
															lon = "decimalLongitude", 
															lat = "decimalLatitude", 
															species = "scientificName") # 13 records removed
	vplants_accessions <- cc_outl(vplants_accessions, 
															lon = "decimalLongitude", 
															lat = "decimalLatitude", 
															species = "scientificName") # 5 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	vplants_accessions <- 
		vplants_accessions %>%
		mutate(
			decimalLatitude = round(decimalLatitude, digits = 3),
			decimalLongitude = round(decimalLongitude, digits = 3)
		)
	
	
	## Remove duplicates
	vplants_accessions <- 
		vplants_accessions %>%
		distinct( decimalLatitude, decimalLongitude, .keep_all = TRUE) #617
	
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	vplants_accessions <- vplants_accessions %>%
		reverse_geocode(
			lat = decimalLatitude,
			long = decimalLongitude,
			method = 'arcgis',
			full_results = TRUE
		) 
	
	## Remove cultivated records per needs
	vplants_accessions <- dplyr::filter(vplants_accessions, 
																			 !grepl("cultiv", occurrenceRemarks, ignore.case = TRUE), 
																			 !grepl("cultiv", habitat, ignore.case = TRUE),
																			 !grepl("cultiv", establishmentMeans, ignore.case = TRUE),
																			 !grepl("cultiv", locality, ignore.case = TRUE))
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			vplants_accessions,
			mapping = aes(x = decimalLongitude,
										y = decimalLatitude,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	vplants_accessions2$country <-
		countrycode(vplants_accessions2$CountryCode, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Fix row input
	vplants_accessions2[113, 47] <- "Mexico"
	
	
	## Select colums to pull information
	vplants <- vplants_accessions %>%
		dplyr::select(
			LAT = decimalLatitude,
			LONG = decimalLongitude,
			PI = recordId,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = "vPlants", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(vplants,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)



}

for (g in 1) {
## Heather Grab Ag and Markets NY Trial
	## Load in the raw dataframe, parse out NA coordinates and 00s
	grab_accessions <-
		read.csv("raw/germplasm_sources/grab_agmarket/agmarkets_ny_data_forZach&Tori.csv") %>%
		filter(!is.na(Latitude) & !is.na(Longitude)) %>%
		filter(Latitude != 0.00 & Longitude != 0.00) #854 records
	
	
	## Filter out Institutional records and outliers
	grab_accessions <- cc_inst(grab_accessions, 
														lon = "Longitude", 
														lat = "Latitude", 
														species = "Location_planted") # 0 records removed
	grab_accessions <- cc_outl(grab_accessions, 
														lon = "Longitude", 
														lat = "Latitude", 
														species = "Location_planted") # 0 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	grab_accessions <- 
		grab_accessions %>%
		mutate(
			Latitude = round(Latitude, digits = 3),
			Longitude = round(Longitude, digits = 3)
		)
	
	
	## Remove duplicates
	grab_accessions <- 
		grab_accessions %>%
		distinct( Latitude, Longitude, .keep_all = TRUE) #304
	
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	grab_accessions <- grab_accessions %>%
		reverse_geocode(
			lat = Latitude,
			long = Longitude,
			method = 'arcgis',
			full_results = TRUE
		) 
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			grab_accessions,
			mapping = aes(x = Longitude,
										y = Latitude,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	grab_accessions$country <-
		countrycode(grab_accessions$CountryCode, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Select colums to pull information
	grab <- grab_accessions %>%
		dplyr::select(
			LAT = Latitude,
			LONG = Longitude,
			PI = Authorization,
			COUNTRY = country,
			YEAR = Year
		) %>%
		tibble::add_column(SOURCE = "Ag&Markets", .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(grab,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)
	
	
	
##UWM
	## Load in the raw dataframe, parse out NA coordinates and 00s
	uwm_accessions <-
		read.csv("raw/germplasm_sources/uwm/update_uwm-cannabis.csv") %>%
		filter(!is.na(Latitude) & !is.na(Longitude)) %>%
		filter(Latitude != 0.00 & Longitude != 0.00) #53 records
	

	uwm_accessions$source <- "ADEMOLA-AINA"
	
	## Filter out Institutional records and outliers
	uwm_accessions <- cc_inst(uwm_accessions, 
																lon = "Longitude", 
																lat = "Latitude", 
																species = "source") # 0 records removed
	uwm_accessions <- cc_outl(uwm_accessions, 
																lon = "Longitude", 
																lat = "Latitude", 
																species = "source") # 0 records removed
	
	
	## Round the points to 2 digits, which should capture up to 1.1 km, this is smaller than our resolution so it will be fine with thinning. 
	uwm_accessions <- 
		uwm_accessions %>%
		mutate(
			Latitude = round(Latitude, digits = 3),
			Longitude = round(Longitude, digits = 3)
		)
	
	
	## Remove duplicates
	uwm_accessions <- 
		uwm_accessions %>%
		distinct( Latitude, Longitude, .keep_all = TRUE) #49
	
	
	## Find country code per lat/long using tinygeocoder to reverse geocode
	uwm_accessions <- uwm_accessions %>%
		reverse_geocode(
			lat = Latitude,
			long = Longitude,
			method = 'arcgis',
			full_results = TRUE
		) 
	
	
	## Use a map to check the accuracy of geocoding
	world <- borders(database = "world", colour = "gray20", fill = "white")
	world_map <- ggplot() +
		world +
		geom_point(
			uwm_accessions,
			mapping = aes(x = Longitude,
										y = Latitude,
										color = CountryCode),
			alpha = 0.5,
			size = 1.2
		)
	
	
	## Save per repository
	ggsave(
		paste0(rep, "_cleandistribution.png"),
		plot = world_map ,
		path = paste0( "raw/germplasm_sources/", rep),
		height = 7,
		width = 14
	)
	
	
	## Convert country code data to country names
	uwm_accessions$country <-
		countrycode(uwm_accessions$CountryCode, 
								origin = 'iso3c', 
								destination = "country.name")
	
	
	## Select colums to pull information
	uwm <- uwm_accessions %>%
		dplyr::select(
			LAT = Latitude,
			LONG = Longitude,
			PI = Date,
			COUNTRY = country,
			YEAR = year
		) %>%
		tibble::add_column(SOURCE = uwm_accessions$source, .before = "PI")
	
	
	## Write out CSV of semi-clean data. 
	write.csv(apply(uwm,2,as.character), paste0("raw/germplasm_sources/maxent/",rep,".csv"), row.names = FALSE)
	
	
}





# Coalesce DataFrames -----------------------------------------------------


	## Read in all semi-cleaned files
	all_accessions <- list.files(paste0("raw/germplasm_sources/maxent/"), full.names = TRUE, 
											recursive = FALSE, include.dirs = FALSE, pattern = "*.csv")
	all_accessions <- lapply(all_accessions, read.csv)
	all_accessions <- do.call(rbind, all_accessions)
	
	write.csv(final.hits,file = paste0("rst/samples/", k,"/occurrence_table.csv"),row.names = FALSE)
	
	## Add species column
	all_accessions <- all_accessions %>% tibble::add_column(SPECIES = "Cannabis sativa", .before = "LAT")

	## Map by SOURCE
	world <- borders(database = "world", colour = "gray20", fill = "white")
	
	world_map <- ggplot() +
		world +
		geom_point(all_accessions, 
							 mapping = aes(x = LONG, y = LAT, color = SOURCE), alpha = 0.5, size= 1.2)
	
	world_map
	
	## Using the map, hand select points that appear to be suspicious or oceanic records. Don't want these records to skew thinning process.
	# all_accessions <- all_accessions[!(all_accessions$LAT == "76.54"), ]
	# all_accessions <- all_accessions[!(all_accessions$LAT == "73.26"), ]
	
	## Map again
	world_map <- ggplot() +
		world +
		geom_point(all_accessions,
							 mapping = aes(x = LONG, y = LAT, color = SOURCE), alpha = 0.5, size= 1.2)

	world_map

	
	## OPTIONAL
	## Or leaflet map for final save, if you also want to use a leaflet, find a shp or json corresponding to your occurrence range
	states <- geojsonio::geojson_read("raw/germplasm_sources/grab_agmarket/gz_2010_us_040_00_500k.json", what = "sp")
	
	## Genereate map from json, add tiles for descriptive map, and minimap
	states_leaf <- leaflet(states) %>% addTiles() %>% addMiniMap()
	
	## Icons for Cannabis sativa
	canna = makeIcon(
		iconUrl = "https://icons.iconarchive.com/icons/pictogrammers/material/128/cannabis-icon.png",
		iconWidth = 20,
		iconHeight = 20
	)
	
	## Add occurrence data
	states_save <- states_leaf %>% addMarkers(data = all_accessions,~LONG, ~LAT, icon = canna)
	
	mapshot(states_save,file = "rst/maps/leaflet_prelim_all_accessions.png")
	
	
	
	## Save semi-clean accession map
	ggsave(paste0("prelim_all_accessions.png"), plot = world_map , path = paste0("rst/maps/"), height = 7, width = 14)
	
	
	## OPTIONAL
	## Subset countries to continent or any other code from the codelist: https://www.rdocumentation.org/packages/countrycode/versions/1.4.0/topics/codelist
	all_accessions$CONTINENT <- countrycode(sourcevar = all_accessions[, "COUNTRY"],
																 origin = "country.name",
																 destination = "continent",
																 nomatch = "BAD")
	
	## Remove duplicates across repositories, UNLESS YOU ARE COMPARING POPULATIONS, IN THAT CASE, DO NOT COMPARE DISTINCT RECORDS ACROSS REPOSITORIES, DUPLICATES ALREADY REMOVED
	all_accessions <- distinct(all_accessions, LAT, LONG, .keep_all = TRUE) #15531 -> 11712
	
	## INSTEAD, UNIQUELY NAME YOUR POPULATIONS
	all_accessions$REGION <- ""
	## Unless you plan to constrain to a region (Complete in next section)
	## Use any unique identifier (constrain regions by source)
	all_accessions <- all_accessions %>% 
		mutate(REGION = case_when(
			all_accessions$SOURCE == "Ag&Markets" ~ "NY-Trial",
			all_accessions$SOURCE == "ADEMOLA-AINA" ~ "Midwest-Feral"
		))
	
	## Save the semi-clean data
	write.csv(alldf, paste0("rst/samples/prelim_accessions.csv"), row.names = FALSE)
	## Clean data by hand, in excel, take out any "BAD" region results. Used https://www.gps-coordinates.net/ to search.
	
	

# Constrain your Occurrences to a Region (i.e., the MidWest US) -----------

## GBIF Original Documentation: https://data-blog.gbif.org/post/shapefiles/
## Read in a shape of the region for which you plan to constrain your occurrences 
constrain <- readOGR("raw/shapefiles/midwest_us.shp")
	
ALL <- read.csv("rst/samples/prelim_accessions.csv")

ALL <- read.csv("rst/samples/Midwest/maxent_ready.csv")

## Subset the relevent data
occ.map <- data.frame(ALL[["LONG"]], ALL[["LAT"]], ALL[["SOURCE"]])

## Correct the names
names(occ.map)[1:3] <- c('LONG', 'LAT', 'SOURCE')

## Make spatial
coordinates(occ.map) <- c("LONG", "LAT")

## Assign a CRS
proj4string(occ.map) <- "+proj=longlat +datum=WGS84"

## Reproject to the same as the shapefile 
occ.map <- spTransform(occ.map, raster::crs(constrain))

## Gathers all records within the bounds of the shape
inside <- occ.map[apply(gIntersects(occ.map, constrain, byid = TRUE), 2, any),]

## Take all occurrences inside the region
ins.ALL <- data.frame(inside@coords)

## Join the occurrences back to the Maxent format
FINAL <- ALL %>% semi_join(ins.ALL, by = c(LONG = "LONG", LAT = "LAT"))

## Optional if not constraining or seperating region into smaller regions (i.e. Midwest to states)
## Add in a "REGION" column - name it according to your shape
FINAL$REGION <- 'Midwest'

## Write out the csv constrained to a region (i.e. Midwest US), can overwrite the other file unless needs point otherwise
write.csv(FINAL, paste0("rst/samples/prelim_accessions.csv"), row.names = FALSE)

## Map again if needed
## Genereate map from json, add tiles for descriptive map, and minimap
leaf <- leaflet(constrain) %>% addTiles() %>% addMiniMap()

## Icons for Cannabis sativa
canna = makeIcon(
	iconUrl = "https://icons.iconarchive.com/icons/pictogrammers/material/128/cannabis-icon.png",
	iconWidth = 20,
	iconHeight = 20
)

## Add occurrence data
saves <- leaf %>% addMarkers(data = FINAL,~LONG, ~LAT, icon = canna,popup = ~htmlEscape(SOURCE))

mapshot(saves,url = "rst/maps/leaflet_midwest_prelim_all_accessions.html")
mapshot(saves,file = "rst/maps/leaflet_midwest_prelim_all_accessions.png")

## Subset BY State
## Use reverse geocoding to 
ins.state <- ins.ALL %>%
	reverse_geocode(
		lat = LAT,
		long = LONG,
		method = 'arcgis',
		full_results = TRUE
	)

## Select the LONG, LAT, and Region columns
state <- ins.state[,c(1,2,18)]

## Joins together the state information into the long-form dataframe
ncombined <- left_join(ALL, state, by = c("LONG","LAT"), multiple = "all")
write.csv(ncombined, paste0("rst/samples/Midwest/sbs_maxent_ready.csv"), row.names = FALSE)

## Make some empty lists
states <- c()
counts <- c()
## Create lists of states, and of the count of occurrences per state
for (j in unique(ncombined$Region)) {
	state_count <- length(ncombined[ncombined==j]) 
	states[[j]] <- j
	counts[[j]] <- state_count
}

## Column bind the states to their occurrence count, make this a data frame
occ_by_state <- cbind(unlist(states),unlist(counts)) %>% as.data.frame()

write.csv(occ_by_state, paste0("rst/samples/state_by_state_occs.csv"), row.names = FALSE)

# Spatial Thinning for All Accessions -------------------------------------



	## Read in pre-thinning data to new dataframe
	alldf <- read.csv(paste0("rst/samples/prelim_accessions.csv"))

	## Build list of thinning distance in kilometers
	dist <- c(0.5)
	
	
	for (j in dist) {
		
		set.seed(430) ## Set seed if you want reproducible thinning numbers, pick any number and as long as it is consistent, the results of thin() will be too
		
		alldfsp <- alldf ## Create a separate spatial frame to avoid overwriting
	
		keep_km <- spThin::thin(loc.data =  alldfsp, 
															verbose = FALSE,
															long.col = "LONG",
															lat.col = "LAT",
															spec.col = "SPECIES",
															thin.par = j, ## Thin the records on the distances set in the earlier list
															reps = 1,
															locs.thinned.list.return = TRUE,
															write.files = FALSE)[[1]]
		df_km <- alldfsp %>%
			filter((LAT %in% keep_km$Latitude +
								LONG %in% keep_km$Longitude) == 2) ## Use thinned lat/long to search in spatial frame and match occurrences, make a new df of them
		
		## Map for each distance
		world <- borders(database = "world", colour = "gray20", fill = "white")
		map_km <- ggplot() +
			world +
			geom_point(
				df_km,
				mapping = aes(x = LONG,
											y = LAT ,
											color = SOURCE),
				alpha = 0.5,
				size = 1.2
			)
	
		## Save 'dist' km thinned map
		ggsave(paste0("all_accessions_",j,"km.png"), plot = map_km , path = paste0("rst/maps/"), height = 7, width = 14)
	
		## Save the km thinned data frame
		write.csv(df_km, paste0("rst/samples/all_accessions_",j,"km.csv"), row.names = FALSE)
	}



# Spatial Thinning for Region by Distance Accessions ----------------------

	
	## Read in pre-thinning data to new dataframe
	alldf <- read.csv(paste0("rst/samples/prelim_accessions.csv"))
	
	## Build list of thinning distance in kilometers
	dist <- c(0.5)
	
	## Build list of regions
	regions <- unique(alldf$REGION)
	
	
for (k in regions) {
	
	for (j in dist) {
		
		set.seed(430) ## Set seed if you want reproducible thinning numbers, pick any number and as long as it is consistent, the results of thin() will be too
		
		regdf <-  alldf %>%
			dplyr::filter(REGION == k) ## Filter by region
		
		regdfsp <- regdf ## Create a separate spatial frame to avoid overwriting
		
		keep_km <- spThin::thin(loc.data =  regdfsp, 
														verbose = FALSE,
														long.col = "LONG",
														lat.col = "LAT",
														spec.col = "SPECIES",
														thin.par = j, ## Thin the records on the distances set in the earlier list
														reps = 1,
														locs.thinned.list.return = TRUE,
														write.files = FALSE)[[1]]
		df_km <- regdfsp %>%
			filter((LAT %in% keep_km$Latitude +
								LONG %in% keep_km$Longitude) == 2) ## Use thinned lat/long to search in spatial frame and match occurrences, make a new df of them
		
		## Map for each distance
		world <- borders(database = "world", colour = "gray20", fill = "white")
		map_km <- ggplot() +
			world +
			geom_point(
				df_km,
				mapping = aes(x = LONG,
											y = LAT ,
											color = SOURCE),
				alpha = 0.5,
				size = 1.2
			)
		
		## Save 'dist' km thinned map
		dir.create(paste0("rst/maps/", k))
		ggsave(paste0(k,"_accessions_",j,"km.png"), plot = map_km , path = paste0("rst/maps/",k), height = 7, width = 14)
		
		## Save the km thinned data frame
		dir.create(paste0("rst/samples/", k))
		write.csv(df_km, paste0("rst/samples/",k,"/",k,"_accessions_",j,"km.csv"), row.names = FALSE)
	}
	
}





