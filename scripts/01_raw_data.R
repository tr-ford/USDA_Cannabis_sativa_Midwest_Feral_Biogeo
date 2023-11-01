## 01_raw_data.R
## Script One
## Tori Ford
### 27-01-23
#### Objective: To use APIs to gather raw accession data for Cannabis sativa and its synonyms (Or just the Cannabis L. genus). Do a bit of formatting for next script

library(dplyr) # 1.1.0
library(tidyverse) # 2.0.0
library(readxl) # 1.4.2
library(sp) # 1.5.1
library(rworldmap) # 1.3.6
## Install Genesys API Package for occurrences
# devtools::install_git('https://gitlab.croptrust.org/genesys-pgr/genesysr')
library(genesysr) # 1.1.0
## Install CRIA API using plantR Package
# remotes::install_github("LimaRAF/plantR")
library(plantR) # 0.1.6
## Load the GBIF API Package
library(rgbif) # 3.7.5
## Load the BEIN API Package
library(BIEN) # 1.2.6
## Load the FinBIF API Package
library(finbif) # 0.7.2
## Load the ALA API Package
library(galah) # 1.5.1
## Load the iNat APi Package
library(rinat) # 0.1.9


# ## Set working directory variable
# wd <- getwd()

## Set repository
rep <- "uwm"


### Genesys Accessions ------------------------------------------------------

## Connect to the genesys API
genesysr::setup_production()

## Log into your account
# If you login using a email or other outside authorization, you may have to login twice, or already be logged in on the browser. 
genesysr::user_login()

## Filter by Genus, create a dataframe
genesys_filters <- mcpd_filter(GENUS = c("Cannabis"))
genesys_accessions <- genesysr::get_accessions(genesys_filters)

## Check the dataframe for any incorrectly added species, note incorrect inclusions for cleaning script
unique(genesys_accessions$taxonomy.species)
# no incorrectly included species. 

## Correct the year format
genesys_accessions$year <- substr(genesys_accessions$acquisitionDate, 1, 4)

## Write out the raw genesys dataframe
write.csv(genesys_accessions, file = paste0("raw/germplasm_sources/genesys/update_genesys-cannabis.csv"), row.names = FALSE)



### CRIA (Centro de Referência em Informação Ambiental) Accessions ----------

## Filter by Genus, create a dataframe
cria_accessions <- rspeciesLink(species = "Cannabis",
																Coordinates = "Yes",
																Synonyms = "flora2020" #adds synonyms from Brazilian Flora 2020 Project
)

## Check the dataframe for any incorrectly added species, note incorrect inclusions for cleaning script
unique(cria_accessions$scientificName)
# no incorrectly included species. 

## Filter out any records containing notes of cultivation (if looking for only feral/wild records)
## First inspect the comments, if any note something other than cultivation, add back to df
cria_accessions2 <- cria_accessions[grep("culti", cria_accessions$occurrenceRemarks), ]

## Then filter out cultivated samples
cria_accessions <- cria_accessions[!grepl("culti", cria_accessions$occurrenceRemarks), ]

## Write out the raw CRIA dataframe
write.csv(cria_accessions, file = paste0("raw/germplasm_sources/cria/update_cria-cannabis.csv"), row.names = FALSE)



### GBIF (Global Biodiversity Information Facility) Accessions --------------

## Use rgbif to filter by Genus, set limit to specific needs
gbif_data <- occ_data(scientificName = "Cannabis", limit = 30000)

## Separate out data into workable dataframe
gbif_accessions <- gbif_data$data

## Check establishment means, refer to gbif documentation to find which to filter (ferals = vagrants?)
unique(gbif_accessions$establishmentMeans)

## Check the scientific name to confirm synonyms for any incorrectly added species, note incorrect inclusions for cleaning script
unique(gbif_accessions$scientificName)
## Synonyms (ones we accept) have been converged as two species, C. sativa and C. indica. 
unique(gbif_accessions$species)

## Check scientific names for records under synonymous names, note ones that are
check_gbif <- filter(gbif_accessions, scientificName == "NA")

## Filter establishmentMeans per needs
gbif_accessions <- filter(gbif_accessions,  establishmentMeans != "NA" & establishmentMeans != "Uncertain")
unique(gbif_accessions$establishmentMeans)

## Coerce the gbif tibble dataframe to one that can be saved as a csv
## Write out the raw GBIF dataframe
write.csv(apply(gbif_accessions,2,as.character), file = paste0("raw/germplasm_sources/gbif/update_gbif-cannabis.csv"), row.names = FALSE)



### BIEN (Botanical Information and Ecology Network) Accessions -------------

## Filter by Genus, create a dataframe
bien_accessions <- BIEN_occurrence_genus("Cannabis", all.taxonomy = TRUE, collection.info = TRUE, natives.only = FALSE, native.status = TRUE)

## Check the dataframe for any incorrectly added species, note incorrect inclusions for cleaning script
unique(bien_accessions$name_matched)

## Write out the raw BIEN dataframe, including duplicate GBIF accessions
write.csv(bien_accessions, file = paste0("raw/germplasm_sources/bien/gbif_included_bien-cannabis.csv"), row.names = FALSE)

## Make column names unique
valid_column_names <- make.names(names=names(bien_accessions), unique=TRUE, allow_ = TRUE)
names(bien_accessions) <- valid_column_names

## Filter out GBIF occurrences
unique(bien_accessions$datasource)
bien_accessions_exc <- filter(bien_accessions, datasource != "GBIF")

## Write out the fixed BIEN dataframe
write.csv(bien_accessions_exc, file = paste0("raw/germplasm_sources/bien/update_bien-cannabis.csv"), row.names = FALSE)



### FinBIF (Finnish Biodiversity Information Facility) Accessions ----------

## Request Access to the FinBIF database by putting in a viable email address to receive an access token
finbif_request_token("tmf94@cornell.edu")

## Input the access token you recieved in the given email inbox. 
Sys.setenv(FINBIF_ACCESS_TOKEN = "")

## Filter by Genus, create a dataframe
finbif_accessions <- finbif_occurrence("Cannabis sativa", n = 500)

## Check the dataframe for any incorrectly added species, note incorrect inclusions for cleaning script
unique(finbif_accessions$scientific_name)

## Write out the raw FinBIF dataframe
write.csv(finbif_accessions, file = paste0(wd, "/raw/germplasm_sources/",rep,"/update_",rep,"-cannabis.csv"), row.names = FALSE)



### ALA (Atlas of Living Australia) Accessions ------------------------------

## Request Access to the ALA database by putting in a viable email address to receive an access token
galah_config(email = "tmf94@cornell.edu")

## Configure the API to the correct repository
galah_config(atlas = "ALA")

## Filter by genus, call basic group of columns, create a dataframe
ala_accessions <- galah_call() |>
	galah_identify("Cannabis L.") |>
	galah_select(basisOfRecord, group = "basic") |>
	atlas_occurrences()

## Check the dataframe for any incorrectly added species, note incorrect inclusions for cleaning script
unique(ala_accessions$scientificName)

## Format data to include a separate column for year
ala_accessions$date <- as.Date(ala_accessions$eventDate)

ala_accessions$year <- as.numeric(format(ala_accessions$date, "%Y"))

## Write out the raw ALA dataframe
write.csv(ala_accessions, file = paste0("raw/germplasm_sources/ala/update_ala-cannabis.csv"), row.names = FALSE)



### iNat (iNaturalist) Accessions -------------------------------------------

## Filter by Genus,specify research grade (will only take observations verified by 3 or more observers),create a dataframe
inat_accessions <- get_inat_obs(taxon_name = "Cannabis", quality = "research", maxresults = 10000)

## Check the dataframe for any incorrectly added species, note incorrect inclusions for cleaning script
unique(inat_accessions$scientific_name)

## Filter out any cultivated accessions (if looking for wild/feral data)
## Documentation : https://www.inaturalist.org/pages/help-inaturalist-canada-en#:~:text=Checking%20captive%20%2F%20cultivated%20means%20that,they%20intended%20to%20do%20so.
inat_accessions <- inat_accessions[!isTRUE(inat_accessions$captive_cultivated), ]

## Format data to include a separate column for year
inat_accessions$date <- as.Date(inat_accessions$datetime)

inat_accessions$year <- as.numeric(format(inat_accessions$date, "%Y"))

## Write out the raw iNat dataframe
write.csv(inat_accessions, file = paste0("raw/germplasm_sources/inat/update_inat-cannabis.csv"), row.names = FALSE)



### EURISCO (European Search Catalogue for Plant Genetic Resources)  --------

## Read in new EURISCO passport data
eurisco_accessions <- read.csv("/Users/tori.ford/Downloads/species_report (1).txt", encoding = "UTF-8")

## Check the dataframe for any incorrectly added species, note incorrect inclusions for cleaning script
unique(eurisco_accessions$SPECIES)

## Correct the year format
eurisco_accessions$year <- substr(eurisco_accessions$ACQDATE, 1, 4)

## Write out the raw EURISCO dataframe
write.csv(eurisco_accessions, file = paste0("raw/germplasm_sources/eurisco/update_eurisco-cannabis.csv"), row.names = FALSE)



### KSU (Kansas State University) Hemp --------------------------------------

## Read in excel file
ksu_accessions <- read_excel("raw/germplasm_sources/ksu/KS_NaturalizedHempLocations.xlsx")

## Convert into CSV
write.csv(ksu_accessions, file = paste0("/raw/germplasm_sources/ksu/update_ksu-cannabis.csv"), row.names = FALSE)



# GRIN Accessions --------------------------------------------------

## Read in excel file
grin_accessions <- read_excel("raw/germplasm_sources/grin/Search_Accessions_GRIN-Global.xlsx",range = "B2:Q445")

## Fix coordinate data
## Split the column using comma, parse into separate data frame
coords <- data.frame(do.call('rbind', strsplit(as.character(grin_accessions$COORDINATES),',',fixed=TRUE)))

## Take columns generated in last step and add them as new columns in GRIN data
grin_accessions$latitude <- coords$X1
grin_accessions$longitude <- coords$X2

## Filter out cultivated occurrences
grin_accessions <- filter(grin_accessions,  `IMPROVEMENT STATUS` != "CULTIVAR" & `IMPROVEMENT STATUS` != "CLONE" & `IMPROVEMENT STATUS` != "UNCERTAIN")

## Convert into CSV
write.csv(grin_accessions, file = paste0("raw/germplasm_sources/grin/update_grin-cannabis.csv"), row.names = FALSE)



# UWM ---------------------------------------------------------------------

uwm_accessions <- read_excel("raw/germplasm_sources/uwm/UWM_Cannabis_Sativa_Coord_Corrected.xlsx")

uwm_accessions$year <- substr(uwm_accessions$Date, 1, 4)

write.csv(uwm_accessions, file = paste0("raw/germplasm_sources/",rep,"/update_",rep,"-cannabis.csv"), row.names = FALSE)

# Completed Accession Data ------------------------------------------------


### Historic USDA Accessions
### vPlants (A Virtual Herbarium of the Chicago Region) Accessions
### Cornell Accessions (Some US landrace data provided by Dr. Craig Carlson)
### (Flora of) Mongolia Accessions
### KSU (Kansas State University) Hemp Accessions
### Smithsonian Accessions












