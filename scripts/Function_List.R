## Function_List.R
## Tori Ford
### 22-02-23
#### Objective: Create a dataframe containing all functions, and the scripts they appear in.


onefunction <- data.frame(
	Packages = c('genesysr'
							 ,'...'
							 
							 ,'plantR'
							 
							 ,'rgbif'
							 
							 ,'BIEN'
							 
							 ,'finbif'
							 
							 ,'galah'
							 ,'...'
							 ,'...'
							 
							 ,'rinat'
							 ),
	Functions = c('mcpd_filter()'
								 ,' get_accessions()'
								
								 ,'rspeciesLink()'
								
								 ,'occ_data()'
								
								 ,'BIEN_occurrence_genus()'
								
								 ,'finbif_occurrence()'
								
								 ,'galah_config()'
								 ,'galah_call()'
								 ,'atlas_occurrences()'
								
								 ,'get_inat_obs()'
								 ), 
	Descriptions = c('sets parameters in correct format for filtering the Genesys repository'
									,'uses mcpd filter to fetch passport data from Genesys repository'
									
									,'contacts the specieslink API, to search for occurrence records using specific parameters'
									
									,'contacts the GBIF API for a simplified search of occurrence records'
									
									,'contacts the BIEN API, to search for occurrence records by genus'
									
									,'contacts the FINBIF API to search for occurrences by genus and maximum records'
									
									,'sets the target atlas, in this case, it is the Atlas of Living Australia'
									,'calls the parameters used to search within the target atlas'
									,'contacts the target API using the parameters set in galah_call()'
									
									,'contacts the iNaturalist API to search for occurrences by taxon name, record quality, and maximum records'
									),
	Citations = c('Obreza M (2023). _genesysr: Genesys PGR Client_. R package version 1.0.1,
  <https://gitlab.croptrust.org/genesys-pgr/genesysr>.'
								,'...'
								
								,'Lima, R.A.F., Sánchez-Tapia, A., Mortara, S.R., ter Steege, H., Siqueira, M.F. (2021). 
	plantR: An R package and workflow for managing species records from biological collections. 
	Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210X.13779' 
								
								,'Chamberlain S, Barve V, Mcglinn D, Oldoni D, Desmet P, Geffert L, Ram K (2023). _rgbif: Interface to the Global
  Biodiversity Information Facility API_. R package version 3.7.5, <https://CRAN.R-project.org/package=rgbif>.'
								
								,'Maitner B (2023). _BIEN: Tools for Accessing the Botanical Information and Ecology Network Database_. R package
  version 1.2.6, <https://CRAN.R-project.org/package=BIEN>.'
								
								,'Morris, William K. (2023). Introduction to the finbif package. 
								,R package version 0.7.2, https://doi.org/10.5281/zenodo.3612814'
								
								,'Westgate M, Stevenson M, Kellie D, Newman P (2023). _galah: Atlas of Living Australia (ALA) Data and Resources in
  R_. R package version 1.5.1, <https://CRAN.R-project.org/package=galah>.'
								,'...'
								,'...'
								
								,'Barve V, Hart E (2022). _rinat: Access iNaturalist Data Through APIs_. R package version 0.1.9,
  <https://CRAN.R-project.org/package=rinat>.'
								)
)



twofunction <- data.frame(
	Packages = c('CoordinateCleaner'
							 ,'...'
							 
							 ,'tidygeocoder'
							 
							 ,'countrycode'
							 
							 ,'spThin'
							 
							 ,'sp'
							 
							 ,'...'
							 
							 ,'...'
	),
	Functions = c('cc_inst()'
								,'cc_outl'
								
								,'reverse_geocode()'
								
								,'countrycode()'
								
								,'thin'
								
								,'coordinates'
								
								,'proj4string'
								
								,'spTransform'
	), 
	Descriptions = c('identifies occurrence records within the vicinity of biodiversity institutions, such as museums and gardens'
									 ,'identifies outlier records by both space and time'
									 
									 ,'uses latitude and longitude coordinates to return reverse geocoding queries'
									 
									 ,'converts country name coding schemes into long-form names, as well as the reverse'
									 
									 ,'thins species occurrences where, at a minimum, all occurrence points are a set distance parameter apart'
									 
									 ,'sets coordinates for, or to create, a spatial object'
									 
									 ,'sets PROJ 4 string representation of CRS for a spatial object'
									 
									 ,'transforms object between projections using WKT2 strings'
		
	),
	Citations = c('Zizka A, Silvestro D, Andermann T, Azevedo J, Duarte Ritter C, Edler D, Farooq H, Herdean A, 
								Ariza M, Scharn R, Svanteson S, Wengtrom N, Zizka V & Antonelli A (2019) CoordinateCleaner: 
								standardized cleaning of occurrence records from biological collection databases. Methods in Ecology 
								and Evolution, 10(5):744-751, doi:10.1111/2041-210X.13152, https://github.com/ropensci/CoordinateCleaner'
								,'...'
								,'Cambon J, Hernangómez D, Belanger C, Possenriede D (2021).
								  tidygeocoder: An R package for geocoding. Journal of Open Source
								  Software, 6(65), 3544, https://doi.org/10.21105/joss.03544 (R package
								  version 1.0.5)'
								,'Arel-Bundock et al., (2018). countrycode: An R package to convert country names and country codes. 
								Journal of Open Source Software, 3(28), 848, https://doi.org/10.21105/joss.00848'
								,'Aiello-Lammens, M. E., Boria, R. A., Radosavljevic, A. , Vilela, B. and Anderson, R. P. (2015). spThin: an R
  package for spatial thinning of species occurrence records for use in ecological niche models. Ecography, 38:
  541-545. URL https://onlinelibrary.wiley.com/doi/10.1111/ecog.01132.'
								,'Pebesma EJ, Bivand RS (2005). “Classes and methods for spatial data in R.” R News, 5(2), 9–13. https://CRAN.R-project.org/doc/Rnews/. 
								; Bivand RS, Pebesma E, Gomez-Rubio V (2013). Applied spatial data analysis with R, Second edition. Springer, NY. https://asdar-book.org/.'
								,'...'
								,'...'
		
	)
)

threefunction <- data.frame(
	Packages = c('geodata'
							 ,'...'
							 ,'...'
							 
							 ,'raster'
							 ,'...'
							 ,'...'
							 ,'...'
							 ,'...'
							 ,'...'
							 
							 ,'rangeBuilder'
							 
							 ,'sf'
							 ,'...'
							 
							 ,'rgdal'
	),
	Functions = c('worldclim_global()'
								,'elevation_global()'
								,'landcover()'
								
								,'projectRaster()'
								,'resample()'
								,'stack()'
								,'crop()'
								,'mask()'
								,'merge()'
								
								,'getDynamicAlphaHull()'
								
								,'st_geometry()'
								,'as_Spatial()'
								
								,'spTransform()'
	), 
	Descriptions = c('downloads WorldClim global climate data, in the selected resolution, to the selected path'
									 ,'downloads WorldClim global elevation data, in the selected resolution, to the selected path'
									 ,'downloads WorldCover ESA landcover data, in 30 arc-second resolution, to the selected path'
									 
									 ,'projects values of a raster layer to that of another projection, typically the coordinate 
									 reference system (CRS), using either bilinear or nearest neighbor interpolation'
									 ,'transfers resolution values between two non-matching raster objects'
									 ,'collects rasterlayer objects with the same resolution and extent, compiles into a rasterstack object'
									 ,'subsets a raster by extent using an extent object'
									 ,'creates a new raster object where NA cells outside of cropped extent are dropped, can also be inverse'
									 ,'merges individual raster objects into one singluar layer with a larger spatial extent'
									 
									 ,'generates a buffered alpha hull polygon around the spatial distribution of given occurrence coordinates'
									 
									 ,'retrieves geometry from sf/sfc object'
									 ,'converts simple feature object to a spatial sp object'
									 
									 ,'transforms object between projections using WKT2 strings'
	),
	Citations = c('Hijmans RJ, Ghosh A, Mandel A (2023). _geodata: Download Geographic Data_. 
								R package version 0.5-6<https://cran.r-project.org/web/packages/geodata/index.html>'
								,'...'
								,'...'
								
								,'Hijmans R (2023). _raster: Geographic Data Analysis and Modeling_. R package version 3.6-13,
  <https://CRAN.R-project.org/package=raster>.'
								,'...'
								,'...'
								,'...'
								,'...'
								,'...'
								
								,'Alison R. Davis Rabosky, Christian L. Cox, Daniel L. Rabosky, Pascal O. Title, Iris A. Holmes, Anat Feldman and
  Jimmy A. McGuire. 2016. Coral snakes predict the evolution of mimicry across New World snakes. Nature
  Communications 7:11484.'
								
								,'Edzer Pebesma, 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal 10:1, 439-446.'
								,'...'
								
								,'Bivand R, Keitt T, Rowlingson B (2023). _rgdal: Bindings for the Geospatial Data Abstraction Library_. R package
  version 1.6-4, <https://CRAN.R-project.org/package=rgdal>.'
	)
)


fourfunction <- data.frame(
	Packages = c('USDM'
							 ,'...'
							 
							 ,'raster'
							 ,'...'
							 
							 ,'ENMeval'
							 
							 ,'dplyr'
							 ,'...'
							 
							 ,'tidyr'
							 
							 ,'Vegan'
							 ,'...'
	),
	Functions = c('vifstep()'
								,'exclude()'
								
								,'subset()'
								,'extract()'
								
								,'eval.variable.importance()'
								
								,'rename()'
								,'slice()'
								
								,'drop_na()'
								
								,'rda()'
								,'scores()'
	),
	Descriptions = c('calculates VIF scores for all environmental predictors, excludes the highest score over the threshold until
									 no variables greater than the threshold exist'
									 ,'excludes all variables identified, by either vifcor() or vifstep(), as being beyond the accepted threshold'
									 
									 ,'extracts rasters from a RasterStack or RasterBrick object by parsing layer names'
									 ,'extracts data from Raster object by coordinates'
									 
									 ,'retrieves the variable importance of an ENMevaluation object(s)'
									 
									 ,'replaces specified column with selected name'
									 ,'indexes rows by row number'
									 
									 ,'drops rows where any column (can be specified) contains missing values'
									 
									 ,'performs a redundancy analysis (optionally, PCA is available)'
									 ,'returns species or site scores for the specified axes of a ordination analysis'
	),
	Citations = c('Naimi B, Hamm Na, Groen TA, Skidmore AK, Toxopeus AG (2014). “Where is positional uncertainty a problem for species
								distribution modelling.” Ecography, 37, 191-203. doi:10.1111/j.1600-0587.2013.00205.x.'
								,'...'
								
								,'Hijmans R (2023). _raster: Geographic Data Analysis and Modeling_. R package version 3.6-13,
  <https://CRAN.R-project.org/package=raster>.'
								,'...'
								
								,'Kass, J. M., Muscarella, R., Galante, P. J., Bohl, C. L., Pinilla-Buitrago, G. E., Boria, R. A., Soley-Guardia,
  M., and R. P. Anderson (2021). ENMeval 2.0: Redesigned for customizable and reproducible modeling of species’
  niches and distributions. Methods in Ecology and Evolution. 12(9), 1602-1608.
  https://doi.org/10.1111/2041-210X.13628'
								
								,'Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation_. R package
  version 1.1.0, <https://CRAN.R-project.org/package=dplyr>.'
								,'...'
								
								,'Wickham H, Vaughan D, Girlich M (2023). _tidyr: Tidy Messy Data_. R package version 1.3.0,
  <https://CRAN.R-project.org/package=tidyr>.'
								
								,'Oksanen J, Simpson G, Blanchet F, Kindt R, Legendre P, Minchin P, OHara R, Solymos P, Stevens M, Szoecs E, Wagner
								H, Barbour M, Bedward M, Bolker B, Borcard D, Carvalho G, Chirico M, De Caceres M, Durand S, Evangelista H,
								FitzJohn R, Friendly M, Furneaux B, Hannigan G, Hill M, Lahti L, McGlinn D, Ouellette M, Ribeiro Cunha E, Smith T,
								Stier A, Ter Braak C, Weedon J (2022). _vegan: Community Ecology Package_. R package version 2.6-4,
								<https://CRAN.R-project.org/package=vegan>.'
								,'...'
	)
)



fivefunction <- data.frame(
	Packages = c(
		'ENMeval'
		,'...'
		,'...'
		,'...'
		,'...'
		
		,'dismo'
	),
	Functions = c(
		'similarity()'
		,'evalplot.grps()'
		,'evalplot.envSim.map()'
		,'ENMevaluate()'
		,'calc.niche.overlap()'
		
		,'randomPoints()'
	),
	Descriptions = c(
		'calculates multivariate environmental similarity statistics'
		,'plots occurrence partitions groups over environmental rasters'
		,'plots enviromental similarity statistics over partitions'
		,'generates niche models using settings defined by the user'
		,'computes pairwise comparison of niche overlap in g-space'
		
		,'generates random points to the extent of the study area, sampled without replacement'
	),
	Citations = c(
		'Kass, J. M., Muscarella, R., Galante, P. J., Bohl, C. L., Pinilla-Buitrago, G. E., Boria, R. A., Soley-Guardia,
  M., and R. P. Anderson (2021). ENMeval 2.0: Redesigned for customizable and reproducible modeling of species’
  niches and distributions. Methods in Ecology and Evolution. 12(9), 1602-1608.
  https://doi.org/10.1111/2041-210X.13628'
		,'...'
		,'...'
		,'...'
		,'...'
		
		,'Hijmans RJ, Phillips S, Leathwick J, Elith J (2022). _dismo: Species Distribution Modeling_. R package version
  1.3-9, <https://CRAN.R-project.org/package=dismo>.'
	)
)



sixfunction <- data.frame(
	Packages = c(
		''
	),
	Functions = c(
		''
	),
	Descriptions = c(
		''
	),
	Citations = c(
		''
	)
)

# threefunction <- data.frame(
# 	Packages = c(
# 	),
# 	Functions = c(
# 	), 
# 	Descriptions = c(
# 	),
# 	Citations = c(
# 	)
# )

