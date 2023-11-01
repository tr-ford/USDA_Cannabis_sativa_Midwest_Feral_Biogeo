## To download climatic models in a loop
run_cmip6 <- function(models,
											ssps,
											time_period,
											variables,
											res,
											cmip6.dir = NULL,
											download.opt = NULL,
											...){
	
	folder <- paste0(models,"_",time_period,"_",ssps)

	dir.create(paste0(cmip6.dir,folder))
	print(paste0(folder," created"))
	
	if (is.null(download.opt)) {direct <- tempdir()} else {direct <- paste0(cmip6.dir,folder)}
	
	for (m in models) {
		for (s in ssps) {
			for (t in time_period) {
				for (v in variables) {
					
					worldclim <- geodata::cmip6_world(var = v,
																						time = t,
																						ssp = s,
																						model = m,
																						res = res,
																						path = direct)
					
				}
			}
		}
	}
	path = paste0(cmip6.dir,folder,"/")
	return(path)
}







## To unlist rasters nested within one another, returns a raster stack of future rasters
unlist_future <- function(path){
	
	future_list <- list.files(path = paste0(path, "wc2.1_2.5m/"), pattern = "*.tif", full.names = TRUE)
	future_stack <- c()
	
	for (i in 1:length(future_list)) {
		
		future_stack[[i]] <- raster::stack(future_list[[i]])
	}
	
	future_stack <- raster::stack(future_stack)
	
	names(future_stack) <- gsub("\\.","_", names(future_stack))
	
	return(future_stack)
}


