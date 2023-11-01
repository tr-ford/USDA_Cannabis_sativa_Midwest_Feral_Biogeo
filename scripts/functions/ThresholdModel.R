## ThresholdModel.R
## Authored by: Anthony E. Melton
## Date: November 30th 2021
## Title: ThresholdModel.R
## Description: 'This function will take a suitability score raster and generate a binary predicted occurrence/absence map. I recommend using the minimum presence threshold ("MPT").'
## Link: https://github.com/aemelton/EA_ENA_ENM/blob/master/Functions/ThresholdModel.R
## Citation: Melton, A. E.(2021, November 30) ThresholdModel.R, github.io, https://github.com/aemelton/EA_ENA_ENM/blob/master/Functions/ThresholdModel.R.
## Modified on 16-03-2023 by Tori M. Ford



ThresholdModel <- function(usr.raster, usr.occs, method, output.type){
	usr.raster <- terra::setMinMax(usr.raster)

	SuitabilityScores <- terra::extract(usr.raster, usr.occs) ## Terra now returns a data.frame as a default instead of a matrix, so we must change the approach to indexing into columns

	SuitabilityScores <- SuitabilityScores[complete.cases(SuitabilityScores), ]
	
	if(method == "MPT"){
		threshold <- min(SuitabilityScores)
	} else if(method == "95pct"){
		threshold <- sort(SuitabilityScores$layer, decreasing = T)[round(length(SuitabilityScores$layer)*.95,0)] 
	} else if(method == "90pct"){
		threshold <- sort(SuitabilityScores$layer, decreasing = T)[round(length(SuitabilityScores$layer)*.90,0)] 
	}else if(method == "50pct"){
		threshold <- sort(SuitabilityScores$layer, decreasing = T)[round(length(SuitabilityScores$layer)*.50,0)] 
	}
		
	if(output.type == "suitability.scores"){
		usr.raster[usr.raster < threshold] <- 0
		return(usr.raster)
	} else if(output.type == "binary"){
		#M <- c(0, threshold, 0,  threshold, 1, 1); 
		#rclmat <- matrix(M, ncol = 3, byrow = TRUE); 
		#Dist <- reclassify(x = usr.raster, rcl = rclmat);
		bin.raster <- usr.raster
		bin.raster[bin.raster < threshold] <- 0
		bin.raster[bin.raster >= threshold] <- 1
		bin.raster <- raster(bin.raster)
		return(bin.raster)
	}
}

## Further modified using: https://groups.google.com/g/Maxent/c/wj4UNTHwCf0 
ThresholdClimModel <- function(proj.raster,clim.raster, usr.occs, method, output.type){
	usr.raster <- terra::setMinMax(usr.raster)
	
	SuitabilityScores <- terra::extract(proj.raster, usr.occs) ## Terra now returns a data.frame as a default instead of a matrix, so we must change the approach to indexing into columns
	
	SuitabilityScores <- SuitabilityScores[complete.cases(SuitabilityScores), ]
	
	if(method == "MPT"){
		threshold <- min(SuitabilityScores)
	} else if(method == "95pct"){
		threshold <- sort(SuitabilityScores$layer, decreasing = T)[round(length(SuitabilityScores$layer)*.95,0)] 
	} else if(method == "90pct"){
		threshold <- sort(SuitabilityScores$layer, decreasing = T)[round(length(SuitabilityScores$layer)*.90,0)] 
	}
	
	if(output.type == "suitability.scores"){
		proj.raster[proj.raster < threshold] <- 0
		clim.raster[clim.raster < threshold] <- 0
		return(proj.raster)
		return(clim.raster)
	} else if(output.type == "binary"){
		#M <- c(0, threshold, 0,  threshold, 1, 1); 
		#rclmat <- matrix(M, ncol = 3, byrow = TRUE); 
		#Dist <- reclassify(x = usr.raster, rcl = rclmat);
		
		bin.proj.raster <- proj.raster
		bin.proj.raster <- bin.proj.raster >= threshold
		bin.proj.raster[bin.proj.raster < threshold] <- NA
		# bin.proj.raster[bin.proj.raster >= threshold] <- 1
		
		bin.clim.raster <- clim.raster
		bin.clim.raster <- bin.clim.raster >= threshold
		bin.clim.raster[bin.clim.raster < threshold] <- NA
		# bin.clim.raster[bin.clim.raster >= threshold] <- 1
		
		proj.cell.size <- area(bin.proj.raster, na.rm=TRUE, weights=FALSE)
		proj.area.present <- cellStats(proj.cell.size, sum)
		
		clim.cell.size <- area(bin.clim.raster, na.rm=TRUE, weights=FALSE)
		clim.area.present <- cellStats(clim.cell.size, sum)
		
		dif_area <- proj.area.present - clim.area.present
		
		return(bin.proj.raster)
		return(bin.clim.raster)
		return(dif_area)
	}
}