## SDM Thresholding Function
## Authored by: Cecina Babich Morrow
## Date: April 12th 2019
## Title: Thresholding species distribution models
## Link: https://babichmorrowc.github.io/post/2019-04-12-sdm-threshold/
## Citation: Morrow, C. B.(2019, April 12) Thresholding species distribution models, github.io, https://babichmorrowc.github.io/post/2019-04-12-sdm-threshold/.
## Modified on 15-03-2023 by Tori M. Ford

library(raster)
## Warning: package 'raster' was built under R version 3.5.2
## Loading required package: sp
## Warning: package 'sp' was built under R version 3.5.2

sdm_threshold <- function(sdm, occs, type = "mtp", binary = FALSE){
	occPredVals <- raster::extract(sdm, occs)
	if(type == "mtp"){
		thresh <- min(na.omit(occPredVals))
	} else if(type == "p10"){
		if(length(occPredVals) < 10){
			p10 <- floor(length(occPredVals) * 0.9)
		} else {
			p10 <- ceiling(length(occPredVals) * 0.9)
		}
		thresh <- rev(sort(occPredVals))[p10]
	}
	sdm_thresh <- sdm
	sdm_thresh[sdm_thresh < thresh] <- 0 ## Originally NA, Changed for the Sake of Mapping Clarity
	if(binary){
		sdm_thresh[sdm_thresh >= thresh] <- 1
	}
	return(sdm_thresh)
}