maxent.csv <- function(
					in.dir,
					regions,
					out.csv,
					opt.dist,
					...
) {
	
	require(dplyr,quietly = TRUE)
	
	for (r in 1:length(regions)) {
		region <- regions[[r]]

		path = paste0(sample.dir,region,"/")
		print(path)

		df <- read.csv(file = paste0(in.dir,region,"/",region,"_accessions_",opt.dist,"km.csv"))

		if (aggregate.model == TRUE) {
			df$TOTAL <- "Total"
		}
		
		write.csv(df, paste0(in.dir,region,"/",out.csv),row.names = FALSE)
		
	}
	
}



aggregate.csv <- function(total.file,
													out.csv){
	
	
	
	
}
