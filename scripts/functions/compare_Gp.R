compare.Gp <- function(
  Gp.one.binary,
  Gp.two.binary,
  name.Gp.one,
  name.Gp.two,
  plot.name,
  plot.outdir,
  threshold.type,
  ...
  ){
  
  require(biomod2,quietly = TRUE)
  require(terra,quietly = TRUE)
  require(ggpubr,quietly = TRUE)
  require(ggplot2,quietly = TRUE)
  require(viridis,quietly = TRUE)
  
  
  range <- BIOMOD_RangeSize(Gp.one.binary,Gp.two.binary)
  
  bm_PlotRangeSize(range,do.count = TRUE,do.perc = TRUE,do.maps = TRUE,do.plot = TRUE,row.names = c("layer"))
  
  rangerast <- raster(range$Diff.By.Pixel)
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
                      labels = c(paste0("invadable for ",name.Gp.one, " population"), "invadable for both populations", "invadable for neither population", paste0("invadable for ",name.Gp.two, " population")), name = "Habitat Suitability") +
    ggtitle(paste0("Projected Global Habitat Suitability Comparison - at a ",threshold.type," threshold - between Populations of C. sativa: ",name.Gp.one, " and ", name.Gp.two))+ 
    coord_sf(crs = crs(Gp.one.binary))
  ggsave(paste0(plot.name,".png"),	plot = comptest ,path = plot.outdir,	height = 10,	width = 20)
}
