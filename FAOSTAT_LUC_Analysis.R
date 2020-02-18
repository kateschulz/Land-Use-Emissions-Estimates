library(tidyverse)
library(ggplot2)
library(wesanderson)

land_types = list("Forest","Crop","Grass")

for (i in seq(1, length(land_types))){
  
  # read in data for each land type
  d.frame <- read.csv(paste0("FAOSTAT_", land_types[i], ".csv"))
  
  # drop unnecessary columns
  d.frame = d.frame[ , which(names(d.frame) %in%
                                      c("Area", "Item","Year","Unit","Value"))]
  
  # aggregate values
  d.frame.agg = aggregate(Value ~ Item+Year+Unit, d.frame, sum)
  # convert gigagrams to gigatons and leave ha/1,000 ha for forest
  d.frame.agg.convert = d.frame.agg %>%
    mutate(Value = ifelse(Unit == "gigagrams", Value*0.000001,Value))
  # re-label gigagrams as gigtons
  levels(d.frame.agg.convert$Unit)[levels(d.frame.agg.convert$Unit) 
                                   =="gigagrams"] <- "gigatons"
  
  # split on gigatons since ha and 1,000 ha differ across dfs
  d.frame.ha = split(d.frame.agg.convert, 
                     d.frame.agg.convert$Unit!='gigatons')[['TRUE']]
  d.frame.gig = split(d.frame.agg.convert, 
                      d.frame.agg.convert$Unit)[['gigatons']]
  
  # join on year and item
  inner <- merge(d.frame.ha, d.frame.gig, by = c("Year", "Item"))
  # rename cols
  names(inner)[names(inner) == "Value.x"] <- "ha"
  names(inner)[names(inner) == "Value.y"] <- "gt"
  
  # compute gt/ha and fix forest in post-processing
  inner$gt_ha = inner$gt/inner$ha

  # convert year to numeric for graphing
  as.numeric(as.character(inner$Year))
  
  # re-name dfs 
  assign(unlist(land_types[i]), inner)
  }

# reset Forest 1,000 ha to ha
Forest$ha = Forest$ha*1000
Forest$gt_ha = Forest$gt_ha/1000

# re-label gigagrams as gigtons
levels(Forest$Unit.x)[levels(Forest$Unit.x) 
                                 =="1000 ha"] <- "ha"

# create final df
final = bind_rows(Forest, Grass, Crop)

write.csv(final, "forest_crop_grass.csv")

ggplot(final, aes(x = Year, y = gt_ha, color = Item, group = Item)) +
  theme_minimal() + geom_point() + geom_line()  +
  ggtitle(expression("Land Use Net Emissions by Year, 1990 - 2017")) +
  scale_x_continuous(name="Year", breaks = seq(1990, 2017, 9)) +
  scale_y_continuous(name = expression("Gigatons per Hectare")) +
  scale_color_manual(name = "Land Use",
                     values=wes_palette(n=3, name="Darjeeling1")) 








