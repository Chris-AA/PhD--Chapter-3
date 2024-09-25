# Syndrome classification
# Read data in. To be done on schools linux servers

#setwd("/exports/csce/datastore/geos/users/s1318698/ch1_rq4")

# Libraries
library(raster)
library(rgdal)
library(parallel)

# Load data----
indir <- file.path("..", "..","1_RQ5","1_inputs", "shapefiles")
proj_area <- readOGR(file.path(indir, "proj_area_34S.shp")) # shapefile of study area


indir <- file.path("..","..","ch1_rq4","data_in", "inputs_aligned")


L <- list()
L$def <- raster(file.path(indir,"defrate_aligned.tif"))
L$deg <- raster(file.path(indir,"degrate_aligned.tif"))
L$grow <- raster(file.path(indir,"growthrate_aligned.tif"))
L$cropreg <- raster(file.path(indir,"cropreg_rate_aligned.tif"))

L$grid10 <- raster(file.path(indir,"grid10_aligned.tif"))

# Crop and mask
C <- mclapply(L, crop, y = proj_area)
M <- mclapply(C, mask, mask = proj_area)


# Sample (check raster package functions: sampleStratified)
RS <- stack(M)# stack
#saveRDS(RS, "/exports/csce/datastore/geos/users/s1318698/Syndrome_class/syndrome_classification/data/RS.rds")
S <- sampleRegular(RS, size = 500000, ext = proj_area) # Sample raster stack systematically
df <- as.data.frame(S)
df <- na.omit(df)
df[1:4] <- df[1:4]/100

#saveRDS(df, "/exports/csce/datastore/geos/users/s1318698/ch1_rq4/df_syndrome2.csv")




