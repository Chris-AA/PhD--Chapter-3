# Syndrome Classification
# Fit random forests model to classify land use syndromes

library(randomForest)

df <- readRDS(file.path("data","df_syndrome2.csv"))
#df$grid10 <- as.factor(df$grid10) # factorise for classification

# Sample data and balance based on grid10
#df_sample <- droplevels(df[sample(nrow(df), 10000), ]) # randomly sample rows and drop unused factor levels
df_sample <- dplyr::bind_rows(lapply(split(df, df$grid10), function(x) x[sample(nrow(x), 1),]))

# Fit model
rf <- randomForest(formula = grid10~., data = df_sample, ntry = 100,ntree=500, importance = TRUE, regression = TRUE)

# Assess model
rf
plot(rf)
varImpPlot(rf)

# Load raster stack from '0.1_read_data_in.R'
RS <- readRDS(file.path("data", "RS.rds"))
RS <- RS[[1:4]] # Remove grid10 from stack

# Predict raster with model
rf_ras <- raster::predict(model = rf, object = RS)
#writeRaster(rf_ras, "/exports/csce/datastore/geos/users/s1318698/ch1_rq4/rf2_raster.tif", format= "GTiff")
