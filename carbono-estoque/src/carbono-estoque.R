# https://r-spatial.github.io/rgee/
# install.packages("geojson", dependencies = TRUE)
if (!require("rgee")) {
  install.packages("rgee")
  rgee::ee_install() # python
}
if (!require("reticulate")) {
  install.packages("reticulate", dependencies = TRUE)
}

# 1. Initialize the Python Environment
ee_Initialize()

# 2. Install geemap in the same Python ENV that use rgee
# reticulate::py_install("geemap")
# gm <- reticulate::import("geemap")
# ee_Initialize()
ee_install_upgrade()

if (!require("sf")) {
  install.packages("sf")
}

# Load FEBR snapshot
febr_data <- read.table("~/ownCloud/febr-repo/publico/febr-superconjunto.txt", dec = ",", sep = ";",
  header = TRUE)
colnames(febr_data)
head(febr_data)
febr_data <- febr_data[!is.na(febr_data$coord_x) & !is.na(febr_data$coord_y), ]
col_names <- c("dataset_id", "observacao_id", "coord_x", "coord_y", "observacao_data")
febr_data <- unique(febr_data[, col_names])
febr_data <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4623)

# Sample Covariates
## MapBiomas 60 v1
mapbiomas60v1 <-
  "projects/mapbiomas-workspace/public/collection6/mapbiomas_collection60_integration_v1"
land_use <- rgee::ee$Image(mapbiomas60v1)
idx <- sample(seq_len(nrow(febr_data)), 10)
febr_land_use <- rgee::ee_extract(x = land_use, y = febr_data[idx, ], sf = TRUE)
write.table(febr_land_use, file = "time/data/mapbiomas60v1.txt",
  sep = "\t", dec = ",", row.names = FALSE)

# Train Random Forest Regression model
formula <- stock ~ 1
split.select.weights <- NULL
always.split.variables <- NULL
random_forest <- ranger::ranger(
  formula = formula,
  data = NULL,
  num.trees = 500,
  importance = "impurity",
  split.select.weights = split.select.weights,
  always.split.variables = always.split.variables
)