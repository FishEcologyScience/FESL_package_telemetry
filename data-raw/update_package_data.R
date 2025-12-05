## --------------------------------------------------------------#
## Script name: update_package_data.R
##
## Purpose of script:
##    Subsample and rename package data objects
##    - Subsample detections to first 100,000 rows
##    - Rename objects to follow new naming convention
##    - Save as .rda files for package distribution
##
## Author: Paul Bzonek [Claude]
##
## Date Created: 2024-12-05
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#

library(tidyverse)

# Load existing data
#----------------------------#
cat("Loading existing data objects...\n")
load("data/data_det_raw.rda")
load("data/HH_shapefile_WGS84.rda")
load("data/HHmapFull.rda")

# Subsample detections to first 100,000 rows
#----------------------------#
cat("Subsampling detections to 100,000 rows...\n")
cat("  Original size:", format(nrow(data_det_raw), big.mark = ","), "rows\n")

example_data_raw_dets <- data_det_raw %>%
  slice_head(n = 100000)

cat("  New size:", format(nrow(example_data_raw_dets), big.mark = ","), "rows\n")

# Rename shapefile object
#----------------------------#
cat("Renaming shapefile object...\n")
shapefile_HH_WGS84 <- HH_shapefile_WGS84

# Rename basemap object
#----------------------------#
cat("Renaming basemap object...\n")
basemap_HH <- HHmapFull

# Save new data objects
#----------------------------#
cat("\nSaving new data objects with compression...\n")

usethis::use_data(example_data_raw_dets,
                  overwrite = TRUE,
                  compress = "xz")
cat("  Saved: example_data_raw_dets.rda\n")

usethis::use_data(shapefile_HH_WGS84,
                  overwrite = TRUE,
                  compress = "xz")
cat("  Saved: shapefile_HH_WGS84.rda\n")

usethis::use_data(basemap_HH,
                  overwrite = TRUE,
                  compress = "xz")
cat("  Saved: basemap_HH.rda\n")

# Report file sizes
#----------------------------#
cat("\nNew data file sizes:\n")
files <- list.files("data", pattern = "\\.rda$", full.names = TRUE)
sizes <- file.info(files)
sizes$file <- basename(rownames(sizes))
sizes <- sizes[order(-sizes$size), c("file", "size")]
sizes$size_mb <- round(sizes$size / 1024^2, 2)

for (i in 1:nrow(sizes)) {
  cat("  ", sizes$file[i], ":", sizes$size_mb[i], "MB\n")
}

cat("\nData update complete!\n")
cat("\nNext steps:\n")
cat("1. Delete old data files:\n")
cat("   - data/data_det_raw.rda\n")
cat("   - data/HH_shapefile_WGS84.rda\n")
cat("   - data/HHmapFull.rda\n")
cat("2. Run: devtools::document()\n")
cat("3. Run: devtools::check()\n")
