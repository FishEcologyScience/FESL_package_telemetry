#format map
HHmapFull <- readRDS("data-raw/files-raw/HHmapFull_2024-09-17.rds")


usethis::use_data(HHmapFull, overwrite = TRUE)

