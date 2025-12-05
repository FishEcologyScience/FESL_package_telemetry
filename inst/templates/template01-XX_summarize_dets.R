## --------------------------------------------------------------#
## Template: template01-XX_summarize_dets
## Title: Detection Data Summary and Quality Checks
##
## Purpose:
##    Comprehensive summary of acoustic telemetry detection data including
##    residency calculations, individual/population/location summaries, and
##    visualization plots.
##
## --------------------------------------------------------------#
## INPUTS:
##   data_det - Detection dataframe with GLATOS-formatted columns
##     Required columns:
##       - animal_id: Fish identifier
##       - station_no: Receiver station identifier
##       - detection_timestamp_est: Detection timestamp (POSIXct)
##       - deploy_lat: Receiver latitude (numeric)
##       - deploy_long: Receiver longitude (numeric)
##       - date: Detection date (Date)
##     Optional columns:
##       - transmitter_codespace: Transmitter manufacturer code
##       - transmitter_id: Transmitter identification number
##       - length: Fish length (mm)
##
## OUTPUTS:
##   df_residency - Residency times by date, animal, and station
##   df_individual_summary - Individual fish summaries (by date/station)
##     Columns: date, animal_id, station_no, dets_fish_station, dets_fish,
##              residence, deploy_lat, deploy_long, transmitter_codespace,
##              transmitter_id, length
##   df_population_summary - Population summaries (by date/station)
##     Columns: date, station_no, deploy_lat, deploy_long, dets, dets_total,
##              dets_prop, residence, residence_total, residence_prop,
##              fish_count, fish_count_total, fish_count_prop
##   df_location_summary - Location summaries (by station)
##     Columns: station_no, deploy_lat, deploy_long, dets_sum, dets_prop,
##              residence_mean, residence_sum
##   plots - List containing visualization objects:
##     $detections_by_fish - Ranked detections plot
##     $durations_by_fish - Ranked duration plot
##     $abacus - Spatiotemporal heatmap
##
## DEPENDENCIES:
##   Required packages:
##     - tidyverse (dplyr, ggplot2, forcats)
##     - data.table
##     - viridis
##     - FESLtelemetry
##
## --------------------------------------------------------------#
## Author: [Your Name]
##
## Date Created: [Date]
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#

# Load example data from FESLtelemetry package
#   data("example_data_raw_dets", package = "FESLtelemetry")
#   data_det <- example_data_raw_dets
#
# Or use your own data:
#   data_det <- your_detection_data

##### Build Individual-fish datasets ##############################----
#-------------------------------------------------------------#
cat("\n--- Building individual-level summaries ---\n")

###Summarize fish residency at stations
#----------------------------#
cat("Calculating fish movements and residency times...\n")

# Use FESLtelemetry package function to calculate residency
df_residency <- FESLtelemetry::calculate_residency(
  data = data_det,
  animal_col = "animal_id",
  station_col = "station_no",
  timestamp_col = "detection_timestamp_est",
  date_col = "date",
  lat_col = "deploy_lat",
  long_col = "deploy_long",
  units = "hours"
)

cat("  Residency calculated for", length(unique(df_residency$animal_id)), "fish\n")


###Build temporary data summaries
#----------------------------#
cat("Summarizing detection counts by fish and date...\n")

### Detections by animal and day
temp_data_daily <- data.table::data.table(data_det)
temp_data_daily <- temp_data_daily[, .(dets_fish=length(detection_timestamp_est)),
                                    keyby=c("date", "animal_id")]

# Plot: Detections by fish
plots$detections_by_fish <- temp_data_daily %>%
  group_by(animal_id) %>%
  summarize(dets_total = sum(dets_fish)) %>%
  ggplot(aes(y=fct_reorder(as.factor(animal_id), dets_total), x= dets_total))+
  geom_col()+
  ylab("Ranked fish identity")+
  xlab("Total detections in dataset")+
  theme_classic()

# Plot: Duration by fish
plots$durations_by_fish <- temp_data_daily %>%
  group_by(animal_id) %>%
  summarise(date_min = min(date),
            date_max = max(date),
            duration = difftime(date_max, date_min, units = "days")) %>%
  ggplot(aes(y=fct_reorder(as.factor(animal_id), duration), x= duration))+
  geom_col()+
  ylab("Ranked fish identity")+
  xlab("Total duration in dataset (days)")+
  theme_classic()

cat("  Detection plots created\n")

### Detections by animal, station, and day
temp_data_daily_station <- data.table::data.table(data_det)
temp_data_daily_station <- temp_data_daily_station[, .(dets_fish_station=length(detection_timestamp_est)),
                                                    keyby=c("date", "animal_id", "station_no")]

#Station key
df_key_station <- select(data_det, station_no, deploy_lat, deploy_long) %>%
  slice_head(by=station_no)

#Animal Key
temp_key_animal <- select(data_det, animal_id, transmitter_codespace, transmitter_id, length) %>%
  slice_head(by=animal_id)

###Combine datasets
#----------------------------#
cat("Combining datasets for individual-level summary...\n")

df_individual_summary <- temp_data_daily_station %>%
  #Add fish-level detection sums
  left_join(temp_data_daily, join_by(date, animal_id)) %>%
  #Add fish-station level residence times (from calculate_residency function)
  left_join(df_residency, join_by(date, animal_id, station_no)) %>%
  #Add station details
  left_join(df_key_station, join_by(station_no)) %>%
  #Add animal details
  left_join(temp_key_animal, join_by(animal_id)) %>%
  ungroup()

cat("  Individual summary complete:",
    format(nrow(df_individual_summary), big.mark=","), "rows\n")

#Clean-up
rm(list = ls(pattern="^temp_")) #Remove environment objects with 'temp_' prefix



##### Build Population-level datasets ############################----
#-------------------------------------------------------------#
cat("\n--- Building population-level summaries ---\n")

### Summarize main dataset
df_population_summary <- df_individual_summary %>%
  group_by(station_no, date) %>%
  summarize(dets = sum(dets_fish_station),
            residence = sum(residence),
            .groups = "drop") %>%
  mutate(residence = case_when(is.na(residence) ~ 0,
                               TRUE ~ residence)
  ) %>%
  ungroup()

### Count fish per receiver
temp_fishcount <- data.table::data.table(df_individual_summary)[, .(fish_count=length(unique(animal_id))),
                                                     keyby=c("date", "station_no")]

### Grab daily totals
temp_dailytotals <- data.table::data.table(df_individual_summary)[, .(dets_total = sum(dets_fish_station),
                                                           residence_total = sum(residence, na.rm=TRUE),
                                                           fish_count_total=length(unique(animal_id))
                                                         ),
                                                     keyby=c("date")]

###Combine datasets
#----------------------------#
df_population_summary <- df_population_summary %>%
  #Add station details
  left_join(df_key_station, join_by(station_no)) %>%
  #Add fishcounts
  left_join(temp_fishcount, join_by(date, station_no)) %>%
  #Add daily totals
  left_join(temp_dailytotals, join_by(date))

### Generate proportion data
df_population_summary <- df_population_summary %>%
  #Calculate proportions
  mutate(dets_prop = dets/dets_total,
         residence_prop = residence/residence_total,
         fish_count_prop = fish_count/fish_count_total) %>%
  #reorder columns
  select(date, station_no, deploy_lat, deploy_long,
         dets, dets_total, dets_prop,
         residence, residence_total, residence_prop,
         fish_count, fish_count_total, fish_count_prop)

cat("  Population summary complete:",
    format(nrow(df_population_summary), big.mark=","), "rows\n")



### Generate plots
#----------------------------#
cat("Generating population-level visualization...\n")

plots$abacus <- df_population_summary %>%
  #Sort station by longitude
  mutate(station_factor = fct_reorder(as.factor(station_no), deploy_long, .fun=median)) %>%
  ggplot(aes(x=date,
             y=station_factor,
             fill=residence_prop))+
  geom_tile()+
  geom_point(aes(size = dets_total), color = "black", alpha = 0.1) +  # overlay detection counts
  viridis::scale_fill_viridis_c(end=0.8)+
  labs(y = "Station (ordered by longitude)",
       x = "Date",
       fill = "Daily proportion\nof residence time",
       size = "Daily detections\nper receiver")+
  theme_classic()

cat("  Abacus plot created\n")

#Clean-up
rm(list = ls(pattern="^temp_")) #Remove environment objects with 'temp_' prefix



##### Build Location-level datasets ##############################----
#-------------------------------------------------------------#
cat("\n--- Building location-level summaries ---\n")

#Note: summing or averaging proportion data is not giving intuitive results. Avoiding for now.
df_location_summary <- df_population_summary %>%
  group_by(station_no) %>%
  summarize(deploy_lat = mean(deploy_lat),
            deploy_long = mean(deploy_long),
            dets_sum = sum(dets),
            dets_prop = dets_sum/sum(df_population_summary$dets, na.rm = TRUE),
            residence_mean = mean(residence),
            residence_sum = sum(residence),
            .groups = "drop")

cat("  Location summary complete:",
    format(nrow(df_location_summary), big.mark=","), "stations\n")



##### Summary Statistics ##########################################----
#-------------------------------------------------------------#
cat("\n--- SUMMARY STATISTICS ---\n")

cat("\n1. Dataset Overview\n")
cat("  Total detections:", format(nrow(data_det), big.mark=","), "\n")
cat("  Unique fish:", length(unique(data_det$animal_id)), "\n")
cat("  Unique stations:", length(unique(data_det$station_no)), "\n")
cat("  Date range:", format(min(data_det$date), "%Y-%m-%d"), "to",
    format(max(data_det$date), "%Y-%m-%d"), "\n")
cat("  Study duration:",
    difftime(max(data_det$date), min(data_det$date), units="days"), "days\n")

cat("\n2. Individual-level Summary\n")
temp_summary_fish <- df_individual_summary %>%
  group_by(animal_id) %>%
  summarize(
    total_dets = sum(dets_fish_station),
    total_residence = sum(residence, na.rm=TRUE),
    n_stations = length(unique(station_no)),
    n_days = length(unique(date))
  )
cat("  Mean detections per fish:",
    round(mean(temp_summary_fish$total_dets), 1),
    "± SD", round(sd(temp_summary_fish$total_dets), 1), "\n")
cat("  Mean stations per fish:",
    round(mean(temp_summary_fish$n_stations), 1),
    "± SD", round(sd(temp_summary_fish$n_stations), 1), "\n")

cat("\n3. Population-level Summary\n")
cat("  Mean daily detections:",
    round(mean(df_population_summary$dets), 1),
    "± SD", round(sd(df_population_summary$dets), 1), "\n")
cat("  Mean daily residence (hours):",
    round(mean(df_population_summary$residence), 1),
    "± SD", round(sd(df_population_summary$residence), 1), "\n")

cat("\n4. Location-level Summary\n")
cat("  Mean detections per station:",
    round(mean(df_location_summary$dets_sum), 1),
    "± SD", round(sd(df_location_summary$dets_sum), 1), "\n")
cat("  Range:", round(min(df_location_summary$dets_sum), 0), "-",
    round(max(df_location_summary$dets_sum), 0), "\n")

#Clean-up summary objects
rm(list = ls(pattern="^temp_summary_"))

cat("\n--- Analysis complete ---\n\n")



##### Optional: Export Data and Plots #############################----
#-------------------------------------------------------------#
# IMPORTANT: Following Paul's coding rules, file exports are commented out
# Uncomment these lines when you want to save outputs

# # Export individual summary
# write.csv(x = df_individual_summary,
#           file = "03_outputs/df_individual_summary.csv",
#           row.names = FALSE)
# saveRDS(object = df_individual_summary,
#         file = "03_outputs/df_individual_summary.rds")
#
# # Export population summary
# write.csv(x = df_population_summary,
#           file = "03_outputs/df_population_summary.csv",
#           row.names = FALSE)
# saveRDS(object = df_population_summary,
#         file = "03_outputs/df_population_summary.rds")
#
# # Export location summary
# write.csv(x = df_location_summary,
#           file = "03_outputs/df_location_summary.csv",
#           row.names = FALSE)
# saveRDS(object = df_location_summary,
#         file = "03_outputs/df_location_summary.rds")
#
# # Export plots
# ggsave("03_outputs/plot_detections_by_fish.png",
#        plot = plots$detections_by_fish,
#        width = 10, height = 6, units = "in", dpi = 300)
#
# ggsave("03_outputs/plot_durations_by_fish.png",
#        plot = plots$durations_by_fish,
#        width = 10, height = 6, units = "in", dpi = 300)
#
# ggsave("03_outputs/plot_abacus.png",
#        plot = plots$abacus,
#        width = 12, height = 8, units = "in", dpi = 300)
#
# cat("Data and plots exported to 03_outputs/\n")
