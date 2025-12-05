# FESLtelemetry Analysis Templates

This directory contains template scripts for common acoustic telemetry analysis workflows.

## Using Templates

Templates can be copied to your project using the `use_template()` function:

```r
library(FESLtelemetry)

# List available templates
list_templates()

# Create a template in your project
use_template("summarize_dets")
```

## Available Templates

### template01-02_summarize_dets.R

**Purpose:** Comprehensive detection data summary and quality assessment

**What it does:**
- Calculates fish residency times at stations
- Generates individual-level summaries (per fish)
- Generates population-level summaries (per station and date)
- Generates location-level summaries (per station)
- Creates visualization plots (detections, durations, abacus plot)
- Provides summary statistics with means and standard deviations

**Expected data format:**
This template expects GLATOS-formatted detection data with standardized column names:
- `animal_id`: Fish identifier
- `station_no`: Receiver station identifier
- `detection_timestamp_est`: Detection timestamp
- `deploy_lat`: Receiver latitude
- `deploy_long`: Receiver longitude
- `date`: Detection date
- `transmitter_codespace`, `transmitter_id`, `length`: Fish metadata

**Required packages:**
- tidyverse (dplyr, ggplot2, forcats)
- data.table
- viridis
- FESLtelemetry

**Example data:**
The template includes instructions for loading example data:
```r
data("script01_01_import_telemetry_data", package = "FESLtelemetry")
data_det <- script01_01_import_telemetry_data
```

**Outputs:**
- `df_individual_summary`: Individual fish detection summaries
- `df_population_summary`: Population-level daily summaries
- `df_location_summary`: Station-level summaries
- `plots$detections_by_fish`: Ranked detections plot
- `plots$durations_by_fish`: Ranked duration plot
- `plots$abacus`: Spatiotemporal heatmap

## Template Naming Convention

Templates follow the FESL naming convention:

`templateXX-YY_description.R`

Where:
- `XX` = Template class (01 = data processing, 02 = analysis, 03 = visualization)
- `YY` = Template number within class
- `description` = Brief description of template purpose

## Customizing Templates

After copying a template to your project:

1. Update the header (Author, Date, Modification Notes)
2. Adjust parameters and filters for your analysis
3. Uncomment export sections when ready to save outputs
4. Modify plots and summaries as needed

## Contributing Templates

To add a new template to the package:

1. Create the template script following naming conventions
2. Save to `inst/templates/`
3. Update `use_template()` function in `R/use_template.R`
4. Update `list_templates()` with description
5. Add documentation to this README
