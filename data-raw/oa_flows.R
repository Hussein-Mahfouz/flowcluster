library(tidyverse)

# ----- Loading datasets

# 1. census 2021 od data
url <- "https://www.nomisweb.co.uk/output/census/2021/odwp01ew.zip"
zip_path <- "data-raw/odwp01ew.zip"

if (!file.exists(zip_path)) {
  download.file(url, zip_path, mode = "wb")
}

zip_contents <- unzip(zip_path, list = TRUE)
print(zip_contents$Name)

target_file <- "ODWP01EW_OA.csv"  # Replace with the actual filename
unzip(zip_path, files = target_file, exdir = "data-raw")

oa_flows <- read_csv(file.path("data-raw", target_file))

# 2. OA shapefile 
# TODO

# 3. OA to MSOA to Region lookup 
# TODO

# ----- Preprocess

# keep only people commuting in the UK
oa_flows = oa_flows |>
  filter(`Place of work indicator (4 categories) label` == "Working in the UK but not working at or from home")

# keep only necessary columns 
oa_flows = oa_flows |>
  select(`Output Areas code`, `OA of workplace code`, Count) |>
  rename(
    origin = `Output Areas code`,
    destination = `OA of workplace code`,
    count = Count
  )

# join lookup table to filter by region
# TODO

# use od package to create od object using oa_flows and zones layer
# TODO





# ----- Save cleaned data to data/ folder for package use
usethis::use_data(raw_data, overwrite = TRUE)