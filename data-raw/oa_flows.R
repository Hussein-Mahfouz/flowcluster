library(tidyverse)
library(geographr)
library(od)
library(sf)

# ----- Loading datasets

# 1. census 2021 od data
url <- "https://www.nomisweb.co.uk/output/census/2021/odwp01ew.zip"
zip_path <- "data-raw/odwp01ew.zip"

if (!file.exists(zip_path)) {
  download.file(url, zip_path, mode = "wb")
}

zip_contents <- unzip(zip_path, list = TRUE)
print(zip_contents$Name)

target_file <- "ODWP01EW_MSOA.csv"  # Replace with the actual filename
unzip(zip_path, files = target_file, exdir = "data-raw")

msoa_flows <- read_csv(file.path("data-raw", target_file))

# 2. OA shapefile 
msoa_boundaries <- geographr::boundaries_msoa21 
# remove unnecessary columns
msoa_boundaries = msoa_boundaries |>
  select(msoa21_code, geometry)


# 3. OA to MSOA to Region lookup 
lookup = geographr::lookup_postcode_oa_lsoa_msoa_ltla_2025

lookup = lookup |>
  select(msoa21_code, ltla24_code) |>
  distinct(msoa21_code, .keep_all = TRUE) 


# ----- Preprocess

# keep only people commuting in the UK
msoa_flows = msoa_flows |>
  filter(`Place of work indicator (4 categories) label` == "Working in the UK but not working at or from home")

# keep only necessary columns 
msoa_flows = msoa_flows |>
  select(`Middle layer Super Output Areas code`, `MSOA of workplace code`, Count) |>
  rename(
    origin = `Middle layer Super Output Areas code`,
    destination = `MSOA of workplace code`,
    count = Count
  )

# join lookup table by origin and then destination
msoa_flows = msoa_flows |>
  left_join(lookup, by = c("origin" = "msoa21_code")) |>
  rename(ltla24_code_origin = ltla24_code) |>
  left_join(lookup, by = c("destination" = "msoa21_code")) |>
  rename(ltla24_code_destination = ltla24_code) 


# Leeds LTLA code: E08000035
msoa_flows_leeds = msoa_flows |>
  filter(ltla24_code_origin == "E08000035", ltla24_code_destination == "E08000035") |>
  select(-c(ltla24_code_origin, ltla24_code_destination)) 

# Keep only boundaries of MSOAs in the flows
msoa_boundaries = msoa_boundaries |>
  filter(msoa21_code %in% unique(c(msoa_flows_leeds$origin, msoa_flows_leeds$destination)))


# use od package to create od object using msoa_flows and zones layer
msoa_flows_leeds_sf = od::od_to_sf(x = msoa_flows_leeds, z = msoa_boundaries)

# jitter
# TODO





# ----- Save cleaned data to data/ folder for package use
#usethis::use_data(raw_data, overwrite = TRUE)