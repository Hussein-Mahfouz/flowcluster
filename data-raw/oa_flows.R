library(tidyverse)
pak::pak("humaniverse/geographr")
library(geographr)
library(od)
library(sf)

# ---------------------
# 1. Download and unzip OD flow data
# ---------------------
zip_url <- "https://www.nomisweb.co.uk/output/census/2021/odwp01ew.zip"
zip_file <- "data-raw/odwp01ew.zip"
target_file <- "ODWP01EW_MSOA.csv"

# Download only if file doesn't exist
if (!file.exists(zip_file)) {
  download.file(zip_url, zip_file, mode = "wb")
}

# Check what files are in the zip
zip_contents <- unzip(zip_path, list = TRUE)
print(zip_contents$Name)

# Unzip only the required CSV
unzip(zip_file, files = target_file, exdir = "data-raw")

# Load OD flow data (Census 2021, MSOA level)
od_raw <- read_csv(file.path("data-raw", target_file))

# ---------------------
# 2. Load spatial and lookup data
# ---------------------

# Load MSOA 2021 boundaries (geometry only)
msoa_geom <- geographr::boundaries_msoa21 |>
  select(msoa21_code, geometry)

# Load MSOA-to-LTLA lookup table
lookup <- geographr::lookup_postcode_oa_lsoa_msoa_ltla_2025 |>
  select(msoa21_code, ltla24_code) |>
  distinct(msoa21_code, .keep_all = TRUE)

# ---------------------
# 3. Preprocess OD flows
# ---------------------

# Filter to include only workers commuting within the UK (exclude home workers)
flows <- od_raw |>
  filter(`Place of work indicator (4 categories) label` ==
    "Working in the UK but not working at or from home") |>
  select(
    origin = `Middle layer Super Output Areas code`,
    destination = `MSOA of workplace code`,
    count = Count
  )

# Join lookup to add LTLA codes for origin and destination
flows <- flows |>
  left_join(lookup, by = c("origin" = "msoa21_code")) |>
  rename(origin_ltla = ltla24_code) |>
  left_join(lookup, by = c("destination" = "msoa21_code")) |>
  rename(dest_ltla = ltla24_code)

# ---------------------
# 4. Subset for Leeds only
# ---------------------

# Leeds LTLA code: E08000035
flows_leeds <- flows |>
  filter(origin_ltla == "E08000035", dest_ltla == "E08000035") |>
  select(origin, destination, count)

# Subset MSOA boundaries used in Leeds flows
zones_leeds <- msoa_geom |>
  filter(msoa21_code %in% c(flows_leeds$origin, flows_leeds$destination))

# ---------------------
# 5. Convert OD table to spatial lines using `od` package
# ---------------------
flows_leeds <- od::od_to_sf(flows_leeds, zones_leeds)

# ---------------------
# 6. Jitter OD data
# ---------------------
# TODO

# ---------------------
# 6. Save dataset for internal or exported use
# ---------------------
usethis::use_data(flows_leeds, overwrite = TRUE)

# Check file size
fs::file_size("data/flows_leeds.rda")
