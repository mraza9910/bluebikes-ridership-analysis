############################################################
# Title: Blue Bike Study
# Author: Maida Raza
# Course: DATA200
# Date: 2025-11-26
# Data Source: https://s3.amazonaws.com/hubway-data/index.html. 
############################################################

############################################################
# LOAD REQUIRED LIBRARIES
############################################################

library(dplyr)
library(readxl)
library(gridExtra)
library(grid)     # needed for grid.draw()
library(stringr)
library(purrr)

############################################################
# LOAD BLUEBIKES DATASET
############################################################

data <- read.csv(
  '/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/bluebikes-tripdata_2022/bluebikes_tripdata_all_months.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)
View(data)
nrow(data)
# Preview first few rows
head(data)

# View full dataset in RStudio
View(data)

# Display variable names
names(data)

############################################################
# CREATE VARIABLE TYPE TABLE 
############################################################

variable_types <- data.frame(
  variable = names(data),
  type = sapply(data, class),
  stringsAsFactors = FALSE
)

# Add units (in exact order of names(data))
variable_types$unit <- c(
  "seconds",                     # tripduration
  "yyyy-mm-dd hh:mm:ss",         # starttime
  "yyyy-mm-dd hh:mm:ss",         # stoptime
  "Integer",                     # start.station.id
  "Text",                        # start.station.name
  "Decimal degrees",             # start.station.latitude
  "Decimal degrees",             # start.station.longitude
  "Integer",                     # end.station.id
  "Text",                        # end.station.name
  "Decimal degrees",             # end.station.latitude
  "Decimal degrees",             # end.station.longitude
  "Integer",                     # bikeid
  "Subscriber/ Customer",        # usertype
  "5 digit zip code"             # postal.code
)

# Add descriptions
variable_types$description <- c(
  "Total length of bike trips",
  "Date and time when the ride started",
  "Date and time when the ride ended",
  "Unique identifier assigned to the bike station where the trip started",
  "Starting bike station name",
  "Latitude coordinate of the trip's starting station",
  "Longitude coordinate of the trip's starting station",
  "Unique identifier assigned to the bike station where the trip ended",
  "Ending bike station name",
  "Latitude coordinate of the trip’s ending station",
  "Longitude coordinate of the trip’s ending station",
  "Unique ID number assigned to each bike used in the rental system",
  "Subscriber: Long term registered member; Customer: Short term casual rider",
  "Indicates the postal code of the start or end area"
)

# CAPITALIZE COLUMN HEADERS
names(variable_types) <- toupper(names(variable_types))

variable_types   # Preview in console

############################################################
# EXPORT VARIABLE TYPE TABLE AS PDF (TIMES NEW ROMAN)
############################################################

# Create formatted table grob
tbl <- tableGrob(
  variable_types,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(cex = 0.9, fontfamily = "Times"),
      # left-align all table body cells
      hjust = 0, 
      x = 0
    ),
    colhead = list(
      fg_params = list(cex = 1.2, fontface = "bold", fontfamily = "Times"),
      # center-align column headers
      hjust = 0.5,
      x = 0.5
    )
  )
)

# Save PDF using Times New Roman
pdf(
  "/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Exhibits/variable_types.pdf",
  width = 15,
  height = 20,
  family = "Times"
)

par(family = "Times")  # ensure text inside PDF is Times New Roman
grid.draw(tbl)
dev.off()

cat("PDF saved to: /Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Exhibits/variable_types.pdf\n")



############################################################
# CHECK KEY VARIABLES
############################################################

unique(data$postal.code)
sum(data$postal.code == "")  # 465,637 missing ZIP codes

unique(data$tripduration)
unique(data$starttime)
unique(data$stoptime)
unique(data$usertype)
unique(data$end.station.id)

############################################################
# CHECK OTHER TYPES OF MISSING DATA
############################################################

# Define all types of missing-like values
nil <- c("", " ", "NA", "na", "NULL", "Null", "null", "\\N")

# Count missing per column
missing_counts <- sapply(data, function(column){
  sum(is.na(column) | trimws(as.character(column)) %in% nil)
})

missing_counts
# postal.code has major missingness — important for later ZIP-based analysis.

############################################################
#4. CHECK DUPLICATES USING SPECIFIC FEATURES
# Trip Duration; Start & Stop Times;
# Start & End Station Latitude & Longitude;
# User Type; Postal Code
############################################################

cat("------------------------------------------------------------\n")
cat(" DUPLICATE ANALYSIS: SPECIFIED FEATURES ONLY\n")
cat("------------------------------------------------------------\n")

# Create a subset with only the specified columns
dup_features <- data %>%
  dplyr::select(
    tripduration,
    starttime,
    stoptime,
    start.station.latitude,
    start.station.longitude,
    end.station.latitude,
    end.station.longitude,
    usertype,
    postal.code
  )

# Identify duplicates
dup_flag_specific <- duplicated(dup_features)
dup_count_specific <- sum(dup_flag_specific)
dup_percent_specific <- round(100 * dup_count_specific / nrow(data), 4)

cat("Number of duplicates based on specified features: ", dup_count_specific, "\n")
cat("Percent of dataset duplicated (specified features): ", dup_percent_specific, "%\n\n")

# Extract duplicated rows (preview only, not deleting)
duplicates_specific_df <- data[dup_flag_specific, ]

cat("Preview of duplicates (first 10 rows):\n")
print(head(duplicates_specific_df, 10))

# View(duplicates_specific_df)

cat("\nDuplicate check (based on selected features) complete. No rows removed.\n")
cat("------------------------------------------------------------\n\n")


############################################################
# EXTRACT UNIQUE STATION COORDINATES (start + end)
############################################################
remove.packages("ggmap")
install.packages('ggmap')
library(ggmap)
install.packages("stringr")
exists("register_google")


register_google(key="AIzaSyDbmkpyGoT8DszPKyouul9ja6umllwDYU8")



unique_start <- data %>%
  dplyr::select(
    station = start.station.name,
    lat = start.station.latitude,
    lon = start.station.longitude
  ) %>%
  dplyr::distinct()

unique_end <- data %>%
  dplyr::select(
    station = end.station.name,
    lat = end.station.latitude,
    lon = end.station.longitude
  ) %>%
  dplyr::distinct()

stations_unique <- dplyr::bind_rows(unique_start, unique_end) %>%
  dplyr::distinct()

# View in RStudio
View(stations_unique)

# Save to Analysis - Datasets folder
write.csv(
  stations_unique,
  '/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/stations_unique.csv',
  row.names = FALSE
)

cat("Saved: stations_unique.csv\n")


############################################################
# EVERSE GEOCODE UNIQUE STATIONS → ZIP (GOOGLE API)
############################################################

# Function to extract ZIP code from Google address
get_zip <- function(lat, lon) {
  
  result <- tryCatch(
    {
      ggmap::geocode(
        location = paste0(lat, ",", lon),
        output = "more",
        source = "google"
      )
    },
    error = function(e) return(NA)
  )
  
  # If Google returned an address field:
  if (!is.null(result) && "address" %in% names(result)) {
    
    # Extract 5-digit ZIP
    zip <- stringr::str_extract(result$address, "\\b\\d{5}\\b")
    return(zip)
    
  } else {
    return(NA)
  }
}

# Apply ZIP extraction
stations_zip <- stations_unique %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    postalcode = get_zip(lat, lon)
  ) %>%
  dplyr::ungroup()

# Preview ZIP results
head(stations_zip)
View(stations_zip)

nrow(stations_zip)

############################################################
# CHECK ALL TYPES OF MISSING VALUES IN postalcode
############################################################

nil_patterns <- c("", " ", "NA", "N/A", "na", "null", "NULL", "\\N")

# Convert to character, trim whitespace, uppercase
postal_clean <- toupper(trimws(as.character(stations_zip$postalcode)))

# Logical TRUE/FALSE for missing
is_missing_zip <- is.na(stations_zip$postalcode) | postal_clean %in% toupper(nil_patterns)

# Counts
total_missing_zip <- sum(is_missing_zip)
total_observations <- length(postal_clean)
percent_missing <- round(100 * total_missing_zip / total_observations, 2)

# Print results
cat("Total missing ZIPs:", total_missing_zip, "\n")
cat("Total observations:", total_observations, "\n")
cat("Percent missing:", percent_missing, "%\n")

# None missing here!

############################################################
# ADD START + END ZIP CODES USING stations_zip LOOKUP
############################################################

# --- Merge START station ZIP ---
data_with_zip <- data %>%
  dplyr::left_join(
    stations_zip,
    by = c(
      "start.station.name"      = "station",
      "start.station.latitude"  = "lat",
      "start.station.longitude" = "lon"
    )
  ) %>%
  dplyr::rename(start_postal = postalcode)  


# --- Merge END station ZIP ---
data_with_zip <- data_with_zip %>%
  dplyr::left_join(
    stations_zip,
    by = c(
      "end.station.name"        = "station",
      "end.station.latitude"    = "lat",
      "end.station.longitude"   = "lon"
    )
  ) %>%
  dplyr::rename(end_postal = postalcode)     # NEW COLUMN


############################################################
# VALIDATION — CHECK ROW MATCH AND PREVIEW RESULTS
############################################################

cat("Rows before ZIP merge: ", nrow(data), "\n")
cat("Rows after ZIP merge:  ", nrow(data_with_zip), "\n")

# Preview only the ZIP columns + station names
head(
  data_with_zip %>%
    dplyr::select(
      start.station.name, start_postal,
      end.station.name,   end_postal,
      postal.code        
    )
)

############################################################
# SAVE PRIMARY (RAW) BLUEBIKES DATASET AS INTERIM
############################################################

write.csv(
  data_with_zip,
  '/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/bluebikes_interim.csv',
  row.names = FALSE
)

cat("Interim dataset saved to: Analysis - Datasets/bluebikes_interim.csv\n")

############################################################
# RELOAD INTERIM DATASET (IMPORTANT FOR REPRODUCIBILITY)
############################################################

interim_data <- read.csv(
  '/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/bluebikes_interim.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)
nrow(interim_data)
View(interim_data)

print(sapply(interim_data, class))

# Ensure postal codes are numeric or character before padding
interim_data$start_postal <- sprintf("%05s", interim_data$start_postal)
interim_data$end_postal   <- sprintf("%05s", interim_data$end_postal)

library(knitr)
library(tidyverse)

# Define all missing-like values
nil_vals <- c("", " ", "NA", "na", "N/A", "NULL", "Null", "null", "\\N")

# ZIP is valid if it begins with 01 or 02
valid_zip <- function(x) {
  grepl("^(01|02)", x)
}

# Create the 5 categories
interim_data$postal_source <- case_when(
  
  # 1. True missing or missing-like
  is.na(interim_data$postal.code) |
    trimws(interim_data$postal.code) %in% nil_vals ~ "Missing",
  
  # 2. Invalid ZIP (does NOT start with 01 or 02)
  !valid_zip(interim_data$postal.code) ~ "Invalid and Unmatched",
  
  # 3. Valid + matches start_postal
  valid_zip(interim_data$postal.code) &
    interim_data$postal.code == interim_data$start_postal ~ "Start Station ZIP",
  
  # 4. Valid + matches end_postal
  valid_zip(interim_data$postal.code) &
    interim_data$postal.code == interim_data$end_postal ~ "End Station ZIP",
  
  # 5. Valid + matches neither
  valid_zip(interim_data$postal.code) ~ "Valid but Unmatched",
  
  TRUE ~ "Invalid and Unmatched"
)

# Summary table
postal_summary <- interim_data %>%
  count(postal_source) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 2)
  ) %>%
  rename(
    `Postal Source` = postal_source,
    `Count` = n,
    `Percent (%)` = Percent
  ) %>%
  arrange(factor(`Postal Source`,
                 levels = c("Start Station ZIP",
                            "End Station ZIP",
                            "Valid but Unmatched",
                            "Invalid and Unmatched",
                            "Missing")))

# Print in nice table
kable(
  postal_summary,
  caption = "Table: Postal Code Classification (5 Categories)",
  align = "lcc"
)
# Replace postal.code only for Invalid and Missing categories
interim_data <- interim_data %>%
  mutate(
    postal.code_cleaned = case_when(
      postal_source %in% c("Invalid and Unmatched", "Missing") ~ start_postal,
      TRUE ~ postal.code
    )
  )

library(lubridate)

# 1. Convert starttime and stoptime from character → POSIXct (with milliseconds)
interim_data <- interim_data %>%
  mutate(
    starttime_dt = ymd_hms(starttime, tz = "America/New_York"),
    stoptime_dt  = ymd_hms(stoptime,  tz = "America/New_York")
  )

# 2. Extract date and time into separate columns
interim_data <- interim_data %>%
  mutate(
    start_date = as.Date(starttime_dt),
    stop_date  = as.Date(stoptime_dt),
    
    start_time = format(starttime_dt, "%H:%M:%S"),
    stop_time  = format(stoptime_dt, "%H:%M:%S")
  )

# 3. Compute new trip duration in seconds
interim_data <- interim_data %>%
  mutate(
    trip_duration_calculated = as.numeric(stoptime_dt - starttime_dt, units = "secs")
  )

head(
  interim_data %>%
    select(tripduration, trip_duration_calculated)
)

names(interim_data)

write.csv(
  interim_data,
  "/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/bluebikes_cleaned.csv",
  row.names = FALSE
)

cat("Cleaned dataset saved to: Analysis - Datasets/bluebikes_cleaned.csv\n")

############################################################
# Analysis
############################################################

############################################################
# LOAD + CLEAN ZIP CODE DATABASE (USPS DATA)
############################################################
library(readxl)
zip_data <- read_excel('/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/ZIP_Locale_Detail.xls')
print(sapply(zip_data, class))
nrow(zip_data)

# Define all forms of missing-like values
nil <- c("", " ", "NA", "na", "NULL", "Null", "null", "\\N")

############################################################
# STANDARDIZE ZIP + CITY + STATE FIELDS
############################################################

zip_data <- zip_data %>%
  mutate(
    `PHYSICAL STATE` = trimws(as.character(`PHYSICAL STATE`)),
    `PHYSICAL CITY`  = trimws(as.character(`PHYSICAL CITY`)),
    `PHYSICAL ZIP`   = trimws(as.character(`PHYSICAL ZIP`))
  )

# Convert TRUE missing-like values to NA
zip_data <- zip_data %>%
  mutate(
    `PHYSICAL STATE` = ifelse(`PHYSICAL STATE` %in% nil, NA, `PHYSICAL STATE`),
    `PHYSICAL CITY`  = ifelse(`PHYSICAL CITY`  %in% nil, NA, `PHYSICAL CITY`),
    `PHYSICAL ZIP`   = ifelse(`PHYSICAL ZIP`   %in% nil, NA, `PHYSICAL ZIP`)
  )

############################################################
# FILTER MASSACHUSETTS ZIP RECORDS + CHECK MISSINGNESS
############################################################

zip_data_MA <- zip_data %>%
  filter(`PHYSICAL STATE` == "MA")
nrow(zip_data_MA)

# Check missing STATE and ZIP entries (after normalization)
cat("Missing PHYSICAL STATE: ", sum(is.na(zip_data_MA$`PHYSICAL STATE`)), "\n")
cat("Missing PHYSICAL ZIP:   ", sum(is.na(zip_data_MA$`PHYSICAL ZIP`)), "\n")
cat("Missing PHYSICAL CITY:  ", sum(is.na(zip_data_MA$`PHYSICAL CITY`)), "\n")

# View cleaned MA subset
# View(zip_data_MA)

############################################################
# COLLAPSE ZIP DATA → UNIQUE ZIP → CITY MAPPING
############################################################

zip_city_lookup <- zip_data_MA %>%
  group_by(`PHYSICAL ZIP`) %>%
  summarise(
    PHYSICAL_CITY = paste(unique(`PHYSICAL CITY`), collapse = " / "),
    .groups = "drop"
  )

View(zip_city_lookup)
nrow(zip_city_lookup)   # should be 517

write.csv(
  zip_city_lookup,
  "/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/zip_city_lookup.csv",
  row.names = FALSE
)

cat("Saved: zip_city_lookup.csv\n")


############################################################
# FINAL BLUEBIKES + ZIP MERGE USING CLEANED DATASET
############################################################

library(dplyr)

############################################################
# LOAD CLEANED BLUEBIKES DATASET
############################################################

bluebikes_cleaned <- read.csv(
  "/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/bluebikes_cleaned.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

############################################################
# LOAD THE DEDUPED ZIP → CITY LOOKUP (ONE ROW PER ZIP)
############################################################

zip_city_lookup <- read.csv(
  "/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/zip_city_lookup.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)
View(bluebikes_cleaned)
names(bluebikes_cleaned)
View(zip_city_lookup)

head(zip_city_lookup)
names(zip_city_lookup)


############################################################
# STANDARDIZE ZIP FORMATS
############################################################

library(dplyr)
library(stringr)

bluebikes_cleaned <- bluebikes_cleaned %>%
  mutate(
    # 1. Create padded versions of start/end ZIPs FIRST
    start_zip = if_else(!is.na(start_postal),
                        sprintf("%05s", as.character(start_postal)),
                        NA_character_),
    end_zip = if_else(!is.na(end_postal),
                      sprintf("%05s", as.character(end_postal)),
                      NA_character_),
    
    # 2. Fix postal.code ONLY for Missing OR Invalid cases *using padded zips*
    postal.code = case_when(
      postal_source %in% c("Invalid and Unmatched", "Missing") &
        !is.na(start_zip) ~ start_zip,
      
      postal_source %in% c("Invalid and Unmatched", "Missing") &
        is.na(start_zip) & !is.na(end_zip) ~ end_zip,
      
      TRUE ~ as.character(postal.code)
    ),
    
    # 3. Now pad postal.code for EVERY record
    postal.code_cleaned = sprintf("%05s", postal.code),
    
    # 4. Replace 00000 with NA
    postal.code_cleaned = na_if(postal.code_cleaned, "00000")
  ) %>%
  select(-start_zip, -end_zip)

View()

zip_city_lookup$PHYSICAL.ZIP <-
  sprintf("%05s", zip_city_lookup$PHYSICAL.ZIP)

# Trim whitespace
bluebikes_cleaned$postal.code_cleaned <- trimws(bluebikes_cleaned$postal.code_cleaned)
zip_city_lookup$PHYSICAL.ZIP          <- trimws(zip_city_lookup$PHYSICAL.ZIP)


############################################################
# FINAL LEFT JOIN — THIS WILL NOT EXPLODE ROWS
############################################################

bluebikes_final <- bluebikes_cleaned %>%
  left_join(zip_city_lookup,
            by = c("postal.code_cleaned" = "PHYSICAL.ZIP"))
View(bluebikes_final)

############################################################
# DISTINCT ZIP COUNTS + TABULAR SUMMARY
############################################################

# 1. Basic merge validation
cat("Rows before merge: ", nrow(bluebikes_cleaned), "\n")
cat("Rows after merge:  ", nrow(bluebikes_final), "\n")
############################################################
# GENERATE TWO PDF REPORTS DIRECTLY FROM R
# 1. ZIP SUMMARY PDF 
# 2. MISSING ZIP SUMMARY PDF
############################################################

############################################################
# SAVE TOP 10 CITIES TABLE AS PNG IN EXHIBITS FOLDER
############################################################

library(dplyr)
library(ggplot2)
library(gridExtra)

# Set exhibits folder
exhibits_path <- '/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Exhibits'

# Make folder if not present
if (!dir.exists(exhibits_path)) dir.create(exhibits_path, recursive = TRUE)

# Compute top 10 cities
top10_cities <- bluebikes_final %>%
  group_by(PHYSICAL_CITY) %>%
  summarize(observations = n()) %>%
  arrange(desc(observations)) %>%
  slice(1:10)

# Convert to tableGrob
tbl <- tableGrob(top10_cities, rows = NULL)

# Save as PNG
png(filename = file.path(exhibits_path, "Exhibit_Top10_Cities.png"),
    width = 1400, height = 900, res = 150)
grid::grid.draw(tbl)
dev.off()

cat("Saved:", file.path(exhibits_path, "Exhibit_Top10_Cities.png"), "\n")

# 3. Summary table of missing rows by ZIP
missing_zip_summary <- bluebikes_final %>%
  filter(is.na(PHYSICAL_CITY)) %>%
  group_by(postal.code_cleaned) %>%
  summarize(count = n()) %>%
  arrange(postal.code_cleaned)

# 4. Print summary table
print(missing_zip_summary)

# 5. Count of unique ZIPs with missing city
unique_zip_n <- nrow(missing_zip_summary)
cat("\nUnique ZIP codes with missing city: ", unique_zip_n, "\n")
total_unique_postal <- n_distinct(bluebikes_final$postal.code_cleaned)
cat("Total unique postal.code_cleaned values: ", total_unique_postal, "\n")



head(
  bluebikes_final %>%
    select(
      postal.code_cleaned,
      PHYSICAL_CITY,
      start.station.name,
      end.station.name
    )
)


############################################################
# SAVE FINAL ZIP-MERGED DATASET
############################################################

write.csv(
  bluebikes_final,
  "/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/bluebikes_final.csv",
  row.names = FALSE
)

cat("Merged dataset saved to: bluebikes_final.csv\n")

### Viewing the merged dataset

View(bluebikes_final)
names(bluebikes_final)

bluebikes_final <- read.csv(
  "/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/bluebikes_final.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

View(bluebikes_final)
temp_zip <- read.csv(
  "/Users/maidaraza/Desktop/Tufts/Course Content/DATA 200/Homework/Project/Analysis - Datasets/temp_zip.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)
View(temp_zip)
