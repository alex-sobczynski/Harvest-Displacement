library(dplyr)
library(readr)
library(readxl)
library(rgdal)

folder_path <- "C:/Users/asobc/PycharmProjects/harvest_TO/crop cal/"
excel_files <- list.files(folder_path, pattern = "\\.xls$", full.names = TRUE)
result_list <- list()
test_path <- "C:/Users/asobc/PycharmProjects/harvest_TO/rough_crop_cal.csv"
test <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/rough_crop_cal.csv")

compare <- test %>%
  filter(crop %in% c("Irragated wheat winter", "Irragated wheat summur", "Wheat"))

dist_map <- readOGR("C:/Users/asobc/PycharmProjects/harvest_TO/398/")
 

library(haven)
terrain <- read_dta("C:/Users/asobc/PycharmProjects/harvest_TO/lwj_timeseries_2021.dta")
head(terrain)

library(stringdist)
library(stringi)

# Sample district names
districts <- unique(terrain$province)

# Target district name with a typo
target_districts <- unique(compare$province)

compare$district <- stri_trans_totitle(stri_trans_tolower(compare$district))
compare$province <- stri_trans_totitle(stri_trans_tolower(compare$province))

# Assuming wheat_test is your data frame
compare$district <- sub("^.*?\\((.*?)\\)", "\\1", compare$district)

fuzzy_match_district <- function(target_district, districts) {
  distances <- stringdist::stringdistmatrix(target_district, districts, method = "jaccard")
  closest_match_index <- which.min(distances)
  closest_match <- districts[closest_match_index]
  return(closest_match)
}

for (i in seq_along(target_districts)) {
  target_district <- target_districts[i]
  closest_match <- fuzzy_match_district(target_district, districts)
  print(paste("Original district:", target_district))
  print(paste("Closest match:", closest_match))
  print("---")
}

removed <- c("Chenarto","Jaji","Meerzaka","Wuza Jadran","Khairkot","Duab",
             "Noor Gram","Paroon","Dil Aram","Khar War","Dawlat Shah","Koznark",
             "Sawkai","Char Sada","Duleena","Zanakhan","Feroz Koh","Nawamesh",
             "Pato","Jalga","Yaftalpayan","Shemel Zagi","Shah Joi","Khak-E-Afghan","Nawzad","Marja","Mamay")
compare <- compare[!compare$district %in% removed, ]
#now that I've removed the districts that don't match, I'm going to reassign the values in wheat_test

vec <- as.vector(sapply(compare$district, function(x) fuzzy_match_district(x, terrain$district)))
vec_2 <- as.vector(sapply(compare$province, function(x) fuzzy_match_district(x, terrain$province)))
compare$district <- vec
compare$province <- vec_2
part <- terrain %>%
  select(district, distid, province) %>%
  distinct()

second_2 <- left_join(compare, part, by = c("district","province"), relationship = "many-to-many")
second_3 <- second_2 %>%
  filter(!is.na(distid))
second_4 <- second_3 %>%
  distinct()

write.csv(second, "C:/Users/asobc/PycharmProjects/harvest_TO/crop_cal_by_dist.csv")
#THERE ARE THREE DUPLICATES IN HERE.
# Print the modified data frame
print(second)

duplicated_values <- duplicated(second_5$district) | duplicated(second_5$district, fromLast = TRUE)

# Subset the data frame to keep only rows with duplicated values in the 'Value' column
duplicates <- second_5[duplicated_values, ]
duplicates

dup_row <- c(33,133,157)

second_5 <- second_4[-dup_row,]

good_one <- second_5

#now I'm going to fill in the unknown districts with their values from the province data.

#load old data
crop_cal <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/crop-calendar.csv")
vec <- crop_cal$Province
#gonna use terrain as a base.
#do some quick renaming of the provinces to terrain:

old_values <- c("Sari pul")

# Define new names for the identified values
new_values <- c("Sari Pul")

# Use indexing to replace old values with new values
crop_cal$Province[crop_cal$Province %in% old_values] <- new_values
crop_cal$Province <- vec

brief <- subset(crop_cal, select = -c(District, notes))
brief <- read_csv("C:/Users/asobc/PycharmProjects/harvest_TO/EDIT_ME.csv")
brief <- brief %>%
  select(province, harv_start, harv_end) 
blend <- left_join(part, brief, by = "province", relationship = "many-to-many")

names(second_5) <- c("crop","province","district","plant_start","plant_end","start_dist","end_dist","distid")

all <- left_join(blend, second_5, by = c("province","district", "distid"), relationship = "many-to-many")

library(lubridate)

day <- as.numeric(gsub("-(\\w+)$", "", all$start_dist))
month <- match(gsub("^\\d+-([A-Za-z]+)$", "\\1", all$start_dist), month.abb)

date <- ifelse(!is.na(month), as.Date(paste("2021", month, day, sep = "-")), NA)

month_abbrev_to_num <- function(abbrev) {
  month.abb[match(toupper(abbrev), toupper(month.abb))]
}

date2 <- as.Date(
  sprintf("2021-%02d-%02d", 
          month_abbrev_to_num(sub("^\\d+-(\\w+)$", "\\1", all$start_dist)),
          as.numeric(sub("^\\d+-(\\d+)$", "\\1", all$start_dist))
  )
)


day <- as.numeric(gsub("-(\\w+)$", "", all$end_dist))
month <- match(gsub("^\\d+-([A-Za-z]+)$", "\\1", all$end_dist), month.abb)

# Create a new column with year 2021
year <- 2021

# Create a lubridate date object
date2 <- make_date(year, month, day)
all$end_dist <- date2
all$start_dist <- date2

# Convert "mm/dd/yyyy" format to Date with year 2021
vec2 <- mdy(all$harv_start)
all$harv_start <- vec2

#pick district value if available. ow 
all$begin_harv_blend <- ifelse(is.na(all$start_dist), all$harv_start, all$start_dist)
all$end_harv_blend <- ifelse(is.na(all$end_dist), all$harv_end, all$end_dist)
all$end_harv_blend <- as.Date(all$end_harv_blend, origin = "1970-01-01")
all$begin_harv_blend <- as.Date(all$begin_harv_blend, origin = "1970-01-01")
all$dist_specific <- ifelse(is.na(all$start_dist), 0, 1)

names(all)

save <- all %>%
  select(district,distid,province,crop,begin_harv_blend,end_harv_blend,dist_specific)

write.csv(save, "C:/Users/asobc/PycharmProjects/harvest_TO/NEW_CROP_CAL.csv")

names(dist_map) <- c("objectid","distid","provid")

merged_data <- merge(dist_map, save, by = "distid")
sf_merge <- st_as_sf(merged_data)

library(ggplot2)
library(sf)
library(sp)

sf_merge$dist_specific <- as.factor(sf_merge$dist_specific)
sf_merge$dist_specific <- factor(sf_merge$dist_specific, levels = c("0", "1"))
sf_merge$mid_harv <- (as.numeric(sf_merge$begin_harv_blend) + as.numeric(sf_merge$end_harv_blend)) / 2
tv <- month(sf_merge$end_harv_blend)
sf_merge$end_month <- month(sf_merge$end_harv_blend)
ggplot(sf_merge) +
  geom_sf(aes(fill = end_month)) +
  #scale_fill_manual(values = c("white", "black")) +  # Adjust colors as needed
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()

ggplot(merged_data, aes(x = long, y = lat, group = group, fill = dist_specific)) +
  geom_polygon(color = "white", size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()

write.csv(sf_merge, "C:/Users/asobc/PycharmProjects/harvest_TO/crop_cal_sf.csv")
  
class(merged_data)
