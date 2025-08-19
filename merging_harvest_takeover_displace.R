library(dplyr)
library(readr)

#Find mean harvest time by province. 

#For displacement data, find first time person is more than 10 km from their dwell location
#conditional on a.) leaving their district, b.) being more than 20 km from their dwell location
#create a dummy for displacing
#say when they displaced
#people are not pinging consistently and will ping multiple times on the same day. Looking like identifying displacement
#time may not be possible. Perhaps I could find the average and variance over district to understand displacement flows 
#see if the flows follow any distribution

#dependent variable: time from takeover to displacement

#independent variable: time from harvest to displacement

##opening up and checking FDD time series
library(haven)
library(purrr)
library(lubridate)
series <- read_dta("C:/Users/asobc/PycharmProjects/harvest_TO/lwj_timeseries_2021.dta")
head(series)

#list all districts
dist_names <- unique(series$district)

tal_switch <- rep(NA, length(dist_names))
cont_switch <- rep(NA, length(dist_names))
dist <- rep(NA, length(dist_names))
dist_id <- rep(NA, length(dist_names))
province <- rep(NA, length(dist_names))
prov_id <- rep(NA, length(dist_names))

for(i in 1:length(dist_names)) {
  df <- series[series$district == dist_names[i] & series$control == 1,]
  if (nrow(df) > 0) {
    cont_switch[i] <- min(df$date)
  } 
  else {cont_switch[i] <- NA}
  df1 <- series[series$district == dist_names[i] & series$control == 2,]
  if (nrow(df1) > 0) {
    tal_switch[i] <- min(df1$date)
  } 
  else {tal_switch[i] <- NA}
  dist[i] <- dist_names[i]
  df2<- series[series$district == dist_names[i],]
  dist_id[i] <- df2$distid[1]
  province[i] <- df2$province[1]
  prov_id[i] <- df2$provid[1]
}
takeover_dates <- as.data.frame(cbind(as.vector(dist),as.vector(dist_id),as.vector(province),as.vector(prov_id), as.vector(cont_switch), as.vector(tal_switch)))
names(takeover_dates) <- c("district","distid","province","provid","cont_switch","tal_switch")
write.csv(takeover_dates, "C:/Users/asobc/PycharmProjects/harvest_TO/takeover_dates.csv")

#rerun this and save the new version of "takeover_dates"
takeover_dates$cont_num <- as.numeric(takeover_dates$cont_switch)
takeover_dates$cont_switch_ymd <- as.Date(takeover_dates$cont_num, origin = "1969-12-31")

takeover_dates$tal_num <- as.numeric(takeover_dates$tal_switch)
takeover_dates$tal_switch_ymd <- as.Date(takeover_dates$tal_num, origin = "1969-12-31")
names(takeover_dates)
takeover_dates <- subset(takeover_dates, select = -c(X))

##takeover dates is now done. 
#names: district, distid, province, provid, cont_switch (epoch from 1970), tal_switch, cont_num(numeric epoch), tal_num, cont_switch_ymd, tal_switch_ymd

#now going to upload displacement data
displace <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/intersection_grid_merged.csv")

#check out frequency of pings
frequency <- aggregate(displace$utc_date, list(displace$caid), FUN = length)
five <- frequency[frequency$x >= 5,]
displace <- displace[displace$caid %in% five$Group.1,]
#find other names first
displace <- subset(displace, select = -c(Unnamed..0,index_right,X510.,X.1188,X492........R_klmno.))

#find all dwell locations
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

pre_dat <- displace[displace$utc_timestamp <= 1619845200,]
dwell_pre <- as.data.frame(aggregate(pre_dat$ID, list(pre_dat$caid), FUN = Mode))
remove(pre_dat)

#identifying centroids of IDs of grid cells
library(sf)
grid_sf <- st_read("C:/Users/asobc/Downloads/afghanistan_grid.geojson")
grid_sf$centroids <- st_centroid(grid_sf$geometry)
grid_coords <- do.call(rbind, grid_sf$centroids) %>%
  as_tibble() %>% setNames(c("lon_grid","lat_grid"))
grid <- cbind(grid_sf$ID, grid_coords)

#binding centroids to dataframes
displace <- left_join(displace, grid, by= "ID", relationship = "many-to-many")
dwell_pre <- left_join(dwell_pre, grid, by= "ID", relationship = "many-to-many")
displace <- displace[displace$caid %in% dwell_pre$caid, ]
write.csv(displace, "C:/Users/asobc/PycharmProjects/harvest_TO/clean_big_displace.csv")

displace <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/clean_big_displace.csv")
displace <- left_join(displace, dwell_pre, by = "caid", relationship = "many-to-many")
#find the first time that each person is more than 2 grid cells away (7.25 km), and the farthest distance they are from home. Store farthest location

#get rid of all data after 2022
displace <- displace[displace$utc_timestamp <= 1640998800, ]

displace$dist_prec <- sqrt(((displace$latitude - displace$lat_grid_pre)^2 + (displace$longitude - displace$lon_grid_pre)^2)) * 111
#finding the maximum distance, but also need location
max_disp <- as.data.frame(aggregate(displace$distance, list(displace$caid), FUN = max))

##now finding the maximum distance:

maximum <- displace %>%
  group_by(caid) %>%
  filter(dist_prec == max(dist_prec)) %>%
  ungroup()

max_unique <- maximum %>%
  group_by(caid) %>%
  filter(utc_timestamp == min(utc_timestamp)) %>%
  ungroup()

column_name <- caid
data_without_duplicates <- max_unique[!duplicated(max_unique$caid),]
write.csv(data_without_duplicates, "C:/Users/asobc/PycharmProjects/harvest_TO/earliest_max_disp.csv")

qualify <- displace[displace$dist_prec >= 25,]

##A problem here might be that we're getting people who are moving as a normal part of their day. 
##In theory we could change it to be the first time after the takeover that they are more than 10 km from home, but that doesn't include
#anticipatory displacement.
##if the dates look wonky, I can redo it later, but I should definitely check to see what portion of these qualifying movements occur before 
#takeover, and if so, by how long (i.e. more than 30 days)
min_qual <- qualify %>%
  group_by(caid) %>%
  filter(utc_timestamp == min(utc_timestamp)) %>%
  ungroup()

min_qual_no_dup <- min_qual[!duplicated(min_qual$caid),]
write.csv(min_qual_no_dup, "C:/Users/asobc/PycharmProjects/harvest_TO/earliest_qual_disp_25.csv")


##We have a problem -- most of the displacement seems to be occurring before the takeover. 
#binding takeover times
#first converting dates in the takeover file:
takeover_dates <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/takeover_dates.csv")
takeover_dates$cont_num <- as.numeric(takeover_dates$cont_switch)
takeover_dates$cont_switch_ymd <- as.Date(takeover_dates$cont_num, origin = "1969-12-31")

takeover_dates$tal_num <- as.numeric(takeover_dates$tal_switch)
takeover_dates$tal_switch_ymd <- as.Date(takeover_dates$tal_num, origin = "1969-12-31")
takeover_dates$cont_unix <- as.numeric(as.POSIXct(takeover_dates$cont_switch_ymd, format="%Y-%m-%d"))
takeover_dates$tal_unix <- as.numeric(as.POSIXct(takeover_dates$tal_switch_ymd, format="%Y-%m-%d"))
summary(dist_shp)

#checking that the province and district IDs are the same in the shapefile and takeover_dates
test_dist <- as.data.frame(subset(dist_shp, select = c(OBJECTID, DISTID, PROVID)))
test_TO <- as.data.frame(subset(takeover_dates, select = c(district, distid, province, provid)))
missing_dist <- setdiff(test_dist$DISTID, test_TO$distid)
#pull out rows of missing districts in takeover data from shapefile
missing <- test_dist[test_dist$DISTID %in% missing_dist,]

#find how province matches to provid in takeover file
prov_provid <- takeover_dates[!duplicated(takeover_dates$provid),]

#manually enter these
province <- c("Badghis","Jawzjan","Faryab","Faryab","Ghazni","Takhar","Zabul")
missing <- cbind(missing, province)

#bind missing districts
head(takeover_dates)
col_NA <- as.vector(c(NA,NA,NA,NA,NA,NA,NA))
missing <- cbind(col_NA, missing, col_NA, col_NA, col_NA, col_NA, col_NA, col_NA, col_NA, col_NA)
#drop OBJECTID and switch province and provid
missing <- subset(missing, select = -c(OBJECTID))
names(missing) <- c("district", "distid", "provid", "province", "cont_switch", "tal_switch", "cont_num", "cont_switch_ymd","tal_num","tal_switch_ymd","cont_unix","tal_unix")
missing<- missing[c("district", "distid", "province", "provid", "cont_switch", "tal_switch", "cont_num", "cont_switch_ymd","tal_num","tal_switch_ymd","cont_unix","tal_unix")]
takeover_dates <- subset(takeover_dates, select = -c(X))
takeover_all <- rbind(takeover_dates, missing)
write.csv(takeover_all, "C:/Users/asobc/PycharmProjects/harvest_TO/takeover_dates.csv")
##merge max_disp, min_qual, and min_qual_25
#keep dist_prec, distance, longitude, latitude, utc_timestamp, date, for each.
#keep caid, lon_grid_pre, lat_grid_pre, altitude unique to each user.
#get rid of ID, ID_pre, lon_grid, lat_grid, X.1, X, geometry for all
max_disp <- subset(max_disp, select= -c(X.1, X, geometry, lat_grid, lon_grid, ID, ID_pre))
min_qual <- subset(min_qual, select= -c(X.1, X, geometry, lat_grid, lon_grid, ID, ID_pre))
min_qual <- subset(min_qual, select= -c(lon_grid_pre,lat_grid_pre))
min_qual_25 <- subset(min_qual_25, select= -c(X.1, X, geometry, lat_grid, lon_grid, ID, ID_pre))
min_qual_25 <- subset(min_qual_25, select= -c(lon_grid_pre,lat_grid_pre))
names(max_disp) <- c("caid","utc_timestamp_max","altitude_max","utc_date_max","lon_max","lat_max","lon_grid_pre","lat_grid_pre","distance_max","dist_prec_max")
names(min_qual) <- c("caid","utc_timestamp_10","altitude_10","utc_date_10","lon_10","lat_10","distance_10","dist_prec_10")
names(min_qual_25) <- c("caid","utc_timestamp_25","altitude_25","utc_date_25","lon_25","lat_25","distance_25","dist_prec_25")
merge_1_2 <- left_join(max_disp, min_qual, by = "caid", relationship = "many-to-many")
merge_1_2 <- left_join(merge_1_2, min_qual_25, by = "caid", relationship = "many-to-many")
head(merge_1_2)
write.csv(merge_1_2, "C:/Users/asobc/PycharmProjects/harvest_TO/merge_displace_only.csv")

#need to find how many pings each users has in 2021 -- pull from displace and merge 
displace <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/clean_big_displace.csv")
frequency2 <- aggregate(displace$utc_date, list(displace$caid), FUN = length)
quantile(frequency2$x)
big <- frequency2[frequency2$x <= 10000,]
num_pings <- frequency2$x

merge_1_2 <- left_join(merge_1_2, frequency2, by = "caid", relationship = "many-to-many")
#saved again as .csv here

#now going to merge with takeover data
#first assign each lon, lat to a district
xy <- merge_1_2[,c("lon_grid_pre","lat_grid_pre")]
lonlat <- SpatialPointsDataFrame(coords = xy, data = merge_1_2,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

grid_int <- over(lonlat,dist_shp)
grid_int_clean_2 <- grid_int2[complete.cases(grid_int2),]
merge_1_2 <- cbind(merge_1_2, grid_int)
merge_1_2 <- merge_1_2 %>%
  rename("distid" = "DISTID",
         "provid" = "PROVID"
  )
head(merge_1_2)

#finally merging with takeover_dates

disp_TO <- left_join(merge_1_2,takeover_all, by = "distid", relationship = "many-to-many")
head(disp_TO)
disp_TO <- subset(disp_TO, select = -c(provid.y))
disp_TO <- disp_TO %>% rename("provid" = "provid.x")

#saving as a .csv
write.csv(disp_TO,"C:/Users/asobc/PycharmProjects/harvest_TO/displace_takeover_merge.csv")

##Now formatting dates and merging crop calendar

wheat <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/crop-calendar.csv")
wheat$spring_begin_unix <- as.numeric(as.POSIXct(wheat$spring_begin, format="%Y-%m-%d"))
wheat$spring_end_unix <- as.numeric(as.POSIXct(wheat$spring_end, format="%Y-%m-%d"))
wheat$fall_begin_unix <- as.numeric(as.POSIXct(wheat$fall_begin, format="%Y-%m-%d"))
wheat$fall_end_unix <- as.numeric(as.POSIXct(wheat$fall_end, format="%Y-%m-%d"))
wheat$spring_mid_unix <- ((wheat$spring_end_unix - wheat$spring_begin_unix)/2) + wheat$spring_begin_unix
wheat$fall_mid_unix <- ((wheat$fall_end_unix - wheat$fall_begin_unix)/2) + wheat$fall_begin_unix

#drop the district column because it's not at all helpful (still in the original crop calendar .csv)
wheat <- subset(wheat, select = -c(District))
wheat <- wheat %>% rename("province_wheat" = "Province")
#now merge with everything else
total_merge <- left_join(disp_TO, wheat, by = "provid", relationship = "many-to-many")
total_merge <- subset(total_merge, select = -c(province_wheat))
write.csv(total_merge, "C:/Users/asobc/PycharmProjects/harvest_TO/three_merge_intermediate.csv")

#the data still looks too noisy.

##I DIDN'T STICK WITH THIS. INSTEAD GOING TO ESTIMATE DISTRICT LEVEL FLOWS AND RESTRICT TO FEW PINGS
#NEW PLAN: Restrict to users that ping at least once every 2 weeks from 3/1/21 to 9/1/21
#first load the displacement data, then restrict only to pings within range
displace <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/clean_big_displace.csv")
restrict_sep <- displace[displace$utc_timestamp >= 1617238800 & displace$utc_timestamp <= 1630458000,]
restrict_aug <- restrict_sep[restrict_sep$utc_timestamp >= 1617238800 & restrict_sep$utc_timestamp <= 1627779600,]

#trying to sort by time within caid group and it's proving difficult. see arbitrarily chosen page 33 in journal.
sorted <- split(displace, displace$caid)
sorted <- lapply(sorted, function(group_data) {
  group_data[order(group_data$utc_timestamp), ]
})
sort_dat <- do.call(rbind, sorted)

differences <- diff(sort_dat$utc_timestamp)
frame <- data.frame(Original = sort_dat$utc_timestamp, Differences = c(NA, differences))
names(frame) <- c("check","differences")
bound_df <- cbind(sort_dat, frame)

#going to see how much smaller the data gets if you remove 
#duplicate pings (if the grid ID and day are the same, keep only one entry)

unique_rows <- bound_df[!duplicated(bound_df[, c("ID", "utc_date")]), ]

#and holy shit, it decreased by 90% BECAUSE I FORGOT TO GROUP BY USER SMH

#I'm going to remove all of the duplicates from the very big file and that will probably make things a lot easier in terms of RAM.

###########
##PROBLEM##
###########
no_dup <- displace %>%
  group_by(caid) %>%
  filter(!duplicated(displace[, c("ID", "utc_date")])) %>%
  ungroup()
###########
##PROBLEM##
###########

#not sure why the above code didn't work :(
#there we go
#now going to find the frequency of remaining pings
freq_no_dup <- aggregate(no_dup_all$utc_date, list(no_dup_all$caid), FUN = length)
no_dup_all <- left_join(no_dup_all, freq_no_dup, by = "caid", relationship = "many-to-many")

##I don't have a lot of faith in my current merged file, so I'm going to set overall takeover times in the takeover file
change_time <- rep(NA, 398)
for (i in seq_along(takeover_all$distid)) {
  if (!is.na(takeover_all$cont_unix[i]) && takeover_all$cont_unix[i] >= 1617238800) {
    change_time[i] = takeover_all$cont_unix[i]
  } else if (!is.na(takeover_all$tal_unix[i]) && takeover_all$tal_unix[i] >= 1617238800) {
    change_time[i] = takeover_all$tal_unix[i]
  } else {
    change_time[i] = NA
  }
}
takeover_all <- cbind(takeover_all, change_time)

