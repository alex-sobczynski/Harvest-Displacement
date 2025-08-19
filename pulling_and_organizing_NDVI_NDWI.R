library(ncdf4)
ncname <- "C:/Users/asobc/PycharmProjects/harvest_TO/bands_for_NDVI/MOD09A1.061_500m_2010.nc"
ncin <- nc_open(ncname)
print(ncin)
time <- ncvar_get(ncin,"time")
tunit <- ncatt_get(ncin,"time","units")
tunit$value

#need to open grid as an shapefile (sp)
library(chron)

#Convert dates into mm/dd/yy
tustr <- strsplit(tunit$value, " ")  
tdstr <- strsplit(unlist(tustr)[3], "-")
tdmstr <- strsplit(unlist(tustr)[4], ":") # 
tmonth <- as.integer(unlist(tdstr)[2]) # 
tday <- as.integer(unlist(tdstr)[3])  #
tyear <- as.integer(unlist(tdstr)[1])
date <- as.Date(chron(time,origin=c(tmonth, tday, tyear)))
dates <- as.data.frame(cbind(row_number(time),time))
dates$date <- as.Date(dates$time, origin = "2000-01-01")

b1name <- "sur_refl_b01"
b2name <- "sur_refl_b02"
b6name <- "sur_refl_b06"
b7name <- "sur_refl_b07"

lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
lonlat <- as.matrix(expand.grid(lon,lat))
#THE FIRST AND LAST DIGITS OF THE SEQUENCE NEEDS TO BE CHANGED

b1_array <- ncvar_get(ncin,b1name)
b2_array <- ncvar_get(ncin,b2name)
b6_array <- ncvar_get(ncin,b6name)
b7_array <- ncvar_get(ncin,b7name)

fillvalue1 <- ncatt_get(ncin,b1name,"_FillValue")
fillvalue2 <- ncatt_get(ncin,b2name,"_FillValue")
fillvalue6 <- ncatt_get(ncin,b6name,"_FillValue")
fillvalue7 <- ncatt_get(ncin,b7name,"_FillValue")

for(i in 1:nrow(dates)){
  
  b1_array2 <- b1_array[,,i]
  b2_array2 <- b2_array[,,i]
  b6_array2 <- b6_array[,,i]
  b7_array2 <- b7_array[,,i]
  
  #make all the arrays
  b1_array2[b1_array2==fillvalue1$value] <- NA
  b2_array2[b2_array2==fillvalue2$value] <- NA
  b6_array2[b6_array2==fillvalue6$value] <- NA
  b7_array2[b7_array2==fillvalue7$value] <- NA
  
  #forming the dataframe
  date <- format(dates$date[i], "%Y%m%d")
  b1_vec <- as.vector(b1_array2)
  b2_vec <- as.vector(b2_array2)
  b6_vec <- as.vector(b6_array2)
  b7_vec <- as.vector(b7_array2)
  NDVI <- (b2_vec - b1_vec)/(b2_vec + b1_vec)
 # NDWI_1 <- undecided
 # NDWI_2 <- undecided
  day <- rep(date, length(NDVI))
  df01 <- data.frame(cbind(lonlat,day,NDVI,NDWI_1,NDWI_2))
  names(df01) <- c("lon","lat","day","NDVI","NDWI_1","NDWI_2")
  df01 <- df01[complete.cases(df01),]
  df_name <- paste("bands_", date, sep = "")
  assign(df_name, df01)
  
  #assign grid cells
  #spdf <-SpatialPointsDataFrame(coords =df01[, c("lon","lat")], data=df01,
  #                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  #file_int <- over(spdf,grid)
  #file_int <- subset(file_int,select = -c(OBJECTID))
  #names(file_int) <- c("distid","provid")
  #file <- cbind(data,file_int)
  
  #csvpath <- "C:/Users/asobc/PycharmProjects/harvest_TO/csvs/"
  #csvname <- paste0("bands_",i,".csv")
  #csvfile <- paste(csvpath, csvname, sep="")
  #write.csv(df01,csvfile)
  print(paste0("Wrote file: ", date))
  
  remove(b1_array2)
  remove(b2_array2)
  remove(b6_array2)
  remove(b7_array2)
  remove(b1_vec)
  remove(b2_vec)
  remove(b6_vec)
  remove(b7_vec)
  remove(NDVI)
  remove(NDWI_1)
  remove(NDWI_2)
  #remove(spdf)
  
}
write.csv(combined_dataframe, "C:/Users/asobc/PycharmProjects/harvest_TO/checking_file_size.csv")
dataframe_names <- ls(pattern = "^bands_")

# Create a list of dataframes using lapply
dataframe_list <- lapply(dataframe_names, get)

# Find the common lon and lat values across all dataframes
common_lonlat <- Reduce(intersect, lapply(dataframe_list, function(df) paste(df$lon, df$lat)))

# Filter each dataframe to keep only the rows with common lon and lat values
filtered_dataframes <- lapply(dataframe_list, function(df) df[paste(df$lon, df$lat) %in% common_lonlat, ])

#make the panel
combined_dataframe <- do.call(rbind, filtered_dataframes)

write.csv(combined_dataframe, "wherever_we_put_it.csv")

nc_close(ncname)

#now I want to create a panel by year with peak NDVI, (date of peak NDVI), and average of top three NDVI values

peak <- combined_dataframe %>%
  group_by(lon, lat) %>%
  mutate(NDVI_top_3 = mean(tail(sort(NDVI), 3))) %>%
  filter(NDVI == max(NDVI)) %>%
  mutate(year = substr(day,1,4)) %>%
  ungroup()

#here I would save the panel

