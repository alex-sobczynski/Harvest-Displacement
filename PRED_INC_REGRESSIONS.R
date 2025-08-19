library(readr)
library(fixest)
library(dplyr)

#OKIE DOKE LET'S RUN SOME REGRESSIONS
big_lpm <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/big_data_for_lpm.csv")
in_range2 <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/IN_RANGE_FARMERS.csv")
in_range <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/in_range.csv")
reg <- lm(displace_20 ~ pred_inc + closest_cap + kabul_dist + treat, data = in_range2)
summary(reg)

#gonna check that district and province crop calendar comparable
crop_cal_dist <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/NEW_CROP_CAL.csv")
crop_cal_prov <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/EDIT_ME.csv")
together <- left_join(crop_cal_dist, crop_cal_prov, by = "province", relationship = "many-to-many")
together2 <- together[!together$dist_specific == 0,]

together$end_harv_blend_unix <- as.numeric(as.POSIXct(together$end_harv_blend, format="%Y-%m-%d"))
together$end_harv_unix <- as.numeric(as.POSIXct(together$harv_end, format="%d/%m/%Y"))


reg <- lm(end_harv_unix ~ end_harv_blend_unix,data = filtered_df)

plot(filtered_df$end_harv_unix, filtered_df$end_harv_blend_unix, main="Scatterplot Example", xlab="X-axis label", ylab="Y-axis label", col="blue", pch=16)

top_values <- together2 %>%
  arrange(desc(end_harv_unix)) %>%
  slice_head(n = 3) %>%
  pull(end_harv_unix)

# Remove rows with the three largest values
filtered_df <- together2 %>%
  filter(!end_harv_unix %in% top_values)

##^^^^^ this didn't work. Just fucking ignore. Afghanistan is fucked

#are harvest time and takeover time exogenous?
to_dates <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/takeover_dates.csv")
to_harv <- left_join(to_dates, crop_cal_dist, by = "distid")
to_harv$end_harv_blend_unix <- as.numeric(as.POSIXct(to_harv$end_harv_blend, format="%Y-%m-%d"))
to_harv$begin_harv_blend_unix <- as.numeric(as.POSIXct(to_harv$begin_harv_blend, format="%Y-%m-%d"))
to_harv$end_harv_blend_num <- as.integer((to_harv$end_harv_blend_unix - 1609484400)/86400)
to_harv$tal_num_jan <- to_harv$tal_num - 18628
aft_jan <- to_harv[to_harv$tal_num >= 18628,]
bef_jan <- to_harv[to_harv$tal_num <= 18628,]
endo <- lm(tal_num_jan ~ end_harv_blend_num,data = aft_jan)
gonna <- to_harv[to_harv$distid %in% wheat_d_vec,]
wheat_dist_b <- wheat_dist[wheat_dist$begin_harv_blend_unix > wheat_dist$tal_unix,]
wheat_dist_i <- wheat_dist[wheat_dist$begin_harv_blend_unix < wheat_dist$tal_unix & wheat_dist$end_harv_blend_unix > wheat_dist$tal_unix,]
wheat_dist_a <- wheat_dist[wheat_dist$end_harv_blend_unix < wheat_dist$tal_unix,]

wheat_dist_b_f <- wheat_dist_b[wheat_dist_b$wheat_area == 1,]
wheat_dist_i_f <- wheat_dist_i[wheat_dist_i$wheat_area == 1,]
wheat_dist_a_f <- wheat_dist_a[wheat_dist_a$wheat_area == 1,]

plot(aft_jan$tal_num_jan, aft_jan$end_harv_blend_num, 
     #main="Correlation Between Takeover and Harvest", 
     xlab="Takeover (Days from January 1, 2021)", cex.lab=1.4, ylab="Wheat Harvest End (Days from January 1, 2021)", col="blue", pch=16)

abline(endo, col = "black", )



#what happens when I take the takeover that occurred the year before?
bef_jan <- to_harv[to_harv$tal_num <= 18628,]
bef_jan$end_harv_blend_unix <- bef_jan$end_harv_blend_unix - 31536000
bef_jan$end_harv_blend_num <- as.integer((bef_jan$end_harv_blend_unix - 1609484400)/86400)
adj_season <- rbind(aft_jan,bef_jan)

endo_season <- lm(tal_num ~ end_harv_blend_num, data = adj_season)

plot(adj_season$tal_num, adj_season$end_harv_blend_num, main="Slight Correlation Between Takeover and Harvest", xlab="Takeover (Days from January 1, 2021)", ylab="Wheat Harvest End (Days from January 1, 2021)", col="blue", pch=16)

##PROMISING

#make a figure of takeover dates
from_jan <- to_dates$tal_switch - 18628
hist(from_jan, breaks = 100)
#breaks <- seq(min(from_jan), max(from_jan) + 1, by = 1)
hist(
  from_jan,
  breaks = 299,              # Number of bins
  col = "black",          # Color of bars
  main = "Taliban Takeover Dates by District",  # Title of the plot
  xlab = "Days from January 1, 2021",    # Label for X-axis
  ylab = "Frequency",       # Label for Y-axis
  border = "black",         # Border color of bars
  xlim = c(-100,250),          # X-axis limits
  ylim = c(0, 80)          # Y-axis limits
  #density = 50,             # Density of shading lines
  #angle = 45               # Angle of shading lines
  #probability = TRUE        # Show probability density instead of frequency
)

abline(v = 122, col = "red", lty = 2)  # Vertical line at day 121
abline(v = 227, col = "blue", lty = 2) # Vertical line at day 227

# Add labels for the vertical lines
text(121, par("usr")[3] - 0.5, "May 1", col = "red", pos = 1, cex = 0.8)
text(227, par("usr")[3] - 0.5, "August 15", col = "red", pos = 1, cex = 0.8)

legend("topright", legend = c("May 1", "August 15"), col = c("red", "blue"), lty = c(2, 2), cex = 0.8)

#Go through event study again

################
################
################
in_range$end_harv_blend_unix <- as.numeric(as.POSIXct(in_range$end_harv_blend, format="%Y-%m-%d"))
in_range$begin_harv_blend_unix <- as.numeric(as.POSIXct(in_range$begin_harv_blend, format="%Y-%m-%d"))
in_range$in_harv_blend <- ifelse(in_range$end_harv_blend_unix > in_range$utc_timestamp & in_range$begin_harv_blend_unix < in_range$utc_timestamp, 1,0)
in_range$aft_harv_blend <- ifelse(in_range$utc_timestamp > in_range$end_harv_blend_unix, 1,0)
in_range$bef_harv_blend <- ifelse(in_range$utc_timestamp < in_range$begin_harv_blend_unix, 1,0)
in_harv <- in_range[in_range$in_harv_blend == 1,]
bef_harv <- in_range[in_range$bef_harv_blend == 1,]
aft_harv <- in_range[in_range$aft_harv_blend == 1,]


farmer$in_harv_blend <- ifelse(farmer$end_harv_blend_unix > farmer$utc_timestamp & farmer$begin_harv_blend_unix < farmer$utc_timestamp, 1,0)
farmer$aft_harv_blend <- ifelse(farmer$utc_timestamp > farmer$end_harv_blend_unix, 1,0)
farmer$bef_harv_blend <- ifelse(farmer$utc_timestamp < farmer$begin_harv_blend_unix, 1,0)
in_harv_d <- farmer[wheat_dist$in_harv_blend == 1,]
bef_harv_d <- farmer[wheat_dist$bef_harv_blend == 1,]
aft_harv_d <- farmer[wheat_dist$aft_harv_blend == 1,]
in_harv_u <- farmer[wheat_urb$in_harv_blend == 1,]
bef_harv_u <- farmer[wheat_urb$bef_harv_blend == 1,]
aft_harv_u <- farmer[wheat_urb$aft_harv_blend == 1,]
in_harv_f <- farmer[farmer$in_harv_blend == 1,]
bef_harv_f <- farmer[farmer$bef_harv_blend == 1,]
aft_harv_f <- farmer[farmer$aft_harv_blend == 1,]


here <- in_range %>%
  group_by(caid) %>%
  filter(dist_prec >= 20) %>%
  filter(utc_timestamp == min(utc_timestamp)) %>%
  ungroup()
#let's try to interact duration of displacement with harvest time

big_lpm$in_range <- ifelse(big_lpm$utc_timestamp - big_lpm$tal_unix < 1296000 & big_lpm$utc_timestamp - big_lpm$tal_unix > -1296000, 1, 0)
mod_maybe <- lm(dist_prec ~ in_range + in_harv + in_range*in_harv + closest_cap + kabul_dist, data = big_lpm)
summary(mod_maybe)

in_range$bef_sep_fat_sum[is.na(in_range$bef_sep_fat_sum)] <- 0
farmer$bef_sep_fat_sum[is.na(farmer$bef_sep_fat_sum)] <- 0
farmer$days_since <- as.integer((farmer$utc_timestamp - 1609480800)/86400)
#I want to know how the amount of time since the end of the harvest effects distance left (and when people leave)
#So I want to know both -- how early do people leave based on when the last harvest was and how far do they go

#curious how the event studies change with and without time fixed effects

small_aft <- aft_harv[sample(nrow(aft_harv), 7534), ]

breaks <- c(-29, -20, -13, -6, 0, 7, 14, 21, 30)
labels <- c(-3,-2,-1, 0, 1, 2, 3, 4)

in_range$weeks_to_event <- cut(in_range$days_to_event, breaks = breaks, labels = labels, include.lowest = TRUE)

library(plm)
wheat_d_vec <- unique(unique_wheat$distid)
wheat_urb <- in_range[!in_range$distid %in% wheat_d_vec,]
wheat_dist <- in_range[in_range$distid %in% wheat_d_vec,]

main_mod_0 <- plm(dist_prec ~  i(days_to_event, ref = -29) + 
                    kabul_dist + closest_cap + bef_sep_fat_sum,                           
                  data =  aft_harv_d,
                  index = "distid", 
                  model = "within")

main_tbl_0_st <- head(broom::tidy(main_mod_0, conf.int = TRUE), 59)
main_tbl_0_st$term <- c(-28:30)
#main_tbl_0_st$term <- c(-2:4)
main_tbl_0_st$term <- factor(main_tbl_0_st$term, levels = main_tbl_0_st$term)
ggcoef(main_tbl_0_st) + labs(x = "Estimate (km)", y = "Days from Takeover") + coord_flip() + theme(axis.text.x = element_text(angle = 60))

ggplot(main_tbl_0_st, aes(x = term, y = estimate)) +
  geom_point(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(0.2), size = 3) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(0.2), size = 1) +
  #coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, color = "black") +
  #geom_vline(linetype = "dashed", size = 1, color = "black") +
  labs(x = "Weeks from Takeover", y = "Estimate (km)") +
  #theme_grey(base_size = 40) +
  theme_minimal() +  # Adjust the theme as needed
  theme(axis.text.x = element_text(size = 14, angle = 60,face="bold"),
        axis.text.y = element_text(size = 14,face="bold"),
        panel.grid = element_blank(),
        axis.title=element_text(size=20,face="bold"))  # Remove the grey grid lines

#Ok, now I'm going to bind all the things I need for 

rf <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/RAINFED_BY_2.5.csv")
rf <- subset(rf, select = c(rainfed, ID_pre))
ir <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/IRRIGATED_BY_2.5.csv")
ir <- subset(ir, select = c(irrigated, ID_pre))
price21 <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/PRICE_2021_BY_2.5.csv")
price21 <- subset(price21, select = c(price21, ID_pre))
price20 <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/PRICE_2020_BY_2.5.csv")
price20 <- subset(price20, select = c(price20, ID_pre))
ndvi <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/NDVI_BY_2.5.csv")
ndvi <- subset(ndvi, select = c(ndvi, ID_pre))

in_range <- left_join(in_range, ndvi, by = "ID_pre", relationship = "many-to-many")
names(in_range)

library(fixest)

in_range$wheat_area <- ifelse(in_range$rainfed == 1 | in_range$irrigated == 1, 1, 0)

farmers_only <- in_range[in_range$wheat_area == 1,]
wheat_only <- farmers_only[farmers_only$ndvi >= 0.2,]

unique_wheat <- wheat_only %>%
  distinct(caid, .keep_all = TRUE)
write.csv(unique_wheat, "C:/Users/asobc/PycharmProjects/harvest_TO/unique_wheat.csv")
unique_wheat2 <- read.csv("C:/Users/asobc/PycharmProjects/harvest_TO/unique_wheat.csv")
not_done <- unique_wheat2[unique_wheat2$aft_harv_blend != 1,]
done <- unique_wheat[unique_wheat$aft_harv_blend == 1,]
#working with unique wheat

#DIFF MEANS AFT HARV (n = 2966)
diffmeanm_a <- lm(max_dist_TO ~ inc_21,data = done)
summary(diffmeanm_a)
#significant 1.54(.04)
diffmeand_a <- lm(displace_20 ~ inc_21,data = done)
summary(diffmeand_a)
#significant .007(.002)

#DIFF MEANS BEFORE HARV (n = 421)
diffmeanm_b <- lm(max_dist_TO ~ inc_21,data = not_done)
summary(diffmeanm_b)
#totally insignificant (ndvi, inc_20, and inc_21)
diffmeand_b <- lm(displace_20 ~ inc_21,data = not_done)
summary(diffmeand_b)
#totally insignificant (ndvi, inc_20, and inc_21)

unique_wheat$inc_21 <- unique_wheat$price21*unique_wheat$ndvi
unique_wheat$inc_20 <- unique_wheat$price20*unique_wheat$ndvi
unique_wheat$mix_inc <- ifelse(unique_wheat$aft_harv_blend == 1, unique_wheat$inc_21, unique_wheat$inc_20)
unique_wheat$mix_price <- ifelse(unique_wheat$aft_harv_blend == 1, unique_wheat$price21, unique_wheat$price20)

#DIFF MEANS FULL SAMPLE (n = 3386)
diffmeanm <- lm(max_dist_TO ~ mix_inc,data = unique_wheat)
summary(diffmeanm)
#significant 1.3(.03)
diffmeand <- lm(displace_20 ~ mix_inc,data = unique_wheat)
summary(diffmeand)
#significant .009(5.46e-06)


#OLS AFTER HARV
olsd_a <- lm(displace_20 ~ ndvi*price21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = done)
summary(olsd_a)
#this one is weird: ndvi: -5.296(.015); price21 -0.0619(.006); ndvi*price21 .156(.013)
olsm_a <- lm(max_dist_TO ~ price21*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = done)
summary(olsm_a)
#again weird: ndvi: -2.943e+03(1.10e-05); price21 -2.561e+01(0.000247); ndvi*price21 8.512e+01(1.16e-05)

olsd_a2 <- lm(displace_20 ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done)
summary(olsd_a2)
#insignificant
olsm_a2 <- lm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done)
summary(olsm_a2)
#insignificant

#OLS BEFORE HARV
olsd_b <- lm(displace_20 ~ ndvi*price20 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = not_done)
summary(olsd_b)
#insignificant w/ price20 and price21
olsm_b <- lm(max_dist_TO ~ price20*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = not_done)
summary(olsm_b)
#insignificant w/ price20 and price21

olsd_b2 <- lm(displace_20 ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = not_done)
summary(olsd_b2)
#insignificant w/ inc_20 and inc_21
olsm_b2 <- lm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = not_done)
summary(olsm_b2)
#insignificant w/ inc_20 and inc_21

#OLS FULL SAMPLE
olsd <- lm(displace_20 ~ ndvi*price21 + aft_harv_blend + utc_max_TO + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = unique_wheat)
summary(olsd)
#price is significant for all of mix_price, price20, price21, but never ndvi
olsm <- lm(max_dist_TO ~ price21*ndvi + aft_harv_blend + utc_max_TO + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = unique_wheat)
summary(olsm)
#all totally insignificant


olsd_b2 <- lm(displace_20 ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = unique_wheat)
summary(olsd_b2)
#insignificant w/ inc_20 and inc_21
olsm_b2 <- lm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = unique_wheat)
summary(olsm_b2)
#insignificant w/ inc_20 and inc_21
done <- done[complete.cases(done$distid),]
not_done <- not_done[complete.cases(not_done$distid),]
unique_wheat <- unique_wheat[complete.cases(unique_wheat$distid),]
unique_wheat <- unique_wheat2[complete.cases(unique_wheat2$distid),]

#FIXED EFFECTS AFTER HARVEST
fixd_a <- plm(displace_20 ~ ndvi*price21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done, index="distid", model="within")
summary(fixd_a)
#this one is weird: ndvi: -6.269(0.023); price21 INSIG; ndvi*price21 0.171(0.032)
fixm_a <- plm(max_dist_TO ~ price21*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = done, index="distid", model="within")
summary(fixm_a)
#again weird: ndvi: -2.6778e+03(.0017); INSIG; ndvi*price21 7.5491e+01(0.002)

fixd_a2 <- plm(displace_20 ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done, index="distid", model="within")
summary(fixd_a2)
#NOT WHAT I'D THINK: -0.009(0.003)
fixm_a2 <- plm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done, index="distid", model="within")
summary(fixm_a2)
#negative and not even .05: -1.972(0.051)

#FIXED EFFECTS BEFORE HARV
#use 2021 prices
fixd_b <- plm(displace_20 ~ ndvi*price21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = not_done, index="distid", model="within")
summary(fixd_b)
#price21 slightly significant, nothing else is
fixm_b <- plm(max_dist_TO ~ price20*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = not_done, index="distid", model="within")
summary(fixm_b)
#bullshit

#checking how much of the sample displaces after the takeover but before the end of the harvest:
vec <- ifelse(not_done$utc_max_TO < not_done$end_harv_blend_unix,1,0)
not_done$inc_21 <- not_done$ndvi*not_done$price21
not_done$inc_20 <- not_done$ndvi*not_done$price20

last_try <- not_done[not_done$utc_max_TO < not_done$begin_harv_blend_unix,]

#forget these
fixd_b2 <- plm(displace_20 ~ inc_20 + ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = not_done, index="distid", model="within")
summary(fixd_b2)
#insignificant
fixm_b2 <- plm(max_dist_TO ~ inc_20 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = not_done, index="distid", model="within")
summary(fixm_b2)
#insignificant 

#FIXED EFFECTS FULL SAMPLE
fixd <- plm(displace_20 ~ ndvi*price20 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = unique_wheat, index="distid", model="within")
summary(fixd)
#not much going on here
fixm <- plm(max_dist_TO ~ price21*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = unique_wheat, index="distid", model="within")
summary(fixm_b)
#not much going on here

fixd_2 <- plm(displace_20 ~ mix_inc + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = unique_wheat, index="distid", model="within")
summary(fixd_2)
#for inc_20: -0.007(0.02163); inc_21 -0.007(0.013); mix_inc -0.007(0.0072)
fixm_2 <- plm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = unique_wheat, index="distid", model="within")
summary(fixm_2)
#I don't like that these are negative^^^ inc_21: -1.510(0.083) NOT SIG

##########################
#CHANGING THE NDVI CUTOFF#
##########################
#ends up being unsuccessful all around
full_25 <- unique_wheat[unique_wheat$ndvi >= .25,]
done_25 <- full_25[full_25$aft_harv_blend == 1,]
not_done_25 <- full_25[full_25$aft_harv_blend != 1,]

#DIFF MEANS AFT HARV (n = 2966)
diffmeanm_a_25 <- lm(max_dist_TO ~ inc_21,data = done_25)
summary(diffmeanm_a_25)
#significant 1.54(.04) --> lost significance
diffmeand_a_25 <- lm(displace_20 ~ inc_21,data = done_25)
summary(diffmeand_a_25)
#significant .007(.002) --> very similar and got worse

#DIFF MEANS BEFORE HARV (n = 421)
diffmeanm_b_25 <- lm(max_dist_TO ~ inc_21,data = not_done_25)
summary(diffmeanm_b_25)
#totally insignificant (ndvi, inc_20, and inc_21) --> still insignificant
diffmeand_b_25 <- lm(displace_20 ~ inc_21,data = not_done_25)
summary(diffmeand_b_25)
#totally insignificant (ndvi, inc_20, and inc_21) --> still insignificant

#DIFF MEANS FULL SAMPLE (n = 3386)
diffmeanm_25 <- lm(max_dist_TO ~ mix_inc, data = full_25)
summary(diffmeanm_25)
#significant 1.3(.03) --> insignificant
diffmeand_25 <- lm(displace_20 ~ mix_inc,data = full_25)
summary(diffmeand_25)
#significant .009(5.46e-06) --> very similar slightly worse


#OLS AFTER HARV
olsd_a_25 <- lm(displace_20 ~ ndvi*price21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = done_25)
summary(olsd_a_25)
#this one is weird: ndvi: -5.296(.015); price21 -0.0619(.006); ndvi*price21 .156(.013) --> about the same
olsm_a_25 <- lm(max_dist_TO ~ price21*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = done_25)
summary(olsm_a_25)
#again weird: ndvi: -2.943e+03(1.10e-05); price21 -2.561e+01(0.000247); ndvi*price21 8.512e+01(1.16e-05) --> about the same

olsd_a2_25 <- lm(displace_20 ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done_25)
summary(olsd_a2_25)
#insignificant --> no change
olsm_a2_25 <- lm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done_25)
summary(olsm_a2_25)
#insignificant --> no change

#OLS BEFORE HARV
olsd_b_25 <- lm(displace_20 ~ ndvi*price20 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = not_done_25)
summary(olsd_b_25)
#insignificant w/ price20 and price21 --> no change
olsm_b_25 <- lm(max_dist_TO ~ price20*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = not_done_25)
summary(olsm_b_25)
#insignificant w/ price20 and price21 --> no change

olsd_b2_25 <- lm(displace_20 ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = not_done_25)
summary(olsd_b2_25)
#insignificant w/ inc_20 and inc_21 --> no change
olsm_b2_25 <- lm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = not_done_25)
summary(olsm_b2_25)
#insignificant w/ inc_20 and inc_21 --> no change

#OLS FULL SAMPLE
olsd_25 <- lm(displace_20 ~ ndvi*price20 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = full_25)
summary(olsd_25)
#price is significant for all of mix_price, price20, price21, but never ndvi, interaction never significant --> no change
olsm_25 <- lm(max_dist_TO ~ price21*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = full_25)
summary(olsm_b_25)
#all totally insignificant --> no change


olsd_b2_25 <- lm(displace_20 ~ inc_20 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = full_25)
summary(olsd_b2_25)
#insignificant w/ inc_20 and inc_21 --> no change
olsm_b2_25 <- lm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = full_25)
summary(olsm_b2_25)
#insignificant w/ inc_20 and inc_21 --> no change

#FIXED EFFECTS AFTER HARVEST
fixd_a_25 <- plm(displace_20 ~ ndvi*price21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done_25, index="distid", model="within")
summary(fixd_a_25)
#this one is weird: ndvi: -6.269(0.023); price21 INSIG; ndvi*price21 0.171(0.032) --> got worse
fixm_a_25 <- plm(max_dist_TO ~ price21*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = done_25, index="distid", model="within")
summary(fixm_a_25)
#again weird: ndvi: -2.6778e+03(.0017); price21 INSIG; ndvi*price21 7.5491e+01(0.002) --> got worse

fixd_a2_25 <- plm(displace_20 ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done_25, index="distid", model="within")
summary(fixd_a2_25)
#NOT WHAT I'D THINK: -0.009(0.003) --> similar slightly worse
fixm_a2_25 <- plm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = done_25, index="distid", model="within")
summary(fixm_a2_25)
#negative and not even .05: -1.972(0.051) --> worse

#FIXED EFFECTS BEFORE HARV
#use 2021 prices
fixd_b_25 <- plm(displace_20 ~ ndvi*price21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = not_done_25, index="distid", model="within")
summary(fixd_b_25)
#price21 slightly significant, nothing else is --> became insignificant
fixm_b_25 <- plm(max_dist_TO ~ price20*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = not_done_25, index="distid", model="within")
summary(fixm_b_25)
#just bad for both


#forget these
fixd_b2_25 <- plm(displace_20 ~ inc_20 + ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = not_done_25, index="distid", model="within")
summary(fixd_b2_25)
#insignificant --> no change
fixm_b2_25 <- plm(max_dist_TO ~ inc_20 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = not_done_25, index="distid", model="within")
summary(fixm_b2_25)
#insignificant --> no change

#FIXED EFFECTS FULL SAMPLE
fixd_25 <- plm(displace_20 ~ ndvi*price20 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = full_25, index="distid", model="within")
summary(fixd_25)
#not much going on here --> no change
fixm_25 <- plm(max_dist_TO ~ price21*ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = full_25, index="distid", model="within")
summary(fixm_b_25)
#not much going on here --> no change

fixd_2_25 <- plm(displace_20 ~ mix_inc + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = full_25, index="distid", model="within")
summary(fixd_2_25)
#for inc_20: -0.007(0.02163); inc_21 -0.007(0.013); mix_inc -0.007(0.0072) --> very similar, still don't like the direction
fixm_2_25 <- plm(max_dist_TO ~ inc_21 + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist, data = full_25, index="distid", model="within")
summary(fixm_2_25)
#I don't like that these are negative^^^ inc_21: -1.510(0.083) NOT SIG --> no change

#MAKE THE TABLE
library(stargazer)
stargazer(diffmeanm_a, olsm_a, fixm_a, diffmeand_a, olsd_a, fixd_a, title="Results", align=TRUE)
stargazer(diffmeanm_b, olsm_b, fixm_b, diffmeand_b, olsd_b, fixd_b, title="Results", align=TRUE)

only$harv_len <- only$end_harv_blend_unix - only$begin_harv_blend_unix
only <- to_harv[to_harv$distid %in% wheat_d_vec,]
mean(only$harv_len)


#show difference in patterns between pre- and post- takeover
#OLS FULL SAMPLE
olsd <- lm(displace_20 ~ aft_harv_blend + ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = unique_wheat)
summary(olsd)
#price is significant for all of mix_price, price20, price21, but never ndvi
olsm <- lm(max_dist_TO ~ aft_harv_blend + ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = unique_wheat)
summary(olsm)
#all totally insignificant
fixd <- plm(displace_20 ~ aft_harv_blend + ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = unique_wheat, index="distid", model="within")
summary(fixd)
#price is significant for all of mix_price, price20, price21, but never ndvi
fixm <- plm(max_dist_TO ~ aft_harv_blend + ndvi + bef_sep_fat_sum + closest_cap + num_pings + kabul_dist,data = unique_wheat, index="distid", model="within")
summary(fixm)

stargazer(fixm, olsm, olsd, fixd, title="Results", align=TRUE)
