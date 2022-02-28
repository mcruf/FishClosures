#################################################################################
#                                                                               #
#        Calculate the proportion of juveniles/adults in the closure areas      #
#                                                                               #
#################################################################################


# This script is to calculate the proportion of juveniles and adults
# in the nursery and spawner closure areas



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1)Load the nursery & spawning closure areas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Spawners shapefiles
#~~~~~~~~~~~~~~~~~~~~~~
setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Shapefile/Hotspots/Spawners/")


sbox2 <- readOGR(".","Spawner_box2")
proj4string(sbox2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#crs(sbox2) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 




# Recruits shapefiles
#~~~~~~~~~~~~~~~~~~~~~~
setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Shapefile/Hotspots/Recruits/")

rbox1 <- readOGR(".","Recruits_box1")
rbox2 <- readOGR(".","Recruits_box2")
rbox3 <- readOGR(".","Recruits_box3")


proj4string(rbox1) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
proj4string(rbox2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
proj4string(rbox3) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Load the commercial and survey data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Commercial data
#~~~~~~~~~~~~~~~~~~
comFULL <- readRDS("C:/users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Commercial/commercial_FULL_Age.rds")
colnames(comFULL)[8] <- "haulduration_hours" #Change name for convencience

comFULL$stock <- ifelse(comFULL$Area=="21","KAT","WBS"); comFULL$stock <- as.factor(comFULL$stock)
comFULL$YearQuarter <- paste(comFULL$Year,comFULL$Quarter,sep=":")

# Subset data only for WBS cod and for the most representative gears
commercial <- subset(comFULL, stock == "WBS" & metiers %in% c("OTB_DEF_>=105_1_110","OTB_DEF_>=105_1_120") &
                       Year %in% c("2015","2016","2017","2018","2019"))



#Calculate a 5+ group
#~~~~~~~~~~~~~~~~~~~~
commercial$age_5 <- rowSums(commercial[,c("age_5","age_6")])
commercial$age_6 <- NULL

commercial$Data <- paste("commercial")

commercial$age_0 <- 0 # create a dummy 0-group


commercial[,c("Month","Year","Quarter","YearQuarter","Area","metiers","HLID","stock")] <- lapply(commercial[,c("Month","Year","Quarter","YearQuarter","Area","metiers","HLID","stock")], factor)
#str(commercial)


# Survey data
#~~~~~~~~~~~~~~~~~~
survey <- readRDS("C:/users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Survey/survey_FULL_clean.rds") #same as the one used in the paper


survey$stock <- ifelse(survey$Area=="21","KAT","WBS")
survey$stock <- as.factor(survey$stock)

survey$Country <- substr(survey$haul.id, start = 8, stop = 10) #Country ID
survey$YearQuarter <- paste(survey$Year,survey$Quarter,sep=":")

survey$Data <- paste("survey")


# Subset data for WBS cod only
survey <- subset(survey, stock == "WBS" &  Year %in% c("2015","2016","2017","2018","2019"))

# Remove info from length groups
survey[,14:27] <- NULL

# Create an age group for A5+ (to few data from A4 onwards,actually)
survey$age_5 <- rowSums(survey[,19:22])
survey[,20:22] <- NULL

survey[,19][is.na(survey[,19])] <- 0


survey[,c("Month","Year","Quarter","Area","Data","YearQuarter","haul.id","Ship","Gear","Country")] <- lapply(survey[,c("Month","Year","Quarter","Area","Data","YearQuarter","haul.id","Ship","Gear","Country")], factor)
str(survey)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Retrieve data-specific information only for the closure areas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 3.1) First take the mean long/lat of the commercial hauls
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
commercial$lon_mean <- rowMeans(commercial[,c("lonStart","lonEnd")])
commercial$lat_mean <- rowMeans(commercial[,c("latStart","latEnd")])



# 3.2) Keep data points within the closures
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Commercial data
com_coords <- as.matrix(commercial[,39:40])
com_pts <- SpatialPoints(com_coords, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))



idx_sg2 <- which(rgeos::gIntersects(com_pts, sbox2, byid=TRUE)) #commercial with SPG2
idx_ng1 <- which(rgeos::gIntersects(com_pts, rbox1, byid=TRUE)) #commercial with NG1
idx_ng2 <- which(rgeos::gIntersects(com_pts, rbox2, byid=TRUE)) #commercial with NG2
idx_ng3 <- which(rgeos::gIntersects(com_pts, rbox3, byid=TRUE)) #commercial with NG3


commercial_SG2 <- commercial[idx_sg2,]
commercial_NG1 <- commercial[idx_ng1,]
commercial_NG2 <- commercial[idx_ng2,]
commercial_NG3 <- commercial[idx_ng3,]




## Survey data
sur_coords <- as.matrix(survey[,9:10])
sur_pts <- SpatialPoints(sur_coords, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))



idx_sg2_sur <- which(rgeos::gIntersects(sur_pts, sbox2, byid=TRUE)) #survey with SPG2
idx_ng1_sur <- which(rgeos::gIntersects(sur_pts, rbox1, byid=TRUE)) #survey with NG1
idx_ng2_sur <- which(rgeos::gIntersects(sur_pts, rbox2, byid=TRUE)) #survey with NG2
idx_ng3_sur <- which(rgeos::gIntersects(sur_pts, rbox3, byid=TRUE)) #survey with NG3




survey_SG2 <- survey[idx_sg2_sur,]
survey_NG1 <- survey[idx_ng1_sur,]
survey_NG2 <- survey[idx_ng2_sur,]
survey_NG3 <- survey[idx_ng3_sur,]




# 3.3) Now merge the age-specific information
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("C:/Users/mruf/Desktop/LGCP_MSPTOOLS/LGNB_test/utilities.R") # Set of functions that are used along the script


## SG2
age_com_sg2   <- transform(commercial_SG2, HaulDur=haulduration_hours, numYear = as.numeric( as.character(Year)))
age_sur_sg2   <- transform(survey_SG2, latStart=lat, lonStart=lon, latEnd=lat, lonEnd=lon,
                       HLID=haul.id, numYear = as.numeric(as.character(Year)))
datatot_sg2 <- mybind(age_com_sg2, age_sur_sg2)

datatot_sg2b <- datatot_sg2[,c("Area","Day","Month","Year","YearQuarter","Data","metiers",
                               "age_0","age_1","age_2","age_3","age_4","age_5",
                               "lon_mean","lat_mean","lon","lat","lonStart","lonEnd","latStart","latEnd")]




## NG1
age_com_ng1   <- transform(commercial_NG1, HaulDur=haulduration_hours, numYear = as.numeric( as.character(Year)))
age_sur_ng1   <- transform(survey_NG1, latStart=lat, lonStart=lon, latEnd=lat, lonEnd=lon,
                           HLID=haul.id, numYear = as.numeric(as.character(Year)))
datatot_ng1 <- mybind(age_com_ng1, age_sur_ng1)

datatot_ng1b <- datatot_ng1[,c("Area","Day","Month","Year","YearQuarter","Data","metiers",
                               "age_0","age_1","age_2","age_3","age_4","age_5",
                               "lon_mean","lat_mean","lon","lat","lonStart","lonEnd","latStart","latEnd")]



## NG2
age_com_ng2   <- transform(commercial_NG2, HaulDur=haulduration_hours, numYear = as.numeric( as.character(Year)))
age_sur_ng2   <- transform(survey_NG2, latStart=lat, lonStart=lon, latEnd=lat, lonEnd=lon,
                           HLID=haul.id, numYear = as.numeric(as.character(Year)))
datatot_ng2 <- mybind(age_com_ng2, age_sur_ng2)

datatot_ng2b <- datatot_ng2[,c("Area","Day","Month","Year","YearQuarter","Data","metiers",
                              "age_0","age_1","age_2","age_3","age_4","age_5",
                              "lon_mean","lat_mean","lon","lat","lonStart","lonEnd","latStart","latEnd")]

## NG3
age_com_ng3   <- transform(commercial_NG3, HaulDur=haulduration_hours, numYear = as.numeric( as.character(Year)))
age_sur_ng3   <- transform(survey_NG3, latStart=lat, lonStart=lon, latEnd=lat, lonEnd=lon,
                           HLID=haul.id, numYear = as.numeric(as.character(Year)))
datatot_ng3 <- mybind(age_com_ng3, age_sur_ng3)

datatot_ng3b <- datatot_ng3[,c("Area","Day","Month","Year","YearQuarter","Data","metiers",
                              "age_0","age_1","age_2","age_3","age_4","age_5",
                              "lon_mean","lat_mean","lon","lat","lonStart","lonEnd","latStart","latEnd")]



# 3.4) Put data in the long format
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Put data into longformat for ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyr)

## SG2
dfl_sg2 <- gather(datatot_ng2b, AgeGroup, Nage, age_0:age_5)

dfl_sg2$AgeGroup <- as.factor(dfl_sg2$AgeGroup)
levels(dfl_sg2$AgeGroup) <- c("A0","A1","A2","A3","A4","A5")
dfl_sg2$Data <- as.factor(dfl_sg2$Data)


## NG1
dfl_ng1 <- gather(datatot_ng1b, AgeGroup, Nage, age_0:age_5)

dfl_ng1$AgeGroup <- as.factor(dfl_ng1$AgeGroup)
levels(dfl_ng1$AgeGroup) <- c("A0","A1","A2","A3","A4","A5")
dfl_ng1$Data <- as.factor(dfl_ng1$Data)


## NG2
dfl_ng2 <- gather(datatot_ng2b, AgeGroup, Nage, age_0:age_5)

dfl_ng2$AgeGroup <- as.factor(dfl_ng2$AgeGroup)
levels(dfl_ng2$AgeGroup) <- c("A0","A1","A2","A3","A4","A5")
dfl_ng2$Data <- as.factor(dfl_ng2$Data)


## NG2
dfl_ng3 <- gather(datatot_ng3b, AgeGroup, Nage, age_0:age_5)

dfl_ng3$AgeGroup <- as.factor(dfl_ng3$AgeGroup)
levels(dfl_ng3$AgeGroup) <- c("A0","A1","A2","A3","A4","A5")
dfl_ng3$Data <- as.factor(dfl_ng3$Data)




## Go for the plot
ggplot(dfl_sg2) +
#add bar for each discipline colored by gender
# geom_bar(aes(x = AgeGroup, y = Nage, group = Data,fill=Data),
#          stat = "identity", position = "dodge") +
  geom_bar(aes(x = AgeGroup, y = Nage, group = Data,fill=Data),
           stat = "identity", position = "fill") +
facet_wrap(~Year,scales = "free") +
  # #ggplot(dfl_ng3,aes(x = AgeGroup, y = Nage, group = Data,fill=Data)) +
  # #stat_summary(fun.y="mean", geom="bar",position = "dodge") +
  # 
  #   ggplot(dfl_sg2,aes(x = AgeGroup, y = Nage, group = Data,fill=Data)) +
  #   stat_summary(fun.y="mean", geom="bar",position = "fill") +
    
    
    
  scale_fill_manual(values=c("#0073C2FF", "#EFC000FF")) +
  theme_pubclean() + 
  labs(x = "Age group", y="Catch (N)") +
  scale_x_discrete(labels=c("commercial"="Commercial", "survey"="Survey"))+
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15,face="bold"),
        legend.position = "bottom",
        
        axis.text = element_text(size=15),
        axis.title.y = element_text(margin=margin(t = 0, r = 18, b = 0, l = 0),size=18),
        axis.title.x = element_text(margin=margin(t=18, r=0, b=0,l=0),size=18),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(size = 15,face="bold"),
        
        plot.title = element_text(hjust = 0.5,size=14),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))




ggplot(Affairs) +
  aes(x = rating, fill = factor(halodrie)) +
  geom_bar(position = "fill")


df_sum_ng3 <- dfl_ng3 %>%
  group_by(Data, Year) %>%
  summarise(Freq = sum(Freq)) %>%
  group_by(Sex, Class) %>%
  mutate(Prop = Freq / sum(Freq))