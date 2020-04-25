# A5 script
# RECS analysis file
# D. Cale Reeves
# UT Austin, Energy Systems Transformation Group
# Created: 2019/01/29
#
# 

#---------------#
# Run Parameters 
#---------------#
options("width"=180)

OutFile <- file("200315_SAL_A5q2_sol_DCR_Out.txt")
sink(OutFile, append=FALSE, type=c("output"))
sink(OutFile, append=FALSE, type=c("message"))
set.seed(1)

#setwd("/Users/dcr953/Desktop/Box Sync/Teaching/20_Spring/SAL/Assignments/A5")

#---------------#
# Libraries
#---------------#

library(ggmap)
library(MASS)
library(reshape2)
library(scales)
library(tree)

#---------------#
# Load Data
#---------------#

EnergyAudit <- read.csv(file="Map__2009-2013__ECAD_Residential_Energy_Audit_Data.csv")
#head(EnergyAudit)
#tail(EnergyAudit)
print(sprintf("EnergyAudit data has %d rows with %d columns", nrow(EnergyAudit), ncol(EnergyAudit)))

#---------------#
# Clean Data
#---------------#

#str(EnergyAudit)
EnergyAudit$Address <- as.character(EnergyAudit$Address)

EnergyAudit$Coords <- sapply(EnergyAudit$Address, function (i) {
	#print(i)
	#print(gregexpr(pattern ='\n',i))
	#print(gregexpr(pattern ='\n',i)[[1]][2])
	#print(nchar(i))
	#print(substr(i, gregexpr(pattern ='\n',i)[[1]][2]+1, nchar(i)))
	substr(i, gregexpr(pattern ='\n',i)[[1]][2]+1, nchar(i))
})

head(EnergyAudit$Coords)

EnergyAudit$Lat <- sapply(EnergyAudit$Coords, function (i) {
	#print(i)
	#print(gregexpr(pattern ='\n',i))
	#print(gregexpr(pattern =', ',i)[[1]][1])
	#print(nchar(i))
	#print(substr(i, 2, gregexpr(pattern =', ',i)[[1]][1]-1))
	as.numeric(substr(i, 2, gregexpr(pattern =', ',i)[[1]][1]-1))
})

head(EnergyAudit$Lat)

EnergyAudit$Long <- sapply(EnergyAudit$Coords, function (i) {
	#print(i)
	#print(gregexpr(pattern ='\n',i))
	#print(gregexpr(pattern =', ',i)[[1]][1])
	#print(nchar(i))
	#print(substr(i, gregexpr(pattern =', ',i)[[1]][1]+2, nchar(i)-1))
	as.numeric(substr(i, gregexpr(pattern =', ',i)[[1]][1]+2, nchar(i)-1))
})

#head(EnergyAudit)


EnergyAudit_clean <-  EnergyAudit[EnergyAudit$Duct.System.1...Type != "",]
nrow(EnergyAudit_clean)
#head(EnergyAudit_clean)

EnergyAudit_clean <- EnergyAudit_clean[,c("TCAD.or.WCAD.Property.ID..s.", 
	"Duct.System.1...Type", 
	"Duct.System.1...RValue", 
	"Duct.System.1...Return.Sizing", 
	"Duct.System.1.....Leakage",
	"System.1...Location.Air.Handler",
	"System.1...Air.Handler.Type",
	"System.1...Age..years.",
#	"System.1...EER",
	"System.1...sqft.ton",
	"Water.Heater...Fuel.Type",
	"Water.Heating...Tank.Type",
	"Toilet.Type",
	"Programmable.Thermostat.Present",
	"Window.Screen.Area.Recommended..sqft.",
	"Furnace...Fuel.Type",
	"Attic.R.Value",
	"Recommended.Additional.R.Value",
	"Year.Built",
	"Bedrooms",
	"Conditioned..sqft.",
	"Attic..sqft.",
	"Home_Type",
	"Lat",
	"Long")]
	
#head(EnergyAudit_clean)
EnergyAudit_clean <-  EnergyAudit_clean[!is.na(EnergyAudit_clean$System.1...Age..years.),]
EnergyAudit_clean$Attic..sqft.[is.na(EnergyAudit_clean$Attic..sqft.)] = 0
EnergyAudit_clean$Attic.R.Value[is.na(EnergyAudit_clean$Attic.R.Value)] = 0
EnergyAudit_clean$Recommended.Additional.R.Value[is.na(EnergyAudit_clean$Recommended.Additional.R.Value)] = 0

# summary(EnergyAudit_clean$Duct.System.1...Type, exclude=NULL) 
# summary(EnergyAudit_clean$Duct.System.1...RValue, exclude=NULL) 
# summary(EnergyAudit_clean$Duct.System.1...Return.Sizing, exclude=NULL) 
# summary(EnergyAudit_clean$Duct.System.1.....Leakage, exclude=NULL) 
# summary(EnergyAudit_clean$System.1...Location.Air.Handler, exclude=NULL) 
# summary(EnergyAudit_clean$System.1...Air.Handler.Type, exclude=NULL) 
# summary(EnergyAudit_clean$System.1...Age..years., exclude=NULL) 
# #summary(EnergyAudit_clean$System.1...EER, exclude=NULL) 
# summary(EnergyAudit_clean$System.1...sqft.ton, exclude=NULL) 
# summary(EnergyAudit_clean$Water.Heater...Fuel.Type, exclude=NULL) 
# summary(EnergyAudit_clean$Water.Heating...Tank.Type, exclude=NULL) 
# summary(EnergyAudit_clean$Toilet.Type, exclude=NULL) 
# summary(EnergyAudit_clean$Programmable.Thermostat.Present, exclude=NULL) 
# summary(EnergyAudit_clean$Window.Screen.Area.Recommended..sqft., exclude=NULL) 
# summary(EnergyAudit_clean$Furnace...Fuel.Type, exclude=NULL) 
# summary(EnergyAudit_clean$Attic.R.Value, exclude=NULL) 
# summary(EnergyAudit_clean$Recommended.Additional.R.Value, exclude=NULL) 
# summary(EnergyAudit_clean$Year.Built, exclude=NULL) 
# summary(EnergyAudit_clean$Bedrooms, exclude=NULL) 
# summary(EnergyAudit_clean$Conditioned..sqft., exclude=NULL) 
# summary(EnergyAudit_clean$Attic..sqft., exclude=NULL) 
# summary(EnergyAudit_clean$Home_Type, exclude=NULL) 
# summary(EnergyAudit_clean$Lat, exclude=NULL) 
# summary(EnergyAudit_clean$Long, exclude=NULL) 

nrow(EnergyAudit_clean)
nrow(na.omit(EnergyAudit_clean))

EnergyAudit_clean <- na.omit(EnergyAudit_clean)



print(sprintf("EnergyAudit_clean data: Longitude Range: (%f,%f). Width: %f", min(EnergyAudit_clean$Long),max(EnergyAudit_clean$Long),min(EnergyAudit_clean$Long) - max(EnergyAudit_clean$Long)))
print(sprintf("EnergyAudit_clean data: Latitude Range: (%f,%f). Height: %f", min(EnergyAudit_clean$Lat),max(EnergyAudit_clean$Lat),min(EnergyAudit_clean$Lat) - max(EnergyAudit_clean$Lat)))


LongRange = c(-98, -97.56)
LatRange = c(30.09, 30.47)

EnergyAudit_clean <- EnergyAudit_clean[EnergyAudit_clean$Long > min(LongRange) & EnergyAudit_clean$Long < max(LongRange), ]
EnergyAudit_clean <- EnergyAudit_clean[EnergyAudit_clean$Lat > min(LatRange) & EnergyAudit_clean$Lat < max(LatRange), ]

print(sprintf("Trimming map to: Latitude Range: (%f,%f),  Latitude Range: (%f,%f)", min(LatRange),max(LatRange),min(LongRange),max(LongRange)))

print(sprintf("EnergyAudit_clean data now has %d rows with %d columns", nrow(EnergyAudit_clean), ncol(EnergyAudit_clean)))


EnergyAudit_clean$set <- sample(c("train", "test"), size=nrow(EnergyAudit_clean), replace=TRUE, prob=c(0.70,0.30))
table(EnergyAudit_clean$set)

summary(EnergyAudit_clean$Duct.System.1.....Leakage)
EnergyAudit_clean$NeedsDuctFixed <- 0
EnergyAudit_clean$NeedsDuctFixed[EnergyAudit_clean$Duct.System.1.....Leakage > quantile(EnergyAudit_clean$Duct.System.1.....Leakage, probs=c(0.8)) ] = 1
EnergyAudit_clean$NeedsDuctFixed <- as.factor(EnergyAudit_clean$NeedsDuctFixed)
table(EnergyAudit_clean$NeedsDuctFixed, EnergyAudit_clean$set)

#---------------#
# Get KDE
#---------------#

N_dens <- 100

BandwidthMultiplier <- 20

LongIncrement <- (max(LongRange)-min(LongRange))/N_dens
LatIncrement <- (max(LatRange)-min(LatRange))/N_dens
# LongIncrement
# LatIncrement

EnergyAudit_cleanDensMap100 = kde2d(EnergyAudit_clean$Long, EnergyAudit_clean$Lat, h=c(BandwidthMultiplier*LongIncrement, BandwidthMultiplier*LatIncrement) , lims=c(LongRange, LatRange), n=N_dens)
NeedsDuctFixedDensMap100 = kde2d(EnergyAudit_clean$Long[EnergyAudit_clean$NeedsDuctFixed == 1], EnergyAudit_clean$Lat[EnergyAudit_clean$NeedsDuctFixed == 1], h=c(BandwidthMultiplier*LongIncrement, BandwidthMultiplier*LatIncrement) , lims=c(LongRange, LatRange), n=N_dens)
NotNeedsDuctFixedDensMap100 = kde2d(EnergyAudit_clean$Long[EnergyAudit_clean$NeedsDuctFixed == 0], EnergyAudit_clean$Lat[EnergyAudit_clean$NeedsDuctFixed == 0], h=c(BandwidthMultiplier*LongIncrement, BandwidthMultiplier*LatIncrement) , lims=c(LongRange, LatRange), n=N_dens)



print(sprintf("EnergyAudit_clean densities calculated for a %d square grid", N_dens))


#head(EmpDensMap100)
#length(EmpDensMap100)

# Now melt them to long format
EnergyAudit_cleanDensMap100.m = melt(EnergyAudit_cleanDensMap100$z, id.var=rownames(EnergyAudit_cleanDensMap100))
names(EnergyAudit_cleanDensMap100.m) = c("Long","Lat","z")

EnergyAudit_cleanDensMap100.m$adjLong <- (EnergyAudit_cleanDensMap100.m$Long * LongIncrement) + min(LongRange)
EnergyAudit_cleanDensMap100.m$adjLat<- (EnergyAudit_cleanDensMap100.m$Lat * LatIncrement) + min(LatRange)
EnergyAudit_cleanDensMap100.m$adjz <- rescale(EnergyAudit_cleanDensMap100.m$z, to = c(0,1))
EnergyAudit_cleanDensMap100.m$STDadjz <- scale(EnergyAudit_cleanDensMap100.m$z)



NeedsDuctFixedDensMap100.m = melt(NeedsDuctFixedDensMap100$z, id.var=rownames(NeedsDuctFixedDensMap100))
names(NeedsDuctFixedDensMap100.m) = c("Long","Lat","z")

NeedsDuctFixedDensMap100.m$adjLong <- (NeedsDuctFixedDensMap100.m$Long * LongIncrement) + min(LongRange)
NeedsDuctFixedDensMap100.m$adjLat<- (NeedsDuctFixedDensMap100.m$Lat * LatIncrement) + min(LatRange)
NeedsDuctFixedDensMap100.m$adjz <- rescale(NeedsDuctFixedDensMap100.m$z, to = c(0,1))
NeedsDuctFixedDensMap100.m$STDadjz <- scale(NeedsDuctFixedDensMap100.m$z)




NotNeedsDuctFixedDensMap100.m = melt(NotNeedsDuctFixedDensMap100$z, id.var=rownames(NotNeedsDuctFixedDensMap100))
names(NotNeedsDuctFixedDensMap100.m) = c("Long","Lat","z")

NotNeedsDuctFixedDensMap100.m$adjLong <- (NotNeedsDuctFixedDensMap100.m$Long * LongIncrement) + min(LongRange)
NotNeedsDuctFixedDensMap100.m$adjLat<- (NotNeedsDuctFixedDensMap100.m$Lat * LatIncrement) + min(LatRange)
NotNeedsDuctFixedDensMap100.m$adjz <- rescale(NotNeedsDuctFixedDensMap100.m$z, to = c(0,1))
NotNeedsDuctFixedDensMap100.m$STDadjz <- scale(NotNeedsDuctFixedDensMap100.m$z)

print(sprintf("EnergyAudit_clean densities standardized over %d square grid", N_dens))


#---------------#
# Make Graphics
#---------------#

#register_google(key = "<Register for and supply your own key to make awesome maps>")
#AustinMap <- ggmap(get_map(location = c(lon = -97.78, lat = 30.28), zoom = 11)) 

#save(AustinMap, file="AustinMap.Rdata")
load(file="AustinMap.Rdata")

BlankMap <- AustinMap
ggsave("AustinMap.png", AustinMap)


EnergyAudit_cleanDensPlot <- AustinMap +
	geom_tile(data=EnergyAudit_cleanDensMap100.m, aes(x = adjLong, y  = adjLat, z=z, fill=z, alpha=z)) +
	geom_contour(data=EnergyAudit_cleanDensMap100.m, aes(x = adjLong, y  = adjLat, z=z)) + 
 	scale_fill_gradient(low="green", high="red", name="EnergyAudit_clean\nDensity") +
  	coord_cartesian(xlim=LongRange, ylim=LatRange) +
  	guides(alpha=FALSE) + 
  	scale_x_continuous(name="Longitude") +
	scale_y_continuous(name="Latitude") +
	theme_bw()
ggsave("MapEnergyAudit_cleanDens.png", EnergyAudit_cleanDensPlot)

NeedsDuctFixedDensMap100DensPlot <- AustinMap +
	geom_tile(data=NeedsDuctFixedDensMap100.m, aes(x = adjLong, y  = adjLat, z=z, fill=z, alpha=z)) +
	geom_contour(data=NeedsDuctFixedDensMap100.m, aes(x = adjLong, y  = adjLat, z=z)) + 
 	scale_fill_gradient(low="green", high="red", name="Needs Duct Fixed\nDensity") +
  	coord_cartesian(xlim=LongRange, ylim=LatRange) +
  	guides(alpha=FALSE) + 
  	scale_x_continuous(name="Longitude") +
	scale_y_continuous(name="Latitude") +
	theme_bw()
ggsave("MapNeedsDuctFixedDensMap100Dens.png", NeedsDuctFixedDensMap100DensPlot)

NotNeedsDuctFixedDensMap100DensPlot <- AustinMap +
	geom_tile(data=NotNeedsDuctFixedDensMap100.m, aes(x = adjLong, y  = adjLat, z=z, fill=z, alpha=z)) +
	geom_contour(data=NotNeedsDuctFixedDensMap100.m, aes(x = adjLong, y  = adjLat, z=z)) + 
 	scale_fill_gradient(low="green", high="red", name="Not Needs Duct Fixed\nDensity") +
  	coord_cartesian(xlim=LongRange, ylim=LatRange) +
  	guides(alpha=FALSE) + 
  	scale_x_continuous(name="Longitude") +
	scale_y_continuous(name="Latitude") +
	theme_bw()
ggsave("MapNotNeedsDuctFixedDensMap100Dens.png", NotNeedsDuctFixedDensMap100DensPlot)

#---------------#
# Do Decision Trees
#---------------#

