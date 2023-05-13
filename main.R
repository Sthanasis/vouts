setwd('/Users/sakis/Desktop/Voutsis')
source("utils.R")

########  Points  ########
# Read Excel file
library(readxl)
library(sf)
library(mapview)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(ggplot2)
library(dplyr)

GNSS_df = read_excel("/Users/sakis/Desktop/Voutsis/files/GNSS_data_correction.xlsx")
# Re-install Rcpp package due to error (sf_library)
# install.packages('Rcpp')
# library(Rcpp)
#Convert data frame to SF points
GNSS_points = st_as_sf(x = GNSS_df, 
                       coords = c("LONGITUDE", "LATITUDE"),
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#Show the projection of the SF object
st_crs(GNSS_points)

# Tranform and SF object
st_transform(GNSS_points)
st_wrap_dateline(GNSS_points, options = "WRAPDATELINE=YES", quiet = TRUE)
# simple plot
plot(st_geometry(GNSS_points))

#mapview(GNSS_points)
# interactive map of SF points:
mapview(GNSS_points)
# convert to 'sp' object 
my.sp.point = as(GNSS_points, "Spatial")

########  SRTM  ########
srtm = raster("/Users/sakis/Desktop/Voutsis/files/srtm_41_04.tif")
# plot(srtm)

########  Tandem-X  ########
tandemx = raster("/Users/sakis/Desktop/Voutsis/files/TDM1_DEM__04_N40E023_DEM.tif")
# plot(tandemx)

########  Perioxi_Meletis  ########
shp = readOGR("/Users/sakis/Desktop/Voutsis/files", "periohi_meletis")
#den diavaze to periohi meletis.shp opote: https://community.rstudio.com/t/error-in-readogr-function-cannot-open-data-source/76202
# plot(shp)
# plot(srtm)
# plot(shp,add=T)
#den diavazei to shp otan einai apothikeumeno se egsa87, mono otan wgs84
r1 = crop(srtm, shp)
# plot(r1)
# plot(tandemx)
# plot(shp,add=T)
r2 = crop(tandemx,shp)
# plot(r2)
#apply water body mask (GIS--->R?)
#srtm
wb = readOGR("/Users/sakis/Desktop/Voutsis/files","limnes")
# plot(wb)
# plot(r1)
# plot(wb,add=T)
#tandem-x
# plot(wb)
# plot(r2)
# plot(wb,add=T)

#Extract Values from a Raster in R, https://www.neonscience.org/resources/learning-hub/tutorials/extract-values-rasters-r
options(stringsAsFactors=FALSE)
GNSS_points2 <- read.csv( "/Users/sakis/Desktop/Voutsis/files/GNSS_data_correction_test.csv", sep = ";")

GNSS_points2$LATITUDE = GNSS_points2$LATITUDE %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()
GNSS_points2$LONGITUDE = GNSS_points2$LONGITUDE %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()
GNSS_points2$ELEVcorr_a = GNSS_points2$ELEVcorr_a %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()
# pernei tis prwtes 6 seires
head(GNSS_points2) # validation
my.sf.point <- st_as_sf(x = GNSS_points2, 
                        coords = c("LONGITUDE", "LATITUDE"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# CONVERT GNSS POINT TO SPATIAL POINT FOR DATA EXTRACTION
my.sp.point2 = as(my.sf.point, "Spatial")
GNSS_data <- crop(my.sp.point2,shp)
# ============================ 
myCol <- terrain.colors(7)
#Plot DEM μαζί με τα σημειακά δεδομένα GNSS
plot(r2, col=myCol, main="SRTM with GNSS points plotted");par(new=TRUE)
# gp = readOGR("/Users/sakis/Desktop/Voutsis/files","gnss_points_study_area")
plot(GNSS_data, add = T)
head(GNSS_data) # validation
#Data extraction (TandemX_DEM)
extractdata <- raster::extract(r2, GNSS_data, weights=FALSE) # monodiastatos
df = data.frame(extractdata)
View(df)
#Plot data (TandemX_DEM)
df$id <- seq.int(nrow(df))
View(df)
x1 <- df$id
y1 <- df$extractdata
plot(x1,y1, main="Elevation_DEM", col.main="black", font.main=4, xlab = "GNSS", ylab="Elevation", type="l")
#Plot data (GNSS)
df_gp <-data.frame(GNSS_data)
View(df_gp)
df_gp$id <- seq.int(nrow(df_gp))
View(df_gp)
x2 <- df_gp$id
y2 <- df_gp$ELEVcorr_a
plot(x2, y2, main="Elevation_GNSS", col.main="black", font.main=4, xlab = "GNSS", ylab="Elevation", type="l")
#Plot both and compare: TandemX_GNSS
# x1 <- df$id
# y1 <- df$extractdata
# plot(x1, y1, main="Elevation Comparison: TandemX_DEM/GNSS", col.main="black", font.main=4, xlab = "GNSS points", ylab="Elevation", type="l", xlim=c(0,39982), ylim=c(0,1000))
x2 <- df_gp$id
y2 <- df_gp$ELEVcorr_a
lines(x2, y2, col="red")
legend(x=31000, y=900, c("TandemX_DEM","GNSS"), cex=.8, col=c("black","red"), lty=c(1,1))
#Data extraction (SRTM_DEM)
extractdata1 <- raster::extract(r1, GNSS_data, weights=FALSE)
df1 = data.frame(extractdata1)
View(df1)
#Plot data (SRTM_DEM)
df1$id <- seq.int(nrow(df1))
View(df1)
x3 <- df1$id
y3 <- df1$extractdata1
plot(x3, y3, main="Elevation Comparison: SRTM_DEM/GNSS", col.main="black", font.main=4, xlab = "GNSS", ylab="Elevation", type="l", xlim=c(0,39982), ylim=c(0,1000))
#Plot both and compare: SRTM_GNSS
x2 <- df_gp$id
y2 <- df_gp$ELEVcorr_a
lines(x2, y2, col="red")
legend(x=31000, y=900, c("SRTM_DEM","GNSS"), cex=.8, col=c("black","red"), lty=c(1,1))
#Plot both and compare: SRTM_TandemX
x1 <- df$id
y1 <- df$extractdata
plot(x1, y1, main="Elevation Comparison: TandemX/SRTM", col.main="black", font.main=4, xlab = "GNSS points", ylab="Elevation", type="l", xlim=c(0,39982), ylim=c(0,1000))
x3 <- df1$id
y3 <- df1$extractdata1
lines(x3, y3, col="red")
legend(x=31000, y=900, c("TandemX","SRTM"), cex=.8, col=c("black","red"), lty=c(1,1))
#step7: resample raster (validating raster)
r = resample(r1, r2, method="bilinear")
plot(r)
# Main process
good_data = extractdata
bad_data = extractdata1
real_data = GNSS_data$ELEVcorr_a %>% gsub(pattern = ",", replacement = ".", fixed=T) %>% as.numeric()
# == STEP 1 ==
len = length(real_data)
# ABSOLUTE DIFFERENCE
absolute_diffs_rasters = get_list_point_difference(good_data,bad_data) # difference between good and bad raster
absolute_diffs_gr_rd = get_list_point_difference(real_data,good_data) # difference between good raster and GNSS
absolute_diffs_br_rd = get_list_point_difference(real_data,bad_data) # difference between bad raster and GNSS

# RELATIVE DIFFERENCE
## create lists with random points
sample = generate_random_list(100,1,len) # get sample at random
random_REAL_points = get_random_geo_points(sample,real_data)
random_GOOD_points = get_random_geo_points(sample,good_data)
random_BAD_points = get_random_geo_points(sample,bad_data)
## get vector differences for each data list
real_vector_diffs = get_point_difference(random_REAL_points)
good_vector_diffs = get_point_difference(random_GOOD_points)
bad_vector_diffs = get_point_difference(random_BAD_points)
## get relative differences 
relative_diffs_rasters = get_list_point_difference(good_vector_diffs,bad_vector_diffs) # difference between good and bad raster
relative_diffs_gr_rd = get_list_point_difference(good_vector_diffs,real_vector_diffs) # difference between good raster and GNSS
relative_diffs_br_rd = get_list_point_difference(bad_vector_diffs,real_vector_diffs) # difference between bad raster and GNSS
# == STEP 2 ==
## Absolute Statistics
# Raster vs Raster 
rvr_abs_df = get_statistiscs(absolute_diffs_rasters)
View(rvr_abs_df)
# GNSS vs Good Raster 
rdvgr_abs_df = get_statistiscs(absolute_diffs_gr_rd)
View(rdvgr_abs_df)
# GNSS vs Bad Raster 
rdvbr_abs_df = get_statistiscs(absolute_diffs_br_rd)
View(rdvbr_abs_df)
## Relative Statistics
# Raster vs Raster 
rvr_rel_df = get_statistiscs(relative_diffs_rasters)
View(rvr_rel_df)
# GNSS vs Good Raster 
rdvgr_rel_df = get_statistiscs(relative_diffs_gr_rd)
View(rdvgr_rel_df)
# GNSS vs Bad Raster 
rdvbr_rel_df = get_statistiscs(relative_diffs_br_rd)
View(rdvbr_rel_df)
# == STEP 3 ==
# FREQUENCY AND CUMULATIVE FREQUENCY HISTOGRAMS
# Absolute diffs
create_histogramm(absolute_diffs_rasters,"Absolute Elevation Error Comparison: TandemX/SRTM", x_title= "Elevation Error" , y_title ="Frequency")
create_histogramm(absolute_diffs_gr_rd,"Absolute Elevation Error Comparison: TandemX/GNSS", x_title= "Elevation Error" , y_title ="Frequency")
create_histogramm(absolute_diffs_br_rd,"Absolute Elevation Error Comparison: SRTM/GNSS", x_title= "Elevation Error" , y_title ="Frequency")
# Relative diffs
create_histogramm(relative_diffs_rasters,"Relative Elevation Error Comparison: TandemX/SRTM", x_title= "Elevation Error" , y_title ="Frequency")
create_histogramm(relative_diffs_gr_rd,"Relative Elevation Error Comparison: TandemX/GNSS", x_title= "Elevation Error" , y_title ="Frequency")
create_histogramm(relative_diffs_br_rd,"Relative Elevation Error Comparison: SRTM/GNSS", x_title= "Elevation Error" , y_title ="Frequency")

# == STEP 4 ==