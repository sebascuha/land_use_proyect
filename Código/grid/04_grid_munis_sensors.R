# 0. Introduction ---------------------------------------------------------
# Clean environment
rm(list=ls())
# Load needed libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(sf,tidyverse,dplyr,stringr,fst)
# Assign paths

#### Paths definition ####
if (Sys.info()[['user']] == 'sebas'){
  main_path <- "C:/Users/sebas/Dropbox/Documents/Proyecto_UsosDeTierra/"
} else {}
setwd(main_path)

# Paths
munis_path <-"RawData/MUNICIPIOS_GEODANE/"
sensor_path<- "CreatedData/Temporary/IDEAM_sensors_coor/"
comimo_path<-"RawData/COMIMO/S2_grids/Uploaded/"
grids_files<-list.files(comimo_path,pattern="*.shp")

# 1. Handle data -----------------------------------------------------------
# Municipal shapefile
muni_sp<-read_sf(munis_path,quiet=TRUE)%>%
  select(coddepto=DPTO_CCDGO,codmpio=MPIO_CDPMP,
         namedepto=DPTO_CNMBR,namempio=MPIO_CNMBR,
         geometry)%>%
  st_transform(crs=9377)%>%
  mutate(codmpio=as.numeric(codmpio),
         coddepto=as.numeric(coddepto),
         )

# Sensors shapefile
sensor_sp<-st_read(sensor_path)%>%select(CODIGO,geometry)
# Grids loop intersection
for(file in 1:length(grids_files)){
  print(file)
  # Load grids divition
  tmp_grid_sp<-read_sf(paste0(comimo_path,grids_files[file]),quiet=TRUE)%>%
    select(-divition)%>%st_transform(crs=9377)
  # Getting grid centroid for easy municipality intersection
  tmp_grids_points<-st_centroid(tmp_grid_sp) 
  # Getting the nearest municipality id to the grid centroid.
  tmp_codmpios<-muni_sp[st_nearest_feature(tmp_grids_points,muni_sp),"codmpio"]
  tmp_grid_sp$codmpio<-tmp_codmpios$codmpio
    
  tmp_near_sensor<-data.frame()
  for(grid in 1:nrow(tmp_grid_sp)){
    tmp_sensor_distance_grid<-matrix(st_distance(tmp_grid_sp[grid,],sensor_sp),
                                     ncol=nrow(sensor_sp),nrow=1)
    colnames(tmp_sensor_distance_grid)<-sensor_sp$CODIGO
    tmp_sensor_distance_grid<-tmp_sensor_distance_grid[,order(tmp_sensor_distance_grid)]
    tmp_near_sensor<-rbind(tmp_near_sensor,names(tmp_sensor_distance_grid)[1:3])
  }
  colnames(tmp_near_sensor)<-c("near_s1","near_s2","near_s3")
  tmp_near_sensor$id_grids<-tmp_grid_sp$id_grids
  tmp_grid_sp<-cbind(tmp_grid_sp,tmp_near_sensor)
  # Saving distances by divition
  write_fst(tmp_near_sensor,
            paste0("CreatedData/Temporary/GRIDS-MUNI-SENSOR/DistancesByDivition/",
                   grids_files[file],".fst"),
            compress=100)
  
  # Saving dataset by divition
  write_fst(st_drop_geometry(tmp_grid_sp),
            paste0("CreatedData/Temporary/GRIDS-MUNI-SENSOR/ByDivition/",
                   grids_files[file],".fst"),
            compress=100)
  rm(tmp_grid_sp,tmp_grids_points,file,grid,tmp_codmpios,tmp_near_sensor)
}
####
## Adding all information stored above
grid_sensor_file<-list.files("CreatedData/Temporary/GRIDS-MUNI-SENSOR/ByDivition/")
grid_sensor<-data.frame() # Empty data set to store data
for(i in 1:length(grid_sensor_file)){
  tmp_grid<-read_fst(paste0("CreatedData/Temporary/GRIDS-MUNI-SENSOR/ByDivition/",
                            grid_sensor_file[i]))%>%select(-id_grids.1)
  grid_sensor<-rbind(grid_sensor,tmp_grid)
  print(i)
}

length(unique(grid_sensor$id_grids))

# 2. Saving data -------------------------------------------------------------
write_fst(grid_sensor,"CreatedData/Temporary/grids_sensors.fst")
