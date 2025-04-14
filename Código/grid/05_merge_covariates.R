# Intro -------------------------------------------------------------------
# Clean evinonment
rm(list=ls())
# Libraries
library(pacman)
p_load(tidyverse,sf,fst,readr)

# Setting relevant paths and working directory
if(Sys.info()["user"]=="sebas"){
  main_path <- paste0("C:/Users/",Sys.info()["user"],
                      "/Dropbox/Documents/Proyecto_UsosDeTierra/")
}else{
  main_path <- paste0("C:/Users/",Sys.info()["user"],
                      "") # Pon el resto de la direccion donde se encuentra la informacion en tu computador
}

setwd(main_path)
master_EVA <-"CreatedData/Temporary/TS_ciclo_siembra_cultivo_ha.R"
master_dw <-"CreatedData/Temporary/TS_dw_class_comimo_grids.dta"
grid_sensor_path<-"CreatedData/Temporary/grids_sensors.fst"
ideam_path <- "CreatedData/Temporary/IDEAM_sensor_TS.csv"
munis_path <- "RawData/MUNICIPIOS_GEODANE/"
sensors_path <- "CreatedData/Temporary/IDEAM_sensors_coor"
covariates_name <-"CreatedData/Temporary/Covariates_usosuelo.csv"

# 1. Load datasets -----------------------------------------------------------
# a.Load grid_sensor data to merge land clasification with IDEAM precipitation information
grid_sensor<-fst::read_fst(grid_sensor_path)[1:40000,]
# b.Load IDEAM sensor TS
ideam<-read_csv(ideam_path)%>%select(CODIGO,ano,anual_precip_mm)

  # Handle data - merge grid_sensor with ideam to triangulate precipitation
  colnames(grid_sensor)
  colnames(ideam)
  # Sensor 1 near
  grid_sensor<-grid_sensor%>%mutate(CODIGO=as.numeric(near_s1))
  mg1_grid_dp<-left_join(grid_sensor,ideam,
                         by="CODIGO",multiple="all")%>%
    arrange(id_grids,ano)%>%drop_na(ano)
  # Sensor 2 near
  grid_sensor<-grid_sensor%>%mutate(CODIGO=as.numeric(near_s2))
  mg2_grid_dp<-left_join(grid_sensor,ideam,
                         by="CODIGO",multiple="all")%>%
    arrange(id_grids,ano)%>%drop_na(ano)
  # Sensor 3 near
  grid_sensor<-grid_sensor%>%mutate(CODIGO=as.numeric(near_s3))
  mg3_grid_dp<-left_join(grid_sensor,ideam,
                         by="CODIGO",multiple="all")%>%
    arrange(id_grids,ano)%>%drop_na(ano)
  
  colnames(mg1_grid_dp)[8]<-"anual_precip_mm_S1"
  colnames(mg2_grid_dp)[8]<-"anual_precip_mm_S2"
  colnames(mg3_grid_dp)[8]<-"anual_precip_mm_S3"
  
  mg1_grid_dp<-mg1_grid_dp%>%select(-CODIGO)
  colnames(mg1_grid_dp)
  mg2_grid_dp<-mg2_grid_dp%>%select(-CODIGO)
  colnames(mg2_grid_dp)
  mg3_grid_dp<-mg3_grid_dp%>%select(-CODIGO)
  colnames(mg3_grid_dp)
  
  # Merging dqata from Sensor 1 with sensor 2
  mg1_mg2_grids<-full_join(mg1_grid_dp,mg2_grid_dp,
                           by=c("id_grids","codmpio","near_s1","near_s2","near_s3","ano"))
  # Merging data from Sensor 1 and sensor 2 with sensor 3
  mg_fn_grids<-full_join(mg1_mg2_grids,mg3_grid_dp,
                         by=c("id_grids","codmpio","near_s1","near_s2","near_s3","ano"))
  # Clean environment
  rm(mg1_grid_dp,mg2_grid_dp,mg3_grid_dp,mg1_mg2_grids)
  gc()
  colnames(mg_fn_grids)
  mg_fn_grids_sum<-mg_fn_grids%>%
    group_by(id_grids,codmpio)%>%
    summarise(freq=n(),
              NAS_s1=sum(is.na(anual_precip_mm_S1)),
              NAS_s2=sum(is.na(anual_precip_mm_S2)),
              NAS_s3=sum(is.na(anual_precip_mm_S3)),
              max_t=max(ano),
              min_t=min(ano),
              )%>%
    mutate(dum=ifelse(NAS_s1!=0&NAS_s2!=0&NAS_s3!=0,1,0))
  # get with dummy
  mg_fn_grids_sum_d<-mg_fn_grids_sum[mg_fn_grids_sum$dum==1,]
