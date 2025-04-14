# Clean environment
rm(list=ls())
# Load needed libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(sf,tidyverse,dplyr,stringr)
# Assign paths

#### Paths definition ####
if (Sys.info()[['user']] =='sebas'){
  main_path <- paste0("C:/Users/",Sys.info()[['user']],"/Dropbox/Documents/Proyecto_UsosDeTierra/")
  info_rev_path<- paste0("C:/Users/",Sys.info()[['user']],"/Dropbox/Research/Information_revealing/")
} else if (Sys.info()[['user']] == ''){
  main_path <- paste0("C:/Users/",Sys.info()[['user']],"/Dropbox/Documents/Proyecto_UsosDeTierra/")
  info_rev_path<- paste0("C:/Users/",Sys.info()[['user']],"/Dropbox/Research/Information_revealing/")
}

# Data paths
comimo_grids_path<-"CreatedData/new_grid_shapefiles/" # from informastion revealing path
useland_path<-"RawData/COMIMO/S2_grids/"
list_grids_files<-list.files(paste0(info_rev_path,comimo_grids_path),pattern="*.shp")
names<-str_extract(list_grids_files,pattern="[a-zA-Z0-9_]{1,}")

# Load Data
n=0
for(i in 1:length(list_grids_files)){
  n=n+1
  temp_grids<-st_read(paste0(info_rev_path,comimo_grids_path,list_grids_files[i]),quiet=T)
  a<-round(nrow(temp_grids)/10)
  factor<-c(rep(1,a),rep(2,a),rep(3,a),rep(4,a),rep(5,a),
            rep(6,a),rep(7,a),rep(8,a),rep(9,a),rep(10,a))
  factor<-factor[1:nrow(temp_grids)]
  temp_grids$divition<-factor
  list_tmp<-split(temp_grids,factor)
  print(n)
  for(j in 1:10){
    write_sf(list_tmp[[j]],paste0(main_path,useland_path,names[i],"_",as.character(j),".shp"))
    cat("\nSaving file",paste0(main_path,useland_path,names[i],"_",as.character(j),".shp"))
  }
  rm(temp_grids,a,list_tmp)
}

list_grids_files<-list.files(paste0(main_path,useland_path),pattern="*.shp")

n=0
nas<-C()
for(i in 1:length(list_grids_files)){
  n=n+1
  temp_grids<-st_read(paste0(main_path,useland_path,list_grids_files[i]),quiet=TRUE)
  nas[i]<-sum(is.na(temp_grids))
  print(i)
}
sum(nas)
