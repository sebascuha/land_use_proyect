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
getwd()
# Paths
DW_file <- "RawData/DW_pixels/"
list_land <- list.files(DW_file)
munis_path <-"RawData/MUNICIPIOS_GEODANE"

# 1. Load Data ------------------------------------------------------------
### Funtion to extract information from exportaed CSV from GEE. ###
pixels_by_grid<-function(df){
  for(i in 1:nrow(df)){
    all<-str_extract_all(df$histogram[i],pattern = "[0-9]=[0-9.]{1,}")
    if(length(all[[1]])>0){
      for(j in 1:length(all[[1]])){
        df[i,str_extract(all[[1]][j],pattern = "[0-9]{1,}")]<-str_extract(all[[1]][j],pattern = "[0-9.]{2,}")
      }
    } else{next}
    rm(all)
  }
  return(df)
}
## 2015
list_land15<-str_extract(list_land,pattern="DW_2015_[0-9]{1,}_[0-9]{1,}.csv")
list_land15<-list_land15[1:100]
## 2016
list_land16<-str_extract(list_land,pattern="DW_2016_[0-9]{1,}_[0-9]{1,}.csv")
list_land16<-list_land16[101:200]
## 2017
list_land17<-str_extract(list_land,pattern="DW_2017_[0-9]{1,}_[0-9]{1,}.csv")
list_land17<-list_land17[201:300]
## 2018
list_land18<-str_extract(list_land,pattern="DW_2018_[0-9]{1,}_[0-9]{1,}.csv")
list_land18<-list_land18[301:400]
## 2019
list_land19<-str_extract(list_land,pattern="DW_2019_[0-9]{1,}_[0-9]{1,}.csv")
list_land19<-list_land19[401:500]
## 2020
list_land20<-str_extract(list_land,pattern="DW_2020_[0-9]{1,}_[0-9]{1,}.csv")
list_land20<-list_land20[501:600]
## 2021
list_land21<-str_extract(list_land,pattern="DW_2021_[0-9]{1,}_[0-9]{1,}.csv")
list_land21<-list_land21[601:700]
## 2022
list_land22<-str_extract(list_land,pattern="DW_2022_[0-9]{1,}_[0-9]{1,}.csv")
list_land22<-list_land22[701:800]
# Union all lists of pixels counts data
list_lists<-list(list_land15,list_land16,list_land17,list_land18,
                 list_land19,list_land20,list_land21,list_land22)
names(list_lists)<-c("2015","2016","2017","2018","2019","2020","2021","2022")
rm(list_land,list_land15,list_land16,list_land17,list_land18,list_land19,
   list_land20,list_land21,list_land22)

# 2. Extracting all DW categories -----------------------------------------
for(j in 1:length(names(list_lists))){
  n=0
  start.time <- Sys.time() # Inizialize timer
  dw_pixels_cat<-data.frame() # Empty data frame to store data
  for(i in 1:length(list_lists[[names(list_lists)[j]]])){
    
    tmp_land<-read_csv(paste0(DW_file,list_lists[[names(list_lists)[j]]][i]),show_col_types = FALSE) # Load DW data
    tmp_land<-pixels_by_grid(tmp_land) # extracting histogram data
    tmp_land$year<-names(list_lists)[j] # Adding year
    write_fst(tmp_land,
              paste0("CreatedData/Temporary/DW_pixels/",list_lists[[names(list_lists)[j]]][i],".fst"),
              compress=100)
    
    if(i>1){
      dw_pixels_cat<-read_fst(paste0("CreatedData/Temporary/DW_pixels/all_grids_",names(list_lists)[j],".fst"))
      dw_pixels_cat<-bind_rows(dw_pixels_cat,tmp_land)
      write_fst(dw_pixels_cat,
                paste0("CreatedData/Temporary/DW_pixels/all_grids_",names(list_lists)[j],".fst"))
    }else{
      dw_pixels_cat<-tmp_land
      write_fst(dw_pixels_cat,
                paste0("CreatedData/Temporary/DW_pixels/all_grids_",names(list_lists)[j],".fst"))
    }
    rm(tmp_land,dw_pixels_cat)
   
    ## not core
    n=n+1
    gc()
  }
  end.time <- Sys.time()
  time.taken <-round(end.time-start.time,2)
  cat(n,"files were saved from year",names(list_lists)[j],"takes",time.taken)
}
## Load all years grids information
dw_pixels_files<-list.files("CreatedData/Temporary/Dw_pixels/",pattern="all_grids_[0-9]{1,}.fst")


dw_pixels<-data.frame() #Store data
for(i in 1:length(dw_pixels_files)){
  print(i)
  tmp<-read_fst(paste0("CreatedData/Temporary/Dw_pixels/",dw_pixels_files[i]))
  tmp<-tmp[,c("id_grids","histogram","0","1","2","3","4","5","6","7","8","year")]
  colnames(tmp)[3:11]<-c("water","trees","grass","flooded_vegetation","crops",
                         "Shrub_and_scrub","built","bare","snow_and_ice")
  dw_pixels<-rbind(dw_pixels,tmp)
  rm(tmp)
}
write_fst(dw_pixels,"CreatedData/Temporary/Dw_pixels/all_grids.fst")
#dw_pixels<-read_fst(paste0("CreatedData/Temporary/DW_pixels/all_grids.fst"))
gc()
# 3. Handle data ----------------------------------------------------------
dw_pixels<-dw_pixels[!duplicated(dw_pixels[,c("id_grids","year")]),] # Drop duplicated id_grids 
dw_pixels[,c(3:11)]<-sapply(dw_pixels[,c(3:11)], as.numeric) # Chatacter to numeric all pixel count
dw_pixels<-dw_pixels%>% # round pixel count
  mutate_at(vars(water,trees,grass,flooded_vegetation,crops,
                 Shrub_and_scrub,built,bare,snow_and_ice),
            round,2)
dw_pixels<-replace_na(dw_pixels, # replace NAs
                      list(water=0,trees=0,grass=0,flooded_vegetation=0,crops=0,
                           Shrub_and_scrub=0,built=0,bare=0,snow_and_ice=0))
dw_pixels$pixel_viewed<-rowSums(dw_pixels[,c(3:11)],na.rm=T) # Sum pixel data

# 4. Saving raw files ---------------------------------------------------------
# all categories
write_fst(dw_pixels,"CreatedData/Temporary/Dw_pixels/all_grids.fst")

### --- --- ####
#dw_pixels<-read_fst(paste0("CreatedData/Temporary/DW_pixels/all_grids.fst"))
### --- --- ####

# Filter data
colnames(dw_pixels)
dw_pixels_fil<-dw_pixels%>%select("id_grids",# Observation identification
                              "trees","grass","flooded_vegetation","crops", # Interest variables
                              "year", # Time identificator.
                              "pixel_viewed") # Sum pixels

# Only interest data
haven::write_dta(dw_pixels_fil,"CreatedData/Temporary/TS_dw_class_comimo_grids.dta")
rm(list = ls())
