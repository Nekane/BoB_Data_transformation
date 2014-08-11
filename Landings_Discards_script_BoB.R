#-----------------------------------------------------------------------------------
# Transforming the DISCARDS and LANDINGS data sets to FLR structure
# Nekane Alzorriz
# May 2014
#-----------------------------------------------------------------------------------

#Libraries used:
library(plyr)
library(FLCore)
library(reshape)
library(reshape2)
library(ggplot2)
library (gdata)

#-----------------------------------------------------------------------------------
# VARIABLES 
season<- c(1:4)
year<- c(2003:2012)
area<- c("VI", "VII","VIIIabd")
gear<- c("OTB", "PTB")

#-----------------------------------------------------------------------------------

# NOTES:
# The data sets contain too many areas, it is not worth to try to assess such detailled discardind behaviour (in such small area).
# We think that is better to aggragate the subareas to the main areas: VI, VII and VIII
# This approach will simplify the process to assign the annual quota. Iriondo et al., (2013) reflects that this approach 
# allows to see the fleets dynamics with the available fishing opportunities. Because the annual quota is
# shared out by area, not by subarea.

# DISCARDS DATA;
# Checking the data, for area VI and VII, there is data for subareas VIa and VIIj. Those subareas have data 
# just for the year 2003, when there was no data available for the respective main areas. So I just change the name
# (VIa= VI and VIIj= VII). 
# For area VIII occurs the same, but we have data during the 2003 in two subareas. I proceed to sum them.
# I have checked the total number of trips, and are completely independent one from each other.

# The ICES area consist on all the areas aggregated. I have this information just for the 2006 due to a problem in the data out
# I just delete it.

#-----------------------------------------------------------------------------------
##DISCARDS WEIGHT
#-----------------------------------------------------------------------------------
# Setting my path!
path <-("~/Documents/BoB/BoB_data/data/DISCARDS_NA/weight/")
filenames <- paste0(path, list.files(path, pattern = ".csv"))
discards.wt = ldply(filenames, function(filename) {
  dum = read.table(filename,header=T, dec=".", sep=";")
  
  #If you want to add the filename as well on the column
  
  dum$filename = filename
  return(dum)
})


#-----------------------------------------------------------------------------------
# DISCARDS files: need to change dec.; name of gear
#-----------------------------------------------------------------------------------

discards.wt[,12] <- sub("~/Documents/BoB/BoB_data/data/DISCARDS_NA/weight/", "", discards.wt[,12])
discards.wt[,12] <- as.numeric(sub("_discards.csv", "", discards.wt[,12]))

#unit = gear
names(discards.wt)<-c("unit","area","season","Officialspp","Meanweightrip","Variancetrip","Totalweightotaltrips",
                      "Totalvariance","CV","Sampletrips","Totaltrips","year")


discards.wt[discards.wt=="#VALUE!"]<-NA
discards.wt[discards.wt=="#DIV/0!"]<-NA
discards.wt[discards.wt=="N.A."]<-NA

discards.wt[,5:8]<-apply(apply(discards.wt[,5:8], 2, gsub, patt=",", replace=""), 2, as.numeric)

discards.wt<-with(discards.wt,data.frame(unit, area, season, Officialspp, Totalweightotaltrips,Sampletrips, Totaltrips, year))

# Renaming the gears...they haven't got the same names along the study period
levels(discards.wt$unit)
#"Bakas"       "Parejas GAV" "OTB"         "PTB"  
discards.wt[discards.wt=="Bakas"]<-'OTB'
discards.wt[discards.wt=="Parejas GAV"]<-'PTB'
discards.wt$unit <- discards.wt$unit[drop=TRUE]
levels(discards.wt$unit)
# "OTB" "PTB"

# Renaming the areas...we aggregate them by areas VI, VII and VIIIabd (not subareas)
levels(discards.wt$area)
#"VIa"     "VIIIa"   "VIIIb"   "VIIj"    "VIa"      "VII"     "VIIIabd" "ICES"    
discards.wt[discards.wt=="VIa"]<-'VI'
discards.wt[discards.wt=="VIIj"]<-'VII'
discards.wt <- subset(discards.wt, !(area %in% c("ICES")))


#Sum the data for 2003 for the areas VIIIa and VIIIb
area_VIII <- subset(discards.wt, (area %in% c("VIIIa","VIIIb")))
area_VIII$area<- "VIIIabd"

aaa<- dcast(area_VIII,unit+season+Officialspp+year ~area, value.var= "Totalweightotaltrips", 
            fun.aggregate=sum,na.rm=TRUE)
bbb<- dcast(area_VIII,unit+season+Officialspp+year ~area, value.var= "Sampletrips",
            fun.aggregate=sum,na.rm=TRUE)
ccc<- dcast(area_VIII,unit+season+Officialspp+year ~area, value.var= "Totaltrips",
            fun.aggregate=sum,na.rm=TRUE)

area_VIII<-aaa
area_VIII$Sampletrips<-bbb$VIIIabd
area_VIII$Totaltrips<-ccc$VIIIabd
area_VIII$area<- "VIIIabd"

names(area_VIII)<- c('unit', 'season', 'Officialspp', 'year', 'Totalweightotaltrips','Sampletrips', 'Totaltrips','area' )
# order in the same way as the other data frame to merge them
area_VIII<-subset(area_VIII,select=c(1,8,2,3,5,6,7,4))


## I remove the VIIIa and VIIIb data to merge after the sum of both
discards.wt <- subset(discards.wt, !(area %in% c("VIIIa","VIIIb")))
discards.wt <- rbind(discards.wt, area_VIII)

discards.wt$area <- discards.wt$area[drop=TRUE]
levels(discards.wt$area)
# "VI"      "VII"     "VIIIabd"

discards.wt<- discards.wt[discards.wt$Totalweightotaltrips>0,]
discards_wt_tot<-discards.wt

rm(aaa,bbb,ccc,area_VIII,filenames,path,discards.wt)

# dsc<- aggregate( cbind( Totalweightotaltrips) ~ Officialspp , data =discards_wt , FUN = sum )
# FAO<- read.table ("~/Documents/BoB/BoB_data/data/Untitled Folder/fao.csv", header=T, dec=".", sep=";")
# FAO[FAO==999]<-NA
# dsc[,3]<-FAO
# names(dsc)<-c("Officialspp","Discards","FAO")


#-----------------------------------------------------------------------------------
##DISCARDS LENGTH DISTRIBUTION
#-----------------------------------------------------------------------------------

##DISCARDS Length Distribution: need to change de NA by hand, also change headers and dec. ---> discards_2012.csv
path <-("~/Documents/BoB/BoB_data/data/DISCARDS_NA/lenCls/")
filenames <- paste0(path, list.files(path, pattern = ".csv"))
discards_lenCls = ldply(filenames, function(filename) {
  dum = read.table(filename,header=T, dec=".", sep=";")
  
  #If you want to add the filename as well on the column
  
  dum$filename = filename
  return(dum)
})

##DISCARDS length files: need to change dec.
#-----------------------------------------------------------------------------------

discards_lenCls[,10] <- sub("~/Documents/BoB/BoB_data/data/DISCARDS_NA/lenCls/", "", discards_lenCls[,10])
discards_lenCls[,10] <- as.numeric(sub("_discards_length.csv", "", discards_lenCls[,10]))

# unit= gear
names(discards_lenCls)<-c("unit","area","season","Officialspp","Lengt","Ret_n","Ret_SOP",
                          "Disc_n","Disc_SOP","year")

discards_lenCls[discards_lenCls=="#VALUE!"]<-NA
discards_lenCls[discards_lenCls=="#DIV/0!"]<-NA
discards_lenCls[discards_lenCls=="N.A."]<-NA

discards_lenCls[,6:9]<-apply(apply(discards_lenCls[,6:9], 2, gsub, patt=",", replace=""), 2, as.numeric)

# Renaming the gears...they haven't got the same names along the study period
levels(discards_lenCls$unit)
#"Bakas"       "Parejas GAV" "OTB"         "PTB"  
discards_lenCls[discards_lenCls=="Bakas"]<-'OTB'
discards_lenCls[discards_lenCls=="Parejas GAV"]<-'PTB'
discards_lenCls$unit <- discards_lenCls$unit[drop=TRUE]
levels(discards_lenCls$unit)
# "OTB" "PTB"


# Renaming the areas...we aggregate them by areas VI, VII and VIIIabd (not subareas)
levels(discards_lenCls$area)
#"VIa"     "VIIIa"   "VIIIb"   "VIIj"    "VIa"      "VII"     "VIIIabd" "ICES"    
discards_lenCls[discards_lenCls=="VIa"]<-'VI'
discards_lenCls[discards_lenCls=="VIIj"]<-'VII'
discards_lenCls <- subset(discards_lenCls, !(area %in% c("ICES")))


#Sum the data for 2003 for the areas VIIIa and VIIIb
area_VIII_lenCls <- subset(discards_lenCls, (area %in% c("VIIIa","VIIIb")))
area_VIII_lenCls$area<- "VIIIabd"

aaa<- dcast(area_VIII_lenCls,unit+season+Officialspp+year+Lengt ~area, value.var= "Ret_n",
            fun.aggregate=sum, na.rm=TRUE)
bbb<- dcast(area_VIII_lenCls,unit+season+Officialspp+year+Lengt ~area, value.var= "Ret_SOP",
            fun.aggregate=sum, na.rm=TRUE)
ccc<- dcast(area_VIII_lenCls,unit+season+Officialspp+year+Lengt ~area, value.var= "Disc_n",
            fun.aggregate=sum, na.rm=TRUE)
ddd<- dcast(area_VIII_lenCls,unit+season+Officialspp+year+Lengt ~area, value.var= "Disc_SOP",
            fun.aggregate=sum, na.rm=TRUE)


area_VIII_lenCls<-aaa
area_VIII_lenCls$Ret_SOP<-bbb$VIIIabd
area_VIII_lenCls$Disc_n<-ccc$VIIIabd
area_VIII_lenCls$Disc_SOP<-ddd$VIIIabd
area_VIII_lenCls$area<- "VIIIabd"

names(area_VIII_lenCls)<- c('unit', 'season', 'Officialspp', 'year', 'Lengt', 'Ret_n','Ret_SOP',
                            'Disc_n', 'Disc_SOP','area' )

# order in the same way as the other data frame to merge them
area_VIII_lenCls<-subset(area_VIII_lenCls,select=c(1,10,2,3,5,6,7,8,9,4))


## I remove the VIIIa and VIIIb data to merge after the sum of both
discards_lenCls <- subset(discards_lenCls, !(area %in% c("VIIIa","VIIIb")))
discards_lenCls <- rbind(discards_lenCls, area_VIII_lenCls)

discards_lenCls$area <- discards_lenCls$area[drop=TRUE]
levels(discards_lenCls$area)
# "VI"      "VII"     "VIIIabd"

discards_lenCls<- discards_lenCls[!(discards_lenCls$Ret_n==0 &discards_lenCls$Disc_n==0),]

rm(aaa,bbb,ccc,ddd,area_VIII_lenCls,filenames,path)

#-----------------------------------------------------------------------------------
# LANDINGS WEIGHT
#-----------------------------------------------------------------------------------
skiplines<- c(9,10,10,10,10)
years<-c(2003:2007)

readXlsSheet <- function(whichSheet, files){
  for(i in seq(files)){
    piece <- read.xls(files[i], sheet=2, skip=skiplines[j], header = FALSE)
    #rbinding frames should work if the sheets are similar, use merge if not.
    if(i == 1) complete <- piece else complete <- rbind(complete, piece)
  }
  complete
}

for (j in 1:length(years)){
  path<-paste("~/Documents/BoB/BoB_data/data/LANDINGS_NA/",years[j],sep="")
  filenames <- list.files( path, pattern="\\.xls$", full.names=TRUE)
  amountOfSheets <- 1 #imput something
  #This snippet gets the first sheet out of all the files and combining them 
  if (j==1){
    landings <- as.data.frame(lapply(amountOfSheets, readXlsSheet, files=filenames))
    names(landings)<- c('Port','season', 'year', 'area','unit', 'Officialspp', 
                        'landings_gutted','landings')
    assign(paste("landings",years[j],sep="_"),landings)
  }else if (j==4){
    landings<- as.data.frame(lapply(amountOfSheets, readXlsSheet, files=filenames))
    names(landings)<- c('Port',"month",'season', 'year', 'area','unit',
                        'Officialspp', 'landings_gutted','landings')
    # Calcular los quareters para los datos que faltan
    landings$season<-(landings$month+2)%/%3
    landings<- landings[,-2]
    assign(paste("landings",years[j],sep="_"),landings)
  }else if (j==5){
    landings<- as.data.frame(lapply(amountOfSheets, readXlsSheet, files=filenames))
    names(landings)<- c('Port','season', 'year','subarea', 'area','unit', 
                        'Officialspp', 'landings_gutted','landings')
    # Calcular los quareters para los datos que faltan
    landings<- landings[,-4]
    assign(paste("landings",years[j],sep="_"),landings)   
  }else {
    landings<- lapply(amountOfSheets, readXlsSheet, files=filenames)
    landings<-as.data.frame(landings)
    landings<- landings[,-1]
    names(landings)<- c('Port','season', 'year', 'area','unit', 'Officialspp',
                        'landings_gutted','landings')
    assign(paste("landings",years[j],sep="_"),landings)
  }
}

# JOin all the data set in a single data frame
landings_BoB<- rbind(landings_2003, landings_2004, landings_2005, landings_2006, landings_2007)

landings_BoB[,7:8]<-apply(apply(landings_BoB[,7:8], 2, gsub, patt=",", replace=""), 2, as.numeric)
landings_BoB<-with(landings_BoB,data.frame(unit, area, season, Officialspp, landings, year))

rm(landings,landings_2003, landings_2004, landings_2005, landings_2006, landings_2007,
   filenames,path,amountOfSheets, skiplines,years,j,readXlsSheet)

# Renaming the gears...they haven't got the same names along the study period
levels(landings_BoB$unit)
# [1] "Bakas"                  "Cerco"                  "Enmalle Bajura"        
# [4] "Palangre de Fondo"      "Palangre de Superficie" "Parejas GAV"           
# [7] "Volanta Comunidad"      "Importadores"           "Pintxo"                
# [10] "Txipironeras"           "Lineas de Mano"         "Tr\xedo GAV"           
# [13] "Desconocido"            "Nasas"                  "Cebo Vivo"             
# [16] "Palangre de Lija"

# WHICH are the anual landings for each unit??
landings_BoB$unit<-as.factor(landings_BoB$unit)
landings_BoB$year<-as.factor(landings_BoB$year)
total_landings_unit<-dcast(landings_BoB,unit+year ~area, value.var= "landings",
                           fun.aggregate=sum, na.rm=TRUE)


landings_BoB$unit<-as.character(landings_BoB$unit)
landings_BoB[landings_BoB=="Bakas"]<-"OTB"
landings_BoB[landings_BoB=="Parejas GAV"]<-"PTB"
landings_BoB[landings_BoB=="Tr\xedo GAV"]<-"PTB" #Trio: ya no los utilizan, 2 barcos pescan y 
                                                # el otro esta en ruta llevando las capturas
total_landings_BoB<- landings_BoB


# We are just going to focus on the trawlers
landings_wt<-subset(landings_BoB, (unit %in% c("OTB","PTB")))
landings_wt$unit<-as.factor(landings_wt$unit)
levels(landings_wt$unit)
# "OTB" "PTB"

# Which are the species that they are landing?
unique(landings_wt$Officialspp)
# [1] Chicharro Blanco   Chicharro Negro    Gallo whiffiagonis Gallo boscii      
# [5] Gallos             Merluza europea    Verdel, Caballa    Rape blanco       
# [9] Rape negro         Rapes              Lirio, Bacaladilla Estornino 

# Renaming the areas...we aggregate them by areas VI, VII and VIIIabd (not subareas)
levels(landings_wt$area)
landings_wt$area<-as.factor(landings_wt$area)
# [1] "VII"     "VIIIa"   "VIIIb"   "VIIIc"   "VIIj"    "VIa"     "VIb"     "VIIb"    "VIIh"   
# [10] "VIIIabd" "ICES"    "VIIc"    "VIIe"    "VIIg"    "VIIId"   "VIIk"    "VI"  

# I want to check the area, try to see if the sum subareas=sum areas
# Areas<-subset(landings_wt, (area %in% c("VIa","VIb")))
# sum(Areas$landings)
# [1] 1934366
# VI<-subset(landings_wt, (area %in% c("VI")))
# sum(VI$landings)
# [1] 503597.1
######It is different data

# I agregate the subareas in areas
#data= landings
area_VI<- subset(landings_wt, (area %in% c("VIa","VIb","VI")))
area_VI$area<- "VI"
a<- dcast(area_VI,unit+season+Officialspp+year ~area, value.var= "landings",
          fun.aggregate=sum, na.rm=TRUE)
a$area<- "VI"
names(a)<-c("unit", 'season', 'Officialspp', 'year',"data","area")

area_VII<- subset(landings_wt, (area %in% c("VII","VIIb","VIIc","VIIg","VIIh","VIIk","VIIj")))
area_VII$area<- "VII"
b<- dcast(area_VII,unit+season+Officialspp+year ~area, value.var= "landings",
          fun.aggregate=sum, na.rm=TRUE)
b$area<- "VII"
names(b)<-c("unit", 'season', 'Officialspp', 'year',"data","area")

area_VIIIabd<- subset(landings_wt, (area %in% c("VIIIa","VIIIabd","VIIIb","VIIIc","VIIId")))
area_VIIIabd$area<- "VIIIabd"
c<- dcast(area_VIIIabd,unit+season+Officialspp+year ~area, value.var= "landings",
          fun.aggregate=sum, na.rm=TRUE)
c$area<- "VIIIabd"
names(c)<-c("unit", 'season', 'Officialspp', 'year',"data","area")

landings_wt <- rbind(a,b,c)

landings_wt$area <- as.factor(landings_wt$area)
levels(landings_wt$area)
# "VI"      "VII"     "VIIIabd"

landings_wt_tot<-landings_wt

rm(a,area_VI,area_VII,area_VIIIabd,b,c,landings_BoB,landings_wt)


#-----------------------------------------------------------------------------------
# LANDINGS length files: 
#-----------------------------------------------------------------------------------

# Because to join all files whith a script was more time consuming than doing by hand, I have put all the landings length
# measures in Tallas.csv

landings_lenCls<- read.table ("~/Documents/BoB/BoB_data/data/LANDINGS_NA/Tallas.csv",
                              header=T, dec=".", sep=";")
# I omit those rows without landings numbers
#landings_lenCls<-landings_lenCls[!is.na(landings_lenCls$Total.Anual),]

landings_lenCls<-as.data.frame(landings_lenCls)
landings_lenCls$weigth<-landings_lenCls$Peso.Total/landings_lenCls$Total.Anual
landings_lenCls<-replace(landings_lenCls,is.na(landings_lenCls),NA)
landings_lenCls_n_wt<-landings_lenCls[,-c(6:7)]
landings_lenCls_n_wt<-melt(landings_lenCls_n_wt,id.vars=-(2:5))
landings_lenCls_n_wt[,7] <- as.numeric(sub("Trimestre.", "", landings_lenCls_n_wt[,7]))

# unit= gear
names(landings_lenCls_n_wt)<-c("length","Officialspp","area","year","unit","weight",
                               "season","landnumber")

rm(landings_lenCls)

#-----------------------------------------------------------------------------------
# PRICES
#-----------------------------------------------------------------------------------
# Setting my path!
path <-("~/Documents/BoB/BoB_data/data/Prices/")
filenames <- paste0(path, list.files(path, pattern = ".csv"))
prices = ldply(filenames, function(filename) {
  dum = read.table(filename,header=T, dec=",", sep=";")
  
  #If you want to add the filename as well on the column
  
  dum$filename = filename
  return(dum)
})


#-----------------------------------------------------------------------------------
# PRICES files: need to change dec.
#-----------------------------------------------------------------------------------

prices[,15] <- sub("~/Documents/BoB/BoB_data/data/Prices/precio trimestral", "", prices[,15])
prices[,15] <- as.character(sub(".csv", "", prices[,15]))

names(prices)<-c("season","2013","2012","2011","2010","2009","2008","2007",
                 "2006","2005","2004","2003","2002","2001","Officialspp")

prices[prices==" Chicharro blanco (HMM)"]<-"Chicharro Blanco"
prices[prices==" Chicharro negro (HOM)"]<-"Chicharro Negro"
prices[prices==" merluza"]<-"Merluza europea"
prices[prices==" rape negro (ANK)"]<-"Rape negro"
prices[prices==" rape blanco (MON)"]<-"Rape blanco"

prices[,2:14]<-apply(apply(prices[,2:14], 2, gsub, patt=",", replace="."), 2, as.numeric)

prices<-melt(prices, id.vars=c("season","Officialspp"))
names(prices)<-c("season","Officialspp","year","price")

prices$date<-paste(prices$year,prices$season,sep="-")

ggplot(prices,aes(date, price, group=Officialspp,colour=Officialspp)) +geom_line()+
  xlab("")+ ylab("Euro")

ggplot(prices,aes(year, price, group=Officialspp,colour=Officialspp)) +geom_line()+
  facet_grid(~season) +xlab("")+ ylab("Euro")
#scale_x_continuous("x axis",breaks=c(1,5,10))

#-----------------------------------------------------------------------------------
# SELECT THE SPECIES WE WANT
#-----------------------------------------------------------------------------------

levels(landings_wt_tot$Officialspp)
# > levels(landings_BoB$Officialspp)
# [1] "Chicharro Blanco"   "Chicharro Negro"    "Gallo boscii"       "Gallos"            
# [5] "Gallo whiffiagonis" "Merluza europea"    "Verdel, Caballa"    "Rape blanco"       
# [9] "Rape negro"         "Rapes"              "Lirio, Bacaladilla" "Estornino"  

# There are Rapes ang Gallos which are not determined from which specie they came from
# but we can avoid it because are just 40 and 10 kg respectively.
# > landings_BoB[landings_BoB$Officialspp=="Rapes",]
# unit season Officialspp year data area
# 180  OTB      4       Rapes 2003 40.8  VII
# > landings_BoB[landings_BoB$Officialspp=="Gallos",]
# unit season Officialspp year   data    area
# 344  OTB      4      Gallos 2003 10.388 VIIIabd

# levels(discards_wt$Officialspp)
# 234 species
# discards_wt[discards_wt$Officialspp=="Rapes",]
# <0 rows> (or 0-length row.names)
#  discards_wt[discards_wt$Officialspp=="Gallos",]
# [1] unit                 area                 season               Officialspp          Totalweightotaltrips
# [6] Sampletrips          Totaltrips           year                
# <0 rows> (or 0-length row.names)


species<- c('HKE', 'MEG','MAC','HOM','WHB','MON','ANK')
name<- c('Merluza europea','Gallo whiffiagonis','Verdel, Caballa','Chicharro Negro','Lirio, Bacaladilla',
         'Rape blanco','Rape negro')


#-----------------------------------------------------------------------------------
# Create a LIST for each specie with the discards and landings data
#-----------------------------------------------------------------------------------
#unit=gear

for (i in 1:length(species)){
  
  wt.discards<-discards_wt_tot[discards_wt_tot$Officialspp==name[i],]
  lenCls.discards<-discards_lenCls[discards_lenCls$Officialspp==name[i],]
  wt.landings<-landings_wt_tot[landings_wt_tot$Officialspp==name[i],]
  lenCls.landings<-landings_lenCls_n_wt[landings_lenCls_n_wt$Officialspp==name[i],]
  
  #DISCARDS
  sample<- subset(wt.discards, select=c(year,unit,season,area,Sampletrips))
  names(sample)<-c('year', 'unit', 'season', 'area', 'data')
  disc_sampletrips<- as.FLQuant(sample)
  units(disc_sampletrips)<-"1"
  
  sample<- subset(wt.discards, select=c(year,unit,season,area,Totaltrips))
  names(sample)<-c('year', 'unit', 'season', 'area', 'data')
  totaltrips<- as.FLQuant(sample)
  units(totaltrips)<-"1"
  
  sample<- subset(wt.discards, select=c(year,unit,season,area,Totalweightotaltrips))
  names(sample)<-c('year', 'unit', 'season', 'area', 'data')
  disc_wt<- as.FLQuant(sample)
  units(disc_wt)<-"kg"
  

  lenCls<- sort(unique(lenCls.discards$Lengt))
  
  sample<- subset(lenCls.discards, select=c(Lengt,year,unit,season,area,Disc_n))
  names(sample)<-c('length','year', 'unit', 'season', 'area', 'data')
  sample$length<-as.numeric(sample$length)
  disc_lenCls_n<- as.FLQuant(sample)
  units(disc_lenCls_n)<-"1"
  
  idx<-dimnames(disc_lenCls_n)
  idx[[1]]<-as.numeric(idx[[1]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  disc_lenCls_n<-do.call('[',c(list(x=disc_lenCls_n),idx))
  
  # nt<-apply(Disc_n, 1, function(x) sum(is.na(x)))
  # prod(dim(Disc_n)[-1]) #240
  # nt>239
  
  sample<- subset(lenCls.discards, select=c(Lengt,year,unit,season,area,Disc_SOP))
  names(sample)<-c('length','year', 'unit', 'season', 'area', 'data')
  sample$length<-as.numeric(sample$length)
  disc_lenCls_wt<- as.FLQuant(sample)
  units(disc_lenCls_wt)<-"kg"
  
  idx<-dimnames(disc_wt)
  idx[[1]]<-as.numeric(idx[[1]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  disc_wt<-do.call('[',c(list(x=disc_wt),idx))
  
  #LANDINGS
  sample<- subset(wt.landings, select=c(year,unit,season,area,data))
  landings_wt<- as.FLQuant(sample)
  units(landings_wt)<-"kg"
  
  
  lenCls.landings<- lenCls.landings[!is.na( lenCls.landings$landnumber),]
  lenCls.landings$data<-lenCls.landings$weight * lenCls.landings$landnumber
  
  lenCls<- sort(unique(lenCls.landings$length))
  
  sample<- subset(lenCls.landings, select=c(length,year,unit,season,area,data))
  sample$length<-as.numeric(sample$length)
  landings_lenCls_wt<- as.FLQuant(sample)
  units(landings_lenCls_wt)<-"kg"
  
  idx<-dimnames(landings_lenCls_wt)
  idx[[1]]<-as.numeric(idx[[1]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  landings_lenCls_wt<-do.call('[',c(list(x=landings_lenCls_wt),idx))
  
  
  sample<- subset(lenCls.landings, select=c(length,year,unit,season,area,landnumber))
  names(sample)<-c('length','year', 'unit', 'season', 'area', 'data')
  sample$length<-as.numeric(sample$length)
  landings_lenCls_n<- as.FLQuant(sample)
  units(landings_lenCls_n)<-"1"
  
  idx<-dimnames(landings_lenCls_n)
  idx[[1]]<-as.numeric(idx[[1]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  landings_lenCls_n<-do.call('[',c(list(x=landings_lenCls_n),idx))
  
  
  
  catch<-FLQuants(disc_sampletrips=disc_sampletrips, totaltrips=totaltrips, 
                  disc_wt=disc_wt,disc_lenCls_n=disc_lenCls_n, disc_lenCls_wt=disc_lenCls_wt, 
                  landings_wt=landings_wt,landings_lenCls_wt=landings_lenCls_wt,
                  landings_lenCls_n=landings_lenCls_n)
  
  assign(paste("catch",species[i],sep="_"),catch)
  
}

save(catch_ANK,catch_HKE,catch_HOM,catch_MAC,catch_MEG,
     catch_MON,catch_WHB,discards_wt_tot,discards_lenCls, landings_wt_tot,landings_lenCls_n_wt,
     total_landings_BoB,total_landings_unit,prices, file="~/Documents/BoB/BoB_data/data/Catch_data_BoB.RData")