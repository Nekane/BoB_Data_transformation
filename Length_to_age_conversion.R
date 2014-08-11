#-----------------------------------------------------------------------------------
#Converting from length to age based data
# Nekane Alzorriz
# Discards PhD, JRC- Ispra
# June 2014
#-----------------------------------------------------------------------------------

#The conversion of length data to age is performed through the use of a growth model. 
#The implementation is done through the \class{a4aGr} class.

library(reshape2)
library(plyr)
library(triangle)
library(FLa4a)
library(FLCore)
library(ggplotFL)
library(XML)

load("/home/alzorne/Documents/BoB/BoB_data/data/Catch_data_BoB.RData")
#load("/home/alzorne/Documents/BoB/BoB_data/data/Landings_data_BoB.RData")
#load("/home/alzorne/Documents/BoB/BoB_data/data/Discards_data_BoB.RData")

#-----------------------------------------------------------------------------------
species<- c('HKE', 'MEG','MAC','HOM','WHB','MON','ANK')
name<- c('Merluza europea','Gallo whiffiagonis','Verdel, Caballa','Chicharro Negro',
            'Lirio, Bacaladilla','Rape blanco','Rape negro') 
#-----------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------
# CONVERTING LENGTH TO AGE: a4aGr- l2a
#-----------------------------------------------------------------------------------

for (i in 1:length(species)){
 
  #Von Bertalanffy
  vb_growth <- ~linf*(1-exp(-k*(t-t0)))
  # t = f(linf, l, t0, len)
  vb_inv_growth <- ~t0-1/k*log(1-len/linf)
  # where t is the age
  
 
  if (i==1){
    
    #Data
    landings<-catch_HKE[[1]]
    landings.n<-catch_HKE[[4]] # This is the length reconstructed FLQuant
    landings.wt<-catch_HKE[[3]]
    discards<-catch_HKE[[5]]
    discards.n<-catch_HKE[[6]]
    discards.wt<-catch_HKE[[7]]
    
    # Address of the growth table for hake in FishBase - check this
    addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=30" 
    # Get the table
    tab <- try(readHTMLTable(addr))
    dat <- tab$dataTable
    
    # Remove questionable and select the ones from the BoB area
    dat <- dat[dat$Questionable=="No",]
    dat <- dat[dat$Locality %in% c("Northwest Iberian Peninsula","North and Northwest Iberian waters","Bay of Biscay (north stock)",
    "Bay of Biscay","Gulf of Biscay", "Golfe de Gascogne Bay of Biscay","Southwest coast"),]
    
    # Pull out data - annoying because columns are factors and we want numeric 
    Linf <- as.numeric(as.character(dat[,"Loo(cm)"]))
    k <-    as.numeric(as.character(dat[,"K(1/y)"]))
    t0 <-   as.numeric(as.character(dat[,"to(years)"]))
    
    # Get the variance covariance matrix
    vb_dat <- cbind(linf = Linf, k = k, t0 = t0)
    #vb_dat[is.na(vb_dat)]<-0
    #vb_dat <- na.omit(vb_dat)
    mm <- var(vb_dat, na.rm=T)
    
    # Set the von Bertalanffy parameters WG 2013
    L1<-14.75
    Linf<-130
    k<-0.147
    t0<- 1-(log (1-L1/Linf))/-k
    vb_params <- FLPar(linf=Linf, k=k, t0=t0)  
    
    # Create an 'a4aGr' object
    # This contains the growth model, the inverse growth model
    # and the parameters
    vbObj <- a4aGr(grMod = vb_growth,
                   grInvMod = vb_inv_growth,
                   params = vb_params,
                   vcov=mm)
    
    # We can use this calculate t (or age) by giving it a length  #27 cm MLS --------------> t= 1.67
    #t <- predict(vbObj, len=27)
    # We can also use it to calculate length, given t (age)
    #len <- predict(vbObj, t=1) #len=14.75
    
    
    
  } else if (i==2){

    #Data
    landings<-catch_MEG[[1]]
    landings.n<-catch_MEG[[4]] # This is the length reconstructed FLQuant
    landings.wt<-catch_MEG[[3]]
    discards<-catch_MEG[[5]]
    discards.n<-catch_MEG[[6]]
    discards.wt<-catch_MEG[[7]]
    
    # Address of the growth table for megrim in FishBase - check this
    addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=28"
    # Get the table
    tab <- try(readHTMLTable(addr))
    dat <- tab$dataTable
    head(dat)
    # Remove questionable
    dat <- dat[dat$Questionable=="No",]
    dat <- dat[dat$Locality %in% c("Northeast Atlantic (ICES Div. VII)","ay of Biscay (ICES VIIIab)"),]
    # Pull out data - annoying because columns are factors and we want numeric 
    Linf <- as.numeric(as.character(dat[,"Loo(cm)"]))
    k <-    as.numeric(as.character(dat[,"K(1/y)"]))
    t0 <-   as.numeric(as.character(dat[,"to(years)"]))
    
    # Get the variance covariance matrix
    vb_dat <- cbind(linf = Linf, k = k, t0 = t0)
    mm <- var(vb_dat, na.rm=T)
    
    vbObj <- a4aGr(grMod = vb_growth,
                   grInvMod = vb_inv_growth,
                   params=FLPar(linf=median(Linf,na.rm=T), k=median(k,na.rm=T), t0=median(t0,na.rm=T), units=c("cm","ano-1","ano")), 
                   vcov=mm)
    
    #For the Megrim I have the ALK for one year, the next script shows the length to age conversion with
    # the ALK
    # ("~/Documents/BoB/BoB_data/data/ALK_Megrim_l2a.R")
    # We can use this calculate t (or age) by giving it a length  #20 cm MLS --------------> t= 2.65
    #t <- predict(vbObj, len=27)
    # We can also use it to calculate length, given t (age)
    #len <- predict(vbObj, t=3) #len=21.54928
    
  } else if (i==3){
    
    #Data
    landings<-catch_MAC[[1]]
    landings.n<-catch_MAC[[4]] # This is the length reconstructed FLQuant
    landings.wt<-catch_MAC[[3]]
    discards<-catch_MAC[[5]]
    discards.n<-catch_MAC[[6]]
    discards.wt<-catch_MAC[[7]]
    
    # Address of the growth table for mackerel in FishBase - check this
    addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=118" 
    # Get the table
    tab <- try(readHTMLTable(addr))
    dat <- tab$dataTable
    head(dat)
    
    # Remove questionable
    dat <- dat[dat$Questionable=="No",]
    dat <- dat[dat$Locality %in% c("Cantabrian Sea","Southern North Sea", "North Sea","English Channel", 
                                   "Celtic Sea", "English Channel & Irish Sea","English Channel","Western stock"),]
    # Pull out data - annoying because columns are factors and we want numeric 
    Linf <- as.numeric(as.character(dat[,"Loo(cm)"]))
    k <-    as.numeric(as.character(dat[,"K(1/y)"]))
    t0 <-   as.numeric(as.character(dat[,"to(years)"]))
    
    # Get the variance covariance matrix
    vb_dat <- cbind(linf = Linf, k = k, t0 = 0)
    mm <- var(vb_dat[,-3], na.rm=T)
    
    vbObj <- a4aGr(grMod = vb_growth,
                   grInvMod = vb_inv_growth,
                   params=FLPar(linf=median(Linf,na.rm=T), k=median(k,na.rm=T), t0=0, units=c("cm","ano-1","ano")), 
                   vcov=mm)
    
    
  
  } else if (i==4){
    #Data
    landings<-catch_HOM[[1]]
    landings.n<-catch_HOM[[4]] # This is the length reconstructed FLQuant
    landings.wt<-catch_HOM[[3]]
    discards<-catch_HOM[[5]]
    discards.n<-catch_HOM[[6]]
    discards.wt<-catch_HOM[[7]]
    
    # Address of the growth table for hake in FishBase - check this
    addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=1365"
    # Get the table
    tab <- try(readHTMLTable(addr))
    dat <- tab$dataTable
    head(dat)
    # Remove questionable
    dat <- dat[dat$Questionable=="No",]
    dat <- dat[dat$Locality %in% c("ICES Subarea VIIf-j","ICES Subarea VIIIb","ICES Subarea VIIIa"),]
    
    # Pull out data - annoying because columns are factors and we want numeric 
    Linf <- as.numeric(as.character(dat[,"Loo(cm)"]))
    k <-    as.numeric(as.character(dat[,"K(1/y)"]))
    t0 <-   as.numeric(as.character(dat[,"to(years)"])) # no t0
    
    vb_dat <- cbind(linf = Linf, k = k, t0 = 0)
    mm <- var(vb_dat[,-3], na.rm=T)
    
    vbObj <- a4aGr(grMod = vb_growth,
                   grInvMod = vb_inv_growth,
                   params=FLPar(linf=median(Linf,na.rm=T), k=median(k,na.rm=T), t0=0, units=c("cm","ano-1","ano")), 
                   vcov=mm) 
    # We can use this calculate t (or age) by giving it a length  #15 cm MLS --------------> t= 2.34
    #t <- predict(vbObj, len=27)
    # We can also use it to calculate length, given t (age)
    #len <- predict(vbObj, t=3) #len=18.26028
    
  } else if (i==5){ 
    #Data
    landings<-catch_WHB[[1]]
    landings.n<-catch_WHB[[4]] # This is the length reconstructed FLQuant
    landings.wt<-catch_WHB[[3]]
    discards<-catch_WHB[[5]]
    discards.n<-catch_WHB[[6]]
    discards.wt<-catch_WHB[[7]]
    
    # Address of the growth table for blue whiting in FishBase - check this
    addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=31"
    # Get the table
    tab <- try(readHTMLTable(addr))
    dat <- tab$dataTable
    
    # Remove questionable
    dat <- dat[dat$Questionable=="No",]
    dat <- dat[dat$Locality %in% c("N. North Sea","Faroe Bank, 60°55´N; 8°40´W",
                                   "North Atlantic","57°N 11°W"," "),]
    
    # Pull out data - annoying because columns are factors and we want numeric 
    Linf <- as.numeric(as.character(dat[,"Loo(cm)"]))
    k <-    as.numeric(as.character(dat[,"K(1/y)"]))
    t0 <-   as.numeric(as.character(dat[,"to(years)"]))
    vb_dat <- cbind(linf = Linf, k = k, t0 = 0)
    mm <- var(vb_dat[,-3], na.rm=T)
    
    vbObj <- a4aGr(grMod = vb_growth,
                   grInvMod = vb_inv_growth,
                   params=FLPar(linf=median(Linf,na.rm=T), k=median(k,na.rm=T), t0=0, units=c("cm","ano-1","ano")), 
                   vcov=mm)
  
  } else if (i==6){ 
    #Data
    landings<-catch_MON[[1]]
    landings.n<-catch_MON[[4]] # This is the length reconstructed FLQuant
    landings.wt<-catch_MON[[3]]
    discards<-catch_MON[[5]]
    discards.n<-catch_MON[[6]]
    discards.wt<-catch_MON[[7]]
    
    # Address of the growth table for hake in FishBase - check this
    addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=716"
    # Get the table
    tab <- try(readHTMLTable(addr))
    dat <- tab$dataTable
    head(dat)
    # Remove questionable
    dat <- dat[dat$Questionable=="No",]
    dat <- dat[dat$Locality %in% c("ICES Div. VIII-IXa","ICES Areas VII and VIII",
                                   "Bay of Biscay (ICES Div. VIIIabd)","Northern stock"),]
    
    # Pull out data - annoying because columns are factors and we want numeric 
    Linf <- as.numeric(as.character(dat[,"Loo(cm)"]))
    k <-    as.numeric(as.character(dat[,"K(1/y)"]))
    t0 <-   as.numeric(as.character(dat[,"to(years)"]))
    vb_dat <- cbind(linf = Linf, k = k, t0 = t0)
    mm <- var(vb_dat, na.rm=T)
    
    # Set the von Bertalanffy parameters [J. Landa et al., 2008]
    
    Linf<-140.00
    k<-0.11
    t0<- 0
    
    # Create an 'a4aGr' object
    
    vbObj <- a4aGr(grMod = vb_growth,
                   grInvMod = vb_inv_growth,
                   params=FLPar(linf=Linf, k=k, t0=t0, units=c("cm","ano-1","ano")), 
                   vcov=mm)
    
    
  } else if (i==7 ){ 
    # no data in fishbase.... Quincoces has told me that there are not reliable studies for the length to age conversion
    #I take the same values as for the MON
    #Data
    landings<-catch_ANK[[1]]
    landings.n<-catch_ANK[[4]] # This is the length reconstructed FLQuant
    landings.wt<-catch_ANK[[3]]
    discards<-catch_ANK[[5]]
    discards.n<-catch_ANK[[6]]
    discards.wt<-catch_ANK[[7]]
    
    # Address of the growth table for hake in FishBase - check this
    addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=716"
    # Get the table
    tab <- try(readHTMLTable(addr))
    dat <- tab$dataTable
    head(dat)
    # Remove questionable
    dat <- dat[dat$Questionable=="No",]
    dat <- dat[dat$Locality %in% c("ICES Div. VIII-IXa","ICES Areas VII and VIII",
                                   "Bay of Biscay (ICES Div. VIIIabd)","Northern stock"),]
    
    # Pull out data - annoying because columns are factors and we want numeric 
    Linf <- as.numeric(as.character(dat[,"Loo(cm)"]))
    k <-    as.numeric(as.character(dat[,"K(1/y)"]))
    t0 <-   as.numeric(as.character(dat[,"to(years)"]))
    vb_dat <- cbind(linf = Linf, k = k, t0 = t0)
    mm <- var(vb_dat, na.rm=T)
    
    # Set the von Bertalanffy parameters [J. Landa et al., 2008]
    
    Linf<-140.00
    k<-0.11
    t0<- 0
    
    # Create an 'a4aGr' object
    
    vbObj <- a4aGr(grMod = vb_growth,
                   grInvMod = vb_inv_growth,
                   params=FLPar(linf=Linf, k=k, t0=t0, units=c("cm","ano-1","ano")), 
                   vcov=mm)
    
    

     
  }
 
  
  
landings.wt <- landings.wt*landings.n 
units(landings.wt)<-"kg"
discards.wt<- discards.wt*discards.n
units(discards.wt)<-"kg"
  
landings.n     <- l2a(landings.n , vbObj, stat="sum")     
landings.wt    <- l2a(landings.wt, vbObj, stat="sum")
landings.wt    <- landings.wt %/% landings.n 
      
discards.n     <- l2a(discards.n , vbObj, stat="sum")     
discards.wt    <- l2a(discards.wt, vbObj, stat="sum")
discards.wt    <- discards.wt %/% discards.n  
  
  

  # CHeck------------------------
sum(landings.n*landings.wt,na.rm=TRUE)
sum(landings,na.rm=TRUE)
sum(discards.n*discards.wt,na.rm=TRUE)
sum(discards,na.rm=TRUE)

  
  
quant(landings)<-"age"
quant(discards)<-"age"
  
landings<-window(landings, start=2003, end=2012)
landings.n<-window(landings.n, start=2003, end=2012)
landings.wt<-window(landings.wt, start=2003, end=2012)

age.stk<-FLStock(FLQuant(NA, dimnames = list(age = 0:31, year = 2003:2012, 
                                          unit = c('OTB',"PTB"), season = 1:4,
                                          area = c("VI","VII","VIIIabd"))))
landings(age.stk)[dimnames(landings)$age]<- landings
landings.n(age.stk)[dimnames(landings.n)$age] <- landings.n
landings.wt(age.stk)[dimnames(landings.wt)$age] <- landings.wt
discards(age.stk)[dimnames(discards)$age]<- discards
discards.n(age.stk)[dimnames(discards.n)$age] <- discards.n
discards.wt(age.stk)[dimnames(discards.wt)$age] <- discards.wt  
    
  assign(paste(species[i],"age.stk",sep="_"),age.stk) 
  
}
hke.stk<- setPlusGroup(HKE_age.stk,15)
meg.stk<- setPlusGroup(MEG_age.stk,10)
mac.stk<- setPlusGroup(MAC_age.stk,12)
hom.stk<- setPlusGroup(HOM_age.stk,11)
whb.stk<- setPlusGroup(WHB_age.stk,10)
mon.stk<- setPlusGroup(MON_age.stk,13)
ank.stk<- setPlusGroup(ANK_age.stk,14)

save(hke.stk,meg.stk,mac.stk,hom.stk,whb.stk,
     mon.stk,ank.stk, file="~/Documents/BoB/BoB_data/data/Catch_age_BoB.RData")

ggplot(data=(landings.n[,ac(c(2003:2012)),"PTB"]%/%quantSums(landings.n[,ac(c(2003:2012)),"PTB"])),aes(factor(season),factor(age),size=data)) +
  geom_point(shape=21) +scale_size(range=c(1,7)) + facet_grid(area~unit+year)+
  ylab("age") +xlab("")+ theme(legend.position="none")  


#------------------------------------------------------------------
# Experiment
# Set the von Bertalanffy parameters
n <- 2
L1<-14.67 * rnorm(n,1,sd=0.1)
Linf<-130 * rnorm(n,1,sd=0.1)
k<-0.159 * rnorm(n,1,sd=0.1)
t0<- 1-(log (1-L1/Linf))/-k
#vb_params <- FLPar(dim = c(3,n))
vb_params <- FLPar(linf=Linf, k=k, t0=t0, iter=n)

#vb_params <- FLPar(NA, dimnames=list(params = c("linf","k","t0"),iter = 1:n))
#vb_params[] <- rbind(Linf,k,t0)

# Create an 'a4aGr' object
# This contains the growth model, the inverse growth model
# and the parameters
vbObj <- a4aGr(grMod = vb_growth,
               grInvMod = vb_inv_growth,
               params = vb_params)

# How does an a4aGr object work?
# We can use this calculate t (or age) by giving it a length
t <- predict(vbObj, len=20)
# We can also use it to calculate length, given t (age)
len <- predict(vbObj, t=t)

age_n     <- l2a(flq, vbObj, stat="sum")

