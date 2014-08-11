library(reshape2)
library(plyr)
library(FLCore)
library(ggplotFL)

load("/home/alzorne/Documents/BoB/BoB_data/data/Landings_data_BoB.RData")
load("/home/alzorne/Documents/BoB/BoB_data/data/Discards_data_BoB.RData")

#-----------------------------------------------------------------------------------
# Check the length Class data gaps for each year/ gear/ season/ specie
#-----------------------------------------------------------------------------------

species<- c('HKE', 'MEG','MAC','HOM','WHB','MON','ANK')
name<- c('Merluza europea','Gallo whiffiagonis','Verdel, Caballa','Chicharro Negro','Lirio, Bacaladilla',
         'Rape blanco','Rape negro')

for (i in 1:length(species)){
  landings<-df_landings[df_landings$spp==species[i],]
  #landings<-df_landings[df_landings$spp==name[i],]
  landings.n<-df_landings.n[df_landings.n$spp==name[i],]
  landings.wt<-df_landings.wt[df_landings.wt$spp==name[i],]
  landings.wt$data<-format(landings.wt$data, digits = 3)
  landings.wt$data<-as.numeric(landings.wt$data)
 
  discards<-df_discards[df_discards$spp==name[i],]
  discards.n<-df_discards.n[df_discards.n$spp==name[i],]
  discards.wt<-df_discards.wt[df_discards.wt$spp==name[i],]
  discards.wt$data<-format(discards.wt$data, digits = 3)
  discards.wt$data<-as.numeric(discards.wt$data)
  
  #LANDINGS
  names(landings)[6]<-"data"
  sample<- subset(landings, select=c(year,unit,season,area,data))
  landings<- as.FLQuant(sample)
  units(landings)<-"kg"
  
  idx<-dimnames(landings)
  idx[[4]]<-as.numeric(idx[[4]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  landings<-do.call('[',c(list(x=landings),idx))
  
  #LANDINGS.wt
  sample<- subset(landings.wt, select=c(len,year,unit,season,area,data))
  sample$len<-as.numeric(sample$len)
  landings.wt<- as.FLQuant(sample)
  units(landings.wt)<-"kg"
  
  idx<-dimnames(landings.wt)
  idx[[1]]<-as.numeric(idx[[1]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  landings.wt<-do.call('[',c(list(x=landings.wt),idx))
  
  #LANDINGS.n
  sample<- subset(landings.n, select=c(len,year,unit,season,area,data))
  sample$len<-as.numeric(sample$len)
  landings.n<- as.FLQuant(sample)
  units(landings.n)<-"1"
  
  idx<-dimnames(landings.n)
  idx[[1]]<-as.numeric(idx[[1]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  landings.n<-do.call('[',c(list(x=landings.n),idx))  

  #DISCARDS
  sample<- subset(discards, select=c(year,unit,season,area,data))
  discards<- as.FLQuant(sample)
  units(discards)<-"kg"
  
  idx<-dimnames(discards)
  idx[[4]]<-as.numeric(idx[[4]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  discards<-do.call('[',c(list(x=discards),idx))
  
  #DISCARDS.wt
  sample<- subset(discards.wt, select=c(len,year,unit,season,area,data))
  sample$len<-as.numeric(sample$len)
  discards.wt<- as.FLQuant(sample)
  units(discards.wt)<-"kg"
  
  idx<-dimnames(discards.wt)
  idx[[1]]<-as.numeric(idx[[1]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  discards.wt<-do.call('[',c(list(x=discards.wt),idx))
  
  #DISCARDS.n
  sample<- subset(discards.n, select=c(len,year,unit,season,area,data))
  sample$len<-as.numeric(sample$len)
  discards.n<- as.FLQuant(sample)
  units(discards.n)<-"1"
  
  idx<-dimnames(discards.n)
  idx[[1]]<-as.numeric(idx[[1]])
  idx<-lapply(idx,order)
  names(idx)<-letters[9:14]
  discards.n<-do.call('[',c(list(x=discards.n),idx)) 
  
  rm(idx,sample)
  

#--------------------------------------------------------------------------------------
# RECONSTRUCTING DATA GAPS
#--------------------------------------------------------------------------------------

  reconst.n<-landings.n
  

if (i==1){
  # Missing data: OTB- area VI and VII there is no data for the year 2006
               #  PTB- area VII 2006
  # Assumptions: I calculate the mean for the missing year when there is data for the previous and the
               #  following year. If not, I take the data from the previous year
               #  Because there were not landings sampled, there is not weight associated, I proceed to calculate it.   
    
    #N mean from the previous and the following year
    reconst.n[,"2006","OTB",,"VI"]<-(landings.n[,"2005","OTB",,"VI"]+landings.n[,"2007","OTB",,"VI"])/2
    landings.wt[,"2006","OTB",,"VI"]<-(landings.wt[,"2005","OTB",,"VI"]+landings.wt[,"2007","OTB",,"VI"])/2
    #N proportion
    reconst.n[,"2006","OTB",,"VI"]<-reconst.n[,"2006","OTB",,"VI"]%/%quantSums(reconst.n[,"2006","OTB",,"VI"])
    # N final
    reconst.n[,"2006","OTB",,"VI"]<-(landings[,"2006","OTB",,"VI"]%/%landings.wt[,"2006","OTB",,"VI"])*reconst.n[,"2006","OTB",,"VI"]
  
    reconst.n[,"2006","OTB",as.character(c(2,3)),"VII"]<-(landings.n[,"2005","OTB",as.character(c(2,3)),"VII"]+landings.n[,"2007","OTB",as.character(c(2,3)),"VII"])/2 
    landings.wt[,"2006","OTB",as.character(c(2,3)),"VII"]<-(landings.wt[,"2005","OTB",as.character(c(2,3)),"VII"]+landings.wt[,"2007","OTB",as.character(c(2,3)),"VII"])/2 
    reconst.n[,"2006","OTB",as.character(c(2,3)),"VII"]<-reconst.n[,"2006","OTB",as.character(c(2,3)),"VII"]%/%quantSums(reconst.n[,"2006","OTB",as.character(c(2,3)),"VII"]) 
    reconst.n[,"2006","OTB",as.character(c(2,3)),"VII"]<-(landings[,"2006","OTB",as.character(c(2,3)),"VII"]%/%landings.wt[,"2006","OTB",as.character(c(2,3)),"VII"])*reconst.n[,"2006","OTB",as.character(c(2,3)),"VII"] 
  
    reconst.n[,"2006","OTB",as.character(c(1,4)),"VII"]<-landings.n[,"2005","OTB",as.character(c(1,4)),"VII"]
    landings.wt[,"2006","OTB",as.character(c(1,4)),"VII"]<-landings.wt[,"2005","OTB",as.character(c(1,4)),"VII"]
    reconst.n[,"2006","OTB",as.character(c(1,4)),"VII"]<-reconst.n[,"2006","OTB",as.character(c(1,4)),"VII"]%/%quantSums(reconst.n[,"2006","OTB",as.character(c(1,4)),"VII"]) 
    reconst.n[,"2006","OTB",as.character(c(1,4)),"VII"]<-(landings[,"2006","OTB",as.character(c(1,4)),"VII"]%/%landings.wt[,"2006","OTB",as.character(c(1,4)),"VII"])*reconst.n[,"2006","OTB",as.character(c(1,4)),"VII"]
      
    reconst.n[,"2006","PTB","1","VII"]<-landings.n[,"2005","PTB","1","VII"]
    landings.wt[,"2006","PTB","1","VII"]<-landings.wt[,"2005","PTB","1","VII"]
    reconst.n[,"2006","PTB","1","VII"]<-reconst.n[,"2006","PTB","1","VII"]%/%quantSums(reconst.n[,"2006","PTB","1","VII"])
    reconst.n[,"2006","PTB","1","VII"]<-(landings[,"2006","PTB","1","VII"]%/%landings.wt[,"2006","PTB","1","VII"])*reconst.n[,"2006","PTB","1","VII"]

    reconst.n[,"2006","PTB","2","VII"]<-(landings.n[,"2005","PTB","2","VII"]+landings.n[,"2007","PTB","2","VII"])/2
    landings.wt[,"2006","PTB","2","VII"]<-(landings.wt[,"2005","PTB","2","VII"]+landings.wt[,"2007","PTB","2","VII"])/2
    reconst.n[,"2006","PTB","2","VII"]<-reconst.n[,"2006","PTB","2","VII"]%/%quantSums(reconst.n[,"2006","PTB","2","VII"]) 
    reconst.n[,"2006","PTB","2","VII"]<-(landings[,"2006","PTB","2","VII"]%/%landings.wt[,"2006","PTB","2","VII"])*
      reconst.n[,"2006","PTB","2","VII"]
    
    reconst.n[,"2009","OTB",,"VIIIabd"]<-landings.n[,"2009","OTB",,"VIIIabd"]
    landings.wt[,"2009","OTB",,"VIIIabd"]<-landings.wt[,"2009","OTB",,"VIIIabd"]
    reconst.n[,"2009","OTB",,"VIIIabd"]<-reconst.n[,"2009","OTB",,"VIIIabd"]%/%quantSums(reconst.n[,"2009","OTB",,"VIIIabd"]) 
    reconst.n[,"2009","OTB",,"VIIIabd"]<-(landings[,"2009","OTB",,"VIIIabd"]%/%landings.wt[,"2009","OTB",,"VIIIabd"])*reconst.n[,"2009","OTB",,"VIIIabd"]
    
    reconst.n[,"2011","PTB",,"VIIIabd"]<-(landings.n[,"2010","PTB",,"VIIIabd"]+landings.n[,"2012","PTB",,"VIIIabd"])/2
    landings.wt[,"2011","PTB",,"VIIIabd"]<-(landings.wt[,"2010","PTB",,"VIIIabd"]+landings.wt[,"2012","PTB",,"VIIIabd"])/2
    reconst.n[,"2011","PTB",,"VIIIabd"]<-reconst.n[,"2011","PTB",,"VIIIabd"]%/%quantSums(reconst.n[,"2011","PTB",,"VIIIabd"])
    reconst.n[,"2011","PTB",,"VIIIabd"]<-(landings[,"2011","PTB",,"VIIIabd"]%/%landings.wt[,"2011","PTB",,"VIIIabd"])*reconst.n[,"2011","PTB",,"VIIIabd"]
    
    reconst.n[]<-reconst.n[]%/%quantSums(reconst.n[]) 
    reconst.n[]<-(landings[]%/%landings.wt[])*reconst.n[]
    
    #     reconst.n[,"2010","OTB",,"VIIIabd"]<-landings.n[,"2010","OTB",,"VIIIabd"]
    #     landings.wt[,"2010","OTB",,"VIIIabd"]<-landings.wt[,"2010","OTB",,"VIIIabd"]
    #     reconst.n[,"2010","OTB",,"VIIIabd"]<-reconst.n[,"2010","OTB",,"VIIIabd"]%/%quantSums(reconst.n[,"2010","OTB",,"VIIIabd"]) 
    #     reconst.n[,"2010","OTB",,"VIIIabd"]<-(landings[,"2010","OTB",,"VIIIabd"]%/%landings.wt[,"2010","OTB",,"VIIIabd"])*reconst.n[,"2010","OTB",,"VIIIabd"]
    #     
    
} else if (i==2){
    # Missing data: OTB- area VIIIabd there are no samples for 2005 and 2006
    #  PTB- no data at all
    # Assumptions: OTB-, so I take 2004 and 2007 to make the mean
    #  PTB- I take the OTB frequency distribution and I apply it to the PTB
    #  Because there were not landings sampled, there is not weight associated, I proceed to calculate it.   
    
    #I create the second unit in the FLQuant, the unit PTB does not exist- expand(landings.n,unit=c("OTB","PTB"))
  discards.a <- dimnames(discards)
  discards.a$unit<-c("OTB","PTB")
  discards.a <-FLQuant(dimnames = discards.a,units="kg")
  discards.a[,,"OTB"]<-discards[,,"OTB"]
  discards<-discards.a
  
  discards.a <- dimnames(discards.n)
  discards.a$unit<-c("OTB","PTB")
  discards.a <-FLQuant(dimnames = discards.a,units="1")
  discards.a[,,"OTB"]<-discards.n[,,"OTB"]
  discards.n<-discards.a
  
  discards.a <- dimnames(discards.wt)
  discards.a$unit<-c("OTB","PTB")
  discards.a <-FLQuant(dimnames = discards.a,units="kg")
  discards.a[,,"OTB"]<-discards.wt[,,"OTB"]
  discards.wt<-discards.a
  
    reconst.n <- dimnames(landings.n)
    reconst.n$unit<-c("OTB","PTB")
    reconst.n <-FLQuant(dimnames = reconst.n,units="1")
    reconst.n[,,"OTB"]<-landings.n [,,"OTB"]
    
    landings.wtt <- dimnames(landings.wt)
    landings.wtt$unit<-c("OTB","PTB")
    landings.wtt <-FLQuant(dimnames = landings.wtt,units="kg")
    landings.wtt[,,"OTB"]<-landings.wt [,,"OTB"]
    landings.wtt[,,"PTB"]<-landings.wt [,,"OTB"]
    landings.wt<-landings.wtt
  
    reconst.n[,c("2005","2006"),"OTB",,"VI"]<-(landings.n[,c("2004","2004"),"OTB",,"VI"]+landings.n[,c("2007","2007"),"OTB",,"VI"])/2 
    landings.wt[,c("2005","2006"),"OTB",,"VI"]<-(landings.wt[,"2004","OTB",,"VI"]+landings.wt[,"2007","OTB",,"VI"])/2 
    reconst.n[,c("2005","2006"),"OTB",,"VI"]<-reconst.n[,c("2005","2006"),"OTB",,"VI"]%/%quantSums(reconst.n[,c("2005","2006"),"OTB",,"VI"])
    reconst.n[,c("2005","2006"),"OTB",,"VI"]<-(landings[,c("2005","2006"),"OTB",,"VI"]%/%landings.wt[,c("2005","2006"),"OTB",,"VI"])*
                                               reconst.n[,c("2005","2006"),"OTB",,"VI"]
  
    reconst.n[,"2009","OTB",,"VI"]<-(landings.n[,"2008","OTB",,"VI"]+landings.n[,"2010","OTB",,"VI"])/2 
    landings.wt[,"2009","OTB",,"VI"]<-(landings.wt[,"2008","OTB",,"VI"]+landings.wt[,"2010","OTB",,"VI"])/2 
    reconst.n[,"2009","OTB",,"VI"]<-reconst.n[,"2009","OTB",,"VI"]%/%quantSums(reconst.n[,"2009","OTB",,"VI"])
    reconst.n[,"2009","OTB",,"VI"]<-(landings[,"2009","OTB",,"VI"]%/%landings.wt[,"2009","OTB",,"VI"])*
    reconst.n[,"2009","OTB",,"VI"]
  
    reconst.n[,"2009","OTB","1","VI"]<-landings.n[,"2008","OTB","1","VI"]
    landings.wt[,"2009","OTB","1","VI"]<-landings.wt[,"2008","OTB","1","VI"]
    reconst.n[,"2009","OTB","1","VI"]<-reconst.n[,"2009","OTB","1","VI"]%/%quantSums(reconst.n[,"2009","OTB","1","VI"])
    reconst.n[,"2009","OTB","1","VI"]<-(landings[,"2009","OTB","1","VI"]%/%landings.wt[,"2009","OTB","1","VI"])*reconst.n[,"2009","OTB","1","VI"]
  
  
    reconst.n[,"2010","OTB",,"VII"]<-landings.n[,"2011","OTB",,"VII"]
    landings.wt[,"2010","OTB",,"VII"]<-landings.wt[,"2011","OTB",,"VII"]
    reconst.n[,"2010","OTB",,"VII"]<-reconst.n[,"2010","OTB",,"VII"]%/%quantSums(reconst.n[,"2010","OTB",,"VII"])
    reconst.n[,"2010","OTB",,"VII"]<-(landings[,"2010","OTB",,"VII"]%/%landings.wt[,"2010","OTB",,"VII"])*
    reconst.n[,"2010","OTB",,"VII"]
  
    reconst.n[,"2010","OTB","2","VII"]<-landings.n[,"2008","OTB","2","VII"]
    landings.wt[,"2010","OTB","2","VII"]<-landings.wt[,"2008","OTB","2","VII"]
    reconst.n[,"2010","OTB","2","VII"]<-reconst.n[,"2010","OTB","2","VII"]%/%quantSums(reconst.n[,"2010","OTB","2","VII"])
    reconst.n[,"2010","OTB","2","VII"]<-(landings[,"2010","OTB","2","VII"]%/%landings.wt[,"2010","OTB","2","VII"])*reconst.n[,"2010","OTB","2","VII"]
  
  
    reconst.n[,,"PTB"]<-reconst.n[,,"OTB"]
    reconst.n[,,"PTB"]<-reconst.n[,,"PTB"]%/%quantSums(reconst.n[,,"PTB"])
    reconst.n[,,"PTB"]<-(landings[,as.character(c(2003:2012)),"PTB"]%/%landings.wt[,,"PTB"])*reconst.n[,,"PTB"]
    
    reconst.n[]<-reconst.n[]%/%quantSums(reconst.n[]) 
    reconst.n[]<-(landings[]%/%landings.wt[])*reconst.n[]   
      
  } else if (i==3){
    #  There is just data for the area VIIIabd, both for total landings and landings by length
    # PTB- 2003 landings there is no landings sampled 
    # Assumptions:PTB- I take the 2004 distribution and apply it to the 2003 PTB landings
    #  Because there were not landings sampled, there is not weight associated, I proceed to calculate it. 
    landings.a <- dimnames(landings)
    landings.a$area<-c("VI","VII","VIIIabd")
    landings.a <-FLQuant(dimnames = landings.a,units="kg")
    landings.a[,,,,c("VII","VIIIabd")]<-landings[,,,,c("VII","VIIIabd")]
    landings<-landings.a
    
    reconst.n <- dimnames(landings.n)
    reconst.n$area<-c("VI","VII","VIIIabd")
    reconst.n <-FLQuant(dimnames = reconst.n,units="1")
    reconst.n[,,,,"VIIIabd"]<-landings.n [,,,,"VIIIabd"]
    
    landings.wtt <- dimnames(landings.wt)
    landings.wtt$area<-c("VI","VII","VIIIabd")
    landings.wtt <-FLQuant(dimnames = landings.wtt,units="kg")
    landings.wtt[,,,,"VI"]<-landings.wt [,,,,"VIIIabd"]
    landings.wtt[,,,,"VII"]<-landings.wt[,,,,"VIIIabd"]
    landings.wtt[,,,,"VIIIabd"]<-landings.wt[,,,,"VIIIabd"]
    landings.wt<-landings.wtt
    
    reconst.n[,"2012","OTB",,"VIIIabd"]<-reconst.n[,"2011","OTB",,"VIIIabd"] 
    landings.wt[,"2012","OTB",,"VIIIabd"]<-landings.wt[,"2011","OTB",,"VIIIabd"] 
    reconst.n[,"2012","OTB",,"VIIIabd"]<-reconst.n[,"2012","OTB",,"VIIIabd"]%/%quantSums(reconst.n[,"2012","OTB",,"VIIIabd"])
    reconst.n[,"2012","OTB",,"VIIIabd"]<-(landings[,"2012","OTB",,"VIIIabd"]%/%landings.wt[,"2012","OTB",,"VIIIabd"])*reconst.n[,"2012","OTB",,"VIIIabd"]
    
    reconst.n[,,"OTB",,"VII"]<-reconst.n[,,"OTB",,"VIIIabd"] 
    landings.wt[,,"OTB",,"VII"]<-landings.wt[,,"OTB",,"VIIIabd"]
    reconst.n[,,"OTB",,"VII"]<-reconst.n[,,"OTB",,"VII"]%/%quantSums(reconst.n[,,"OTB",,"VII"])
    reconst.n[,"2005","OTB",,"VII"]<-(landings[,"2005","OTB",,"VII"]%/%landings.wt[,"2005","OTB",,"VII"])*reconst.n[,"2005","OTB",,"VII"]
    reconst.n[,"2006","OTB",,"VII"]<-(landings[,"2006","OTB",,"VII"]%/%landings.wt[,"2006","OTB",,"VII"])*reconst.n[,"2006","OTB",,"VII"]
    
    
    reconst.n[,"2003","PTB",,"VIIIabd"]<-landings.n[,"2004","PTB",,"VIIIabd"] 
    landings.wt[,"2003","PTB",,"VIIIabd"]<-landings.wt[,"2004","PTB",,"VIIIabd"]
    reconst.n[,"2003","PTB",,"VIIIabd"]<-reconst.n[,"2003","PTB",,"VIIIabd"]%/%quantSums(reconst.n[,"2003","PTB",,"VIIIabd"])
    reconst.n[,"2003","PTB",,"VIIIabd"]<-(landings[,"2003","PTB",,"VIIIabd"]%/%landings.wt[,"2003","PTB",,"VIIIabd"])*reconst.n[,"2003","PTB",,"VIIIabd"]
    
    reconst.n[,c("2008","2009"),"PTB",,"VIIIabd"]<-landings.n[,c("2006","2006"),"PTB",,"VIIIabd"] 
    landings.wt[,c("2008","2009"),"PTB",,"VIIIabd"]<-landings.wt[,c("2006","2006"),"PTB",,"VIIIabd"]
    reconst.n[,c("2008","2009"),"PTB",,"VIIIabd"]<-reconst.n[,c("2008","2009"),"PTB",,"VIIIabd"]%/%quantSums(reconst.n[,c("2008","2009"),"PTB",,"VIIIabd"])
    reconst.n[,c("2008","2009"),"PTB",,"VIIIabd"]<-(landings[,c("2008","2009"),"PTB",,"VIIIabd"]%/%landings.wt[,c("2008","2009"),"PTB",,"VIIIabd"])*reconst.n[,c("2008","2009"),"PTB",,"VIIIabd"]
    
    reconst.n[]<-reconst.n[]%/%quantSums(reconst.n[]) 
    reconst.n[]<-(landings[]%/%landings.wt[])*reconst.n[]
    
} else if (i==4){
  # Missing data: OTB- area VIIIabd there are no samples for 2005 and 2006, for area VII no samples at all (landings are few)
  #  PTB- there are landings in area VIIIabd, but length samples are only available for 2003. In area VII occurs the same: few 
  #  landings with no samples at all.
  # Assumptions: OTB-VIIIabd, I take 2004 and 2006 to make the mean for 2005. For 2007 I take the 2006 distribution
  #             OTB- VII, the same distribution as VIIIabd
  #  PTB- I take the 2003 distribution and apply it to the whole PTB landings
  #  Because there were not landings sampled, there is not weight associated, I proceed to calculate it. 
  
  reconst.n <- dimnames(landings.n)
  reconst.n$year<-c(as.character(c(2003:2012)))
  reconst.n$area<-c("VI","VII","VIIIabd")
  reconst.n <-FLQuant(dimnames = reconst.n,units="1")
  reconst.n[,as.character(c(2003:2012)),,,"VIIIabd",]<-landings.n [,as.character(c(2003:2012)),,,"VIIIabd",]
  
  landings.wtt <- dimnames(landings.wt)
  landings.wtt$year<-c(as.character(c(2003:2012)))
  landings.wtt$area<-c("VI","VII","VIIIabd")
  landings.wtt <-FLQuant(dimnames = landings.wtt,units="kg")
  landings.wtt[,as.character(c(2003:2012)),,,"VIIIabd",]<-landings.wt[,as.character(c(2003:2012)),,,"VIIIabd",]
  landings.wt<-landings.wtt
  
  reconst.n[,"2005","OTB",,"VIIIabd"]<-(landings.n[,"2004","OTB",,"VIIIabd"]+landings.n[,"2006","OTB",,"VIIIabd"])/2 
  landings.wt[,"2005","OTB",,"VIIIabd"]<-(landings.wt[,"2004","OTB",,"VIIIabd"]+landings.wt[,"2006","OTB",,"VIIIabd"])/2 
  reconst.n[,"2005","OTB",,"VIIIabd"]<-reconst.n[,"2005","OTB",,"VIIIabd"]%/%quantSums(reconst.n[,"2005","OTB",,"VIIIabd"])
  reconst.n[,"2005","OTB",,"VIIIabd"]<-(landings[,"2005","OTB",,"VIIIabd"]%/%landings.wt[,"2005","OTB",,"VIIIabd"])*reconst.n[,"2005","OTB",,"VIIIabd"]
    
  reconst.n[,"2007","OTB",,"VIIIabd"]<-(landings.n[,"2006","OTB",,"VIIIabd"] +landings.n[,"2008","OTB",,"VIIIabd"])/2 
  landings.wt[,"2007","OTB",,"VIIIabd"]<-(landings.wt[,"2006","OTB",,"VIIIabd"]+landings.wt[,"2008","OTB",,"VIIIabd"])/2 
  reconst.n[,"2007","OTB",,"VIIIabd"]<-reconst.n[,"2007","OTB",,"VIIIabd"]%/%quantSums(reconst.n[,"2007","OTB",,"VIIIabd"])
  reconst.n[,"2007","OTB",,"VIIIabd"]<-(landings[,"2007","OTB",,"VIIIabd"]%/%landings.wt[,"2007","OTB",,"VIIIabd"])*reconst.n[,"2007","OTB",,"VIIIabd"]
  
  reconst.n[,c("2010","2011"),"OTB","3","VIIIabd"]<-landings.n[,c("2011","2011"),"OTB","3","VIIIabd"] 
  landings.wt[,c("2010","2011"),"OTB","3","VIIIabd"]<-landings.wt[,c("2011","2011"),"OTB","3","VIIIabd"] 
  reconst.n[,c("2010","2011"),"OTB","3","VIIIabd"]<-reconst.n[,c("2010","2011"),"OTB","3","VIIIabd"]%/%quantSums(reconst.n[,c("2010","2011"),"OTB","3","VIIIabd"])
  reconst.n[,c("2010","2011"),"OTB","3","VIIIabd"]<-(landings[,c("2010","2011"),"OTB","3","VIIIabd"]%/%landings.wt[,c("2010","2011"),"OTB","3","VIIIabd"])*reconst.n[,c("2010","2011"),"OTB","3","VIIIabd"]
  
  reconst.n[,,"OTB",,"VII"]<-reconst.n[,,"OTB",,"VIIIabd"] 
  landings.wt[,,"OTB",,"VII"]<-landings.wt[,,"OTB",,"VIIIabd"]
  reconst.n[,,"OTB",,"VII"]<-reconst.n[,,"OTB",,"VII"]%/%quantSums(reconst.n[,,"OTB",,"VII"])
  reconst.n[,,"OTB",,"VII"]<-(landings[,as.character(c(2003:2012)),"OTB",,"VII"]%/%landings.wt[,,"OTB",,"VII"])*reconst.n[,,"OTB",,"VII"]
  
  reconst.n[,,"OTB",,"VI"]<-reconst.n[,,"OTB",,"VIIIabd"] 
  landings.wt[,,"OTB",,"VI"]<-landings.wt[,,"OTB",,"VIIIabd"]
  reconst.n[,,"OTB",,"VI"]<-reconst.n[,,"OTB",,"VIIIabd"]%/%quantSums(reconst.n[,,"OTB",,"VIIIabd"])
  reconst.n[,,"OTB",,"VI"]<-(landings[,as.character(c(2003:2012)),"OTB",,"VI"]%/%landings.wt[,,"OTB",,"VIIIabd"])*reconst.n[,,"OTB",,"VI"]
  
  reconst.n[,as.character(c(2004:2009)),"PTB",,"VIIIabd"]<-landings.n[,"2003","PTB",,"VIIIabd"] 
  landings.wt[,as.character(c(2004:2009)),"PTB",,"VIIIabd"]<-landings.wt[,"2003","PTB",,"VIIIabd"]
  reconst.n[,as.character(c(2004:2009)),"PTB",,"VIIIabd"]<-reconst.n[,"2003","PTB",,"VIIIabd"]%/%quantSums(reconst.n[,"2003","PTB",,"VIIIabd"])
  reconst.n[,as.character(c(2004:2009)),"PTB",,"VIIIabd"]<-(landings[,as.character(c(2004:2009)),"PTB",,"VIIIabd"]%/%landings.wt[,as.character(c(2004:2009)),"PTB",,"VIIIabd"])*reconst.n[,as.character(c(2004:2009)),"PTB",,"VIIIabd"]
  
  reconst.n[,"2012","PTB",,"VIIIabd"]<-landings.n[,"2011","PTB",,"VIIIabd"] 
  landings.wt[,"2012","PTB",,"VIIIabd"]<-landings.wt[,"2011","PTB",,"VIIIabd"] 
  reconst.n[,"2012","PTB",,"VIIIabd"]<-reconst.n[,"2012","PTB",,"VIIIabd"]%/%quantSums(reconst.n[,"2012","PTB",,"VIIIabd"])
  reconst.n[,"2012","PTB",,"VIIIabd"]<-(landings[,"2012","PTB",,"VIIIabd"]%/%landings.wt[,"2012","PTB",,"VIIIabd"])*reconst.n[,"2012","PTB",,"VIIIabd"]
  
  reconst.n[,,"PTB",,"VII"]<-reconst.n[,,"PTB",,"VIIIabd"] 
  landings.wt[,,"PTB",,"VII"]<-landings.wt[,,"PTB",,"VIIIabd"]
  reconst.n[,,"PTB",,"VII"]<-reconst.n[,,"PTB",,"VII"]%/%quantSums(reconst.n[,,"PTB",,"VII"])
  reconst.n[,,"PTB",,"VII"]<-(landings[,as.character(c(2003:2012)),"PTB",,"VII"]%/%landings.wt[,,"PTB",,"VII"])*reconst.n[,,"PTB",,"VII"]
  
  reconst.n[]<-reconst.n[]%/%quantSums(reconst.n[]) 
  reconst.n[]<-(landings[]%/%landings.wt[])*reconst.n[]
  
} else if (i==5){ 
  # Missing data: PTB-VIIIabd-2007. There is just one landings sample for the whole dataset. Looking to the length frequency distribution
  #             It seems that the selectivity pattern is well represented, but not data for the first quarter.
  # Assumptions: I take the unique length frequency distribution and I apply it for the whole dataset.
  #    I apply the second quarter distribution to the first one as well.
  #  Because there were not landings sampled, there is not weight associated, I proceed to calculate it.  
  
  #proportion
  reconst.n <- dimnames(landings.n)
  reconst.n$year<-c(as.character(c(2003:2012)))
  reconst.n$area<-c("VI","VII","VIIIabd")
  reconst.n$unit<-c("OTB","PTB")
  reconst.n <-FLQuant(dimnames = reconst.n,units="1")
  
  
  landings.wtt <- dimnames(landings.wt)
  landings.wtt$year<-c(as.character(c(2003:2012)))
  landings.wtt$area<-c("VI","VII","VIIIabd")
  landings.wtt$unit<-c("OTB","PTB")
  landings.wtt <-FLQuant(dimnames = landings.wtt,units="kg")
  landings.wtt[]<-landings.wt[,"2011","PTB","1","VIIIabd",]

  reconst.n[]<-landings.n[,"2007","PTB","2","VIIIabd",]%/%quantSums(landings.n[,"2007","PTB","2","VIIIabd",])
  reconst.n[ ]<-(landings[,as.character(c(2003:2012))]%/%landings.wtt)*reconst.n
  
  #plot(landings.n[,"2007","PTB","2","VIIIabd",],reconst.n[,"2007","PTB","2","VIIIabd",])
  # Checking the results of doing this reconstruction, it is appreciated a substantial change in the landings.n 

  reconst.n[,"2007","PTB",c("2","3","4"),"VIIIabd", ]<-landings.n[,"2007","PTB",c("2","3","4"),"VIIIabd",]
  landings.wtt[,"2007","PTB",c("2","3","4"),"VIIIabd", ]<-landings.wt[,"2007","PTB",c("2","3","4"),"VIIIabd",]
  
  reconst.n[,"2011","PTB",c("1","2"),"VIIIabd", ]<-landings.n[,"2011","PTB",c("1","2"),"VIIIabd", ]
  landings.wtt[,"2011","PTB",c("1","2"),"VIIIabd", ]<-landings.wt[,"2011","PTB",c("1","2"),"VIIIabd", ]
  
  reconst.n[,"2012","PTB",c("1","2","3","4"),"VIIIabd", ]<-landings.n[,"2012","PTB",c("1","2","3","4"),"VIIIabd", ]
  landings.wtt[,"2012","PTB",c("1","2","3","4"),"VIIIabd", ]<-landings.wt[,"2012","PTB",c("1","2","3","4"),"VIIIabd", ]
  
  landings.wt<-landings.wtt
  
  reconst.n[]<-reconst.n[]%/%quantSums(reconst.n[]) 
  reconst.n[]<-(landings[]%/%landings.wt[])*reconst.n[]
  
  
  
} else if (i==6 ){ #MON  have the same sampling frequency. I apply the same reconstruction method for both
  # Missing data: PTB- There is just one landings sample for the whole dataset. Looking to the length frequency distribution
  #             It seems that the selectivity pattern is well represented.
  #               OTB- VI missing 2005 and 2006 landings sampled data.
  # Assumptions: I take the unique length frequency distribution and I apply it for the whole dataset.
  #         OTB- I calculate the 2004-2007 mean and applied to the 2005-2006 landings.
  #  Because there were not landings sampled, there is not weight associated, I proceed to calculate it. 
     
  reconst.n[,as.character(c("2005","2006")),"OTB",,"VI"]<-(landings.n[,"2004","OTB",,"VI"]+landings.n[,"2007","OTB",,"VI"])/2 
  landings.wt[,c("2005","2006"),"OTB",,"VI"]<-(landings.wt[,"2004","OTB",,"VI"]+landings.wt[,"2007","OTB",,"VI"])/2 
  reconst.n[,c("2005","2006"),"OTB",,"VI"]<-reconst.n[,"2005","OTB",,"VI"]%/%quantSums(reconst.n[,"2005","OTB",,"VI"])
  reconst.n[,c("2005","2006"),"OTB",,"VI"]<-(landings[,c("2005","2006"),"OTB",,"VI"]%/%landings.wt[,c("2005","2006"),"OTB",,"VI"])*
    reconst.n[,c("2005","2006"),"OTB",,"VI"]
  
  
  reconst.n[,,"PTB",,]<-landings.n[,"2004","PTB",,"VIIIabd",]%/%quantSums(landings.n[,"2004","PTB",,"VIIIabd",])
  landings.wt[,,"PTB",,]<-landings.wt[,"2004","PTB",,"VIIIabd",]
  
  reconst.n[,,"PTB",,]<-(landings[,as.character(c(2003:2012)),"PTB",,]%/%landings.wt[,,"PTB",,])*reconst.n[,,"PTB",,]
  
  reconst.n[,"2004","PTB",,"VIIIabd",]<-landings.n[,"2004","PTB",,"VIIIabd",]
  landings.wt[,"2004","PTB",,"VIIIabd",]<-landings.wt[,"2004","PTB",,"VIIIabd",]
  
  reconst.n[,"2008","PTB",,"VIIIabd",]<-landings.n[,"2008","PTB",,"VIIIabd",]
  landings.wt[,"2008","PTB",,"VIIIabd",]<-landings.wt[,"2008","PTB",,"VIIIabd",]
  
  reconst.n[]<-reconst.n[]%/%quantSums(reconst.n[]) 
  reconst.n[]<-(landings[]%/%landings.wt[])*reconst.n[]

} else if (i==7){ #ANK have the same sampling frequency. I apply the same reconstruction method for both
  # Missing data: PTB- There is just one landings sample for the whole dataset. Looking to the length frequency distribution
  #             It seems that the selectivity pattern is well represented.
  #               OTB- VI missing 2005 and 2006 landings sampled data.
  # Assumptions: I take the unique length frequency distribution and I apply it for the whole dataset.
  #         OTB- I calculate the 2004-2007 mean and applied to the 2005-2006 landings.
  #  Because there were not landings sampled, there is not weight associated, I proceed to calculate it. 
  
    discards.a <- dimnames(discards)
    discards.a$area<-c("VI","VII","VIIIabd")
    discards.a <-FLQuant(dimnames = discards.a,units="kg")
    discards.a[,,,,c("VII","VIIIabd")]<-discards[,,,,c("VII","VIIIabd")]
    discards<-discards.a
    units(discards)<-"kg"
    
    discards.b <- dimnames(discards.n)
    discards.b$area<-c("VI","VII","VIIIabd")
    discards.b <-FLQuant(dimnames = discards.b,units="1")
    discards.b[,,,,c("VII","VIIIabd")]<-discards.n[,,,,c("VII","VIIIabd")]
    discards.n<-discards.b
    units(discards.n)<-"1"
    
    
    discards.c <- dimnames(discards.wt)
    discards.c$area<-c("VI","VII","VIIIabd")
    discards.c <-FLQuant(dimnames = discards.c,units="kg")
    discards.c[,,,,c("VII","VIIIabd")]<-discards.wt[,,,,c("VII","VIIIabd")]
    discards.wt<-discards.c
    units(discards.wt)<-"kg"
    
    reconst.n[,c("2005","2006"),"OTB",,"VI"]<-landings.n[,c("2005","2006"),"OTB",,"VII"]
    landings.wt[,c("2005","2006"),"OTB",,"VI"]<-landings.wt[,c("2005","2006"),"OTB",,"VII"]
    reconst.n[,c("2005","2006"),"OTB",,"VI"]<-reconst.n[,c("2005","2006"),"OTB",,"VI"]%/%quantSums(reconst.n[,c("2005","2006"),"OTB",,"VI"])
    reconst.n[,c("2005","2006"),"OTB",,"VI"]<-(landings[,c("2005","2006"),"OTB",,"VI"]%/%landings.wt[,c("2005","2006"),"OTB",,"VI"])*reconst.n[,c("2005","2006"),"OTB",,"VI"]
  
    reconst.n[,"2012","OTB",,"VI"]<-landings.n[,"2011","OTB",,"VI"]
    landings.wt[,"2012","OTB",,"VI"]<-landings.wt[,"2011","OTB",,"VI"]
    reconst.n[,"2012","OTB",,"VI"]<-reconst.n[,"2012","OTB",,"VI"]%/%quantSums(reconst.n[,"2012","OTB",,"VI"])
    reconst.n[,"2012","OTB",,"VI"]<-(landings[,"2012","OTB",,"VI"]%/%landings.wt[,"2012","OTB",,"VI"])*reconst.n[,"2012","OTB",,"VI"]
    
    reconst.n[,,"PTB",,]<-landings.n[,"2004","PTB",,"VIIIabd",]%/%quantSums(landings.n[,"2004","PTB",,"VIIIabd",])
    landings.wt[,,"PTB",,]<-landings.wt[,"2004","PTB",,"VIIIabd",]
  
    reconst.n[,,"PTB",,]<-(landings[,as.character(c(2003:2012)),"PTB",,]%/%landings.wt[,as.character(c(2003:2012)),"PTB",,])*reconst.n[,,"PTB",,]
  
    reconst.n[,"2004","PTB",,"VIIIabd",]<-landings.n[,"2004","PTB",,"VIIIabd",]
    landings.wt[,"2004","PTB",,"VIIIabd",]<-landings.wt[,"2004","PTB",,"VIIIabd",]
    
    reconst.n[,as.character(c(2008:2012)),"PTB",,"VIIIabd",]<-landings.n[,as.character(c(2008:2012)),"PTB",,"VIIIabd",]
    landings.wt[,as.character(c(2008:2012)),"PTB",,"VIIIabd",]<-landings.wt[,as.character(c(2008:2012)),"PTB",,"VIIIabd",]
    
    reconst.n[]<-reconst.n[]%/%quantSums(reconst.n[]) 
    reconst.n[]<-(landings[]%/%landings.wt[])*reconst.n[]
}
 
  
  catch<-FLQuants(landings=landings,
                  landings_l.n=landings.n,
                  landings_l.wt=landings.wt,
                  reconst_l.n=reconst.n,
                  discards=discards,
                  discards_l.n=discards.n,
                  discards_l.wt=discards.wt)
  
  assign(paste("catch",species[i],sep="_"),catch) 
  
}
  
save(catch_ANK,catch_HKE,catch_HOM,catch_MAC,catch_MEG,
     catch_MON,catch_WHB, file="~/Documents/BoB/BoB_data/data/Catch_data_BoB.RData")
  
  #--------------------------------------------------------------------------------------
  # Create a data frame, put 0 where is no data
  #--------------------------------------------------------------------------------------
  
  fq<-landings.n
  fq[]<-as.numeric(!is.na(fq)) # 0=NA, 1=data
  
  fd<-as.data.frame(fq)
  head(fd)
  
  
  # number of samples measured
  #OTB
  unclass(by(fd[,"data"], list(year=fd$year, unit=fd$unit, season=fd$season, area=fd$area), sum))[,1,,]
  
  #PTB
  unclass(by(fd[,"data"], list(year=fd$year, unit=fd$unit, season=fd$season, area=fd$area), sum))[,2,,]
  
  
  #--------------------------------------------------------------------------------------
  # PLOTTINg DATA GAPS + LENGTH DISTRIBUTION
  #--------------------------------------------------------------------------------------
  
  # Compare the sum of the total sampled weight by length with the total landings
  # And take a look on the length frequency distribution 
  
  tot<-quantSums(landings.n%*%landings.wt,na.rm=TRUE)
  units(tot)<-"kg"
  
  #OTB
  a<-tot[,,"OTB",]
  quant(a)<-"quant"
  b<-landings[,,"OTB",]  
  dat<-FLQuants(sampled=a,total=b) 
  ggplot(dat,aes(season, data,fill=qname)) +geom_bar(stat="identity",position="dodge")+
    facet_grid(area~year,scales="free") +
    labs(x="season",y="Total Landings (kg)",title="HOM-OTB")
  
  # NOrmalizing the landings 
  a<-landings.n[,,"OTB",]%/%quantSums(landings.n[,,"OTB",])
  units(a)<-"1"
  ggplot(a, aes(len, data, group=as.factor(year),colour=as.factor(year))) + geom_line()+ 
    facet_grid(area~season) + 
    labs(x="length (cm)",y="Landings Normalized",title="HOM-OTB")
  
  #PTB
  c<-tot[,,"PTB",]
  quant(c)<-"quant"
  d<-landings[,,"PTB",]  
  dat<-FLQuants(sampled=c,total=d) 
  ggplot(dat,aes(season, data,fill=qname)) +geom_bar(stat="identity",position="dodge")+
    facet_grid(area~year,scales="free") +
    labs(x="season",y="Total Landings (kg)",title="HOM-PTB")
  
  # NOrmalize the landings
  a<-reconst.n[,,"PTB",]%/%quantSums(reconst.n[,,"PTB",])
  units(a)<-"1"
  ggplot(a, aes(len, data, group=as.factor(year),colour=as.factor(year))) + geom_line()+ 
    facet_grid(area~season) + 
    labs(x="length (cm)",y="Landings Normalized",title="HOM-PTB")
  
  #--------------------------------------------------------------------------------------
  # LENGTH- WEIGHT RELATIONSHIP
  #--------------------------------------------------------------------------------------
  
  # Check the mean weights along the years/ areas/ gears; 
  # The mean weight is the same 
  fq<-landings.wt
  ggplot(fq, aes(len, data,group=year,colour=year)) + geom_line()+ 
    #facet_grid(area~unit) + 
    labs(x="length (cm)",y="mean_weight",title="Length~ Weight")
  
  #--------------------------------------------------------------------------------------
  # PLOTTINg DATA GAPS
  #--------------------------------------------------------------------------------------
  
 
  tot<-quantSums(reconst.n%*%landings.wt,na.rm=TRUE)
  units(tot)<-"kg"
  
  #OTB
  a<-tot[,,1,]
  quant(a)<-"quant"
  b<-landings[,,1,]
  
  dat<-FLQuants(sampled=a,total=b)
  
ggplot(dat,aes(season, data,fill=qname)) +geom_bar(stat="identity",position="dodge")+
  facet_grid(area~year,scales="free") +
  labs(x="season",y="Total Landings (kg)",title="HKE-OTB (reconstructed)")
 
  
  #PTB
  a<-tot[,,2,]
  quant(a)<-"quant"
  b<-landings[,,2,]
  
  dat<-FLQuants(sampled=a,total=b)
  
  ggplot(dat,aes(season, data,fill=qname)) +geom_bar(stat="identity",position="dodge")+
    facet_grid(area~year, scales="free") +
    labs(x="season",y="Total Landings (kg)",title="HKE-PTB (reconstructed)")
 
  
  
#   #--------------------------------------------------------------------------------------------
#   # VARIABLES 
#   
#   year<- c(2003:2012)
#   unit<- c("OTB", "PTB") 
#   season<- c(1:4)
#   area<- c("VI", "VII","VIIIabd")
#   
#   
#   for (j in 1:length(year)){
#     for (k in 1:length(unit)){
#       for (s in 1:length(season)){
#         for (l in 1:length(area)){
#           
#           reconst.n<-landings.n
#           
#           a<-is.na(quantSums(landings.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]]))
#           b<-!is.na(landings[,as.character(year[j]),unit[k],as.character(season[s]),area[l]])
#           c<-!is.na(quantSums(landings.n[,as.character(year[j-1]),unit[k],as.character(season[s]),area[l]])) 
#           d<-!is.na(quantSums(landings.n[,as.character(year[j+1]),unit[k],as.character(season[s]),area[l]]))
#           e<-is.na(quantSums(landings.n[,as.character(year[j+1]),unit[k],as.character(season[s]),area[l]]))
#           f<-is.na(quantSums(landings.n[,as.character(year[j-1]),unit[k],as.character(season[s]),area[l]]))
#           
#           
#           if ( a && b) #si el sumatorio de los n=NA y el valor de landings correspondiente es distinto de NA 
#           {
#             
#             if ( c && d)  # si hay valores en el ano anterior y posterior, se hace la media   
#             {
#               
#               
#               #N mean from the previous and the following year
#               reconst.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]]<- (landings.n[,as.character(year[j-1]),unit[k],as.character(season[s]),area[l]] +
#                                                                                              landings.n[,as.character(year[j+1]),unit[k],as.character(season[s]),area[l]])/2
#               reconst.n[,as.character(year[j-1]:year[j+1]),unit[k],as.character(season[s]),area[l]]
#               
#               #N proportion
#               reconst.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]]<-reconst.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]] %/%
#                 quantSums(reconst.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]])
#               
#               # N final
#               reconst.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]]<-(landings[,as.character(year[j]),unit[k],as.character(season[s]),area[l]] %/%
#                                                                                             landings.wt[,as.character(year[j-1]),unit[k],as.character(season[s]),area[l]]) *
#                 reconst.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]]
#               reconst.n[,as.character(year[j-1]:year[j+1]),unit[k],as.character(season[s]),area[l]]
#               
#               #Checking
#               tot_weight<-quantSums( reconst.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]] *
#                                        landings.wt[,as.character(year[j-1]),unit[k],as.character(season[s]),area[l]])
#               landings[,as.character(year[j]),unit[k],as.character(season[s]),area[l]]
#               
#             } else if ( c & e)
#             {
#               #si solo hay dato el anos anterior, solo cojo ese mismo valor de n
#               reconst.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]]<- landings.n[,as.character(year[j-1]),unit[k],as.character(season[s]),area[l]]            
#               
#             } else if ( f & d)
#             {
#               #si solo hay dato el anos posterior, solo cojo ese mismo valor de n 
#               reconst.n[,as.character(year[j]),unit[k],as.character(season[s]),area[l]]<- landings.n[,as.character(year[j+1]),unit[k],as.character(season[s]),area[l]]  
#               
#             }
#           }
#         }
#       }
#       
#     }
#   }
# }


  
