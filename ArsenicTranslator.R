#look into relationships between total recoverable arsenic and Inorganic Arsenic

library(readxl)
library(tidyverse)
library(AWQMSdata)

#download all arsenic and inorganic arsenic data from AWQMS from 2000- just looking at effluent for the moment
source("NPDES_AWQMSQuery.R")
ars<-NPDES_AWQMS_Qry(startdate="2000-01-01",char=c("Arsenic","Arsenic, Inorganic"),
                     montype=c("Facility Municipal Sewage (POTW)","Facility Other","Facility Industrial"))

#convert all to ug/L
ars<-unit_conv(ars,c("Arsenic","Arsenic, Inorganic"),"mg/l","ug/l")

#cannibalize dissolved vs total recoverable function from EDD data checks to compare total recoverable arsenic with inorganic arsenic

    
    #create new identifier out of date and characteristic name
    #(used to use activity ID, but a lot of the labs have been using multiple activity IDs per batch 
    #(e.g. unfilterd sample is xx-001A and filtered sample is xx-001B))
    #use date and location since it is extremely rare for a permittee to be taking more than one sample a day at each location for metals data
    ars$comb<-paste0(ars$SampleStartDate,",",ars$MLocID)
    
    #get unique identifiers, put into new dataset
    new<-unique(subset(ars,select=c("act_id","SampleStartDate","MLocID","Org_Name","Char_Name","comb")))
    
    #select all TR arsenic
    trars<-subset(ars,ars$Sample_Fraction %in% c("Total Recoverable","Total") & ars$Char_Name %in% "Arsenic")
    
    #select all inorganic arsenic
    inars<-subset(ars,ars$Sample_Fraction %in% c("Total Recoverable","Total") & ars$Char_Name %in% "Arsenic, Inorganic")
    
    #add arsenic and inorganic arsenic to new dataset
    new$trars<-trars$Result_Numeric[match(new$comb,trars$comb,nomatch=NA)]
    new$inars<-inars$Result_Numeric[match(new$comb,inars$comb,nomatch=NA)]
    
    #add MDL value
    new$trarsMDL<-trars$MDLValue[match(new$comb,trars$comb,nomatch=NA)]
    new$inarsMDL<-inars$MDLValue[match(new$comb,inars$comb,nomatch=NA)]

    
    #calculate the difference, round to 3 figures
    new$diff<-round(new$trars-new$inars,3)
    
    #calculate the percent of arsenic that is inorganic arsenic
    new$percinorg<-(new$inars/new$trars)*100
    
    #round the percentage
    new$percinorg<-round(new$percinorg,2)
    
    #remove all data points where there is no comparison, just keep arsenic so we don't have duplicate rows
    new<-subset(new, !(is.na(new$percinorg))& new$Char_Name %in% "Arsenic")
    
max(new$percinorg)
min(new$percinorg)
mean(new$percinorg)
median(new$percinorg)
sd(new$percinorg)
nrow(new)

