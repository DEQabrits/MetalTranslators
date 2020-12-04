#determine if copper translator is feasible

library(readxl)
library(tidyverse)


#just looking at copper effluent for the moment
source("NPDES_AWQMSQuery.R")
cop<-NPDES_AWQMS_Qry(startdate="2000-01-01",char="Copper",
                     montype=c("Facility Municipal Sewage (POTW)","Facility Other","Facility Industrial"))

#cannibalize the Dissolved vs Total Recoverable code from EDD quality control check script to compare total and dissolved copper
#get data with fraction outlined (should be everything anyway)
cop<-subset(cop,cop$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total"))

#convert all to ug/L
cop<-unit_conv(cop,c("Copper"),"mg/l","ug/l")

#create new identifier out of date and characteristic name
#(used to use activity ID, but a lot of the labs have been using multiple activity IDs per batch 
#(e.g. unfilterd sample is xx-001A and filtered sample is xx-001B))
#use date and location since it is extremely rare for a permittee to be taking more than one sample a day at each location for metals data
cop$comb<-paste0(cop$SampleStartDate,",",cop$MLocID)

#get unique identifiers, put into new dataset
newcop<-unique(subset(cop,select=c("SampleStartDate","MLocID","Org_Name","Char_Name","comb")))

#select all dissolved
dis<-subset(cop,cop$Sample_Fraction %in% "Dissolved")

#select all total recoverable/total
tot<-subset(cop,cop$Sample_Fraction %in% c("Total Recoverable","Total"))

#add dissolved and total columns to new dataset
newcop$dissolved<-dis$Result_Numeric[match(newcop$comb,dis$comb,nomatch=NA)]
newcop$total<-tot$Result_Numeric[match(newcop$comb,tot$comb,nomatch=NA)]

#add MDL value (take lower value if one is lower than the other (makes for more conservative test))
newcop$totMDL<-tot$MDLValue[match(newcop$comb,tot$comb,nomatch=NA)]
newcop$disMDL<-dis$MDLValue[match(newcop$comb,dis$comb,nomatch=NA)]


#calculate the difference, round to 3 figures
newcop$diff<-round(newcop$total-newcop$dissolved,3)

#calculate the percent of arsenic that is inorganic arsenic
newcop$perc<-(newcop$dissolved/newcop$total)*100

#round the percentage
newcop$perc<-round(newcop$perc,2)

#remove any that are greater than 100% (probably sampling error)
newcop<-subset(newcop, newcop$perc<=100)

#remove all data points where there is no comparison, just keep copper so we don't have duplicate rows
newcop<-unique(subset(newcop, !(is.na(newcop$perc))))
    
max(newcop$perc)
min(newcop$perc)
mean(newcop$perc)
median(newcop$perc)
sd(newcop$perc)
nrow(newcop)

