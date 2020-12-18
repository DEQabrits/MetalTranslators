#Create function to get percent of a metal that is dissolved (cannibalized from EDD function)


PercDiss<-function (x)
{
#create new identifier out of date and characteristic name
#(used to use activity ID, but a lot of the labs have been using multiple activity IDs per batch 
#(e.g. unfilterd sample is xx-001A and filtered sample is xx-001B))
#use date and location since it is extremely rare for a permittee to be taking more than one sample a day at each location for metals data
x$comb<-paste0(x$SampleStartDate,",",x$MLocID)

#get unique identifiers, put into new dataset
new<-unique(subset(x,select=c("SampleStartDate","MLocID","Org_Name","HUC8","HUC8_Name","MonLocType","Char_Name","comb")))

#select all dissolved
dis<-subset(x,x$Sample_Fraction %in% "Dissolved")

#select all total recoverable/total
tot<-subset(x,x$Sample_Fraction %in% c("Total Recoverable","Total"))

#add dissolved and total columns to new dataset
new$dissolved<-dis$Result_Numeric[match(new$comb,dis$comb,nomatch=NA)]
new$total<-tot$Result_Numeric[match(new$comb,tot$comb,nomatch=NA)]

#add MDL value (take lower value if one is lower than the other (makes for more conservative test))
new$totMDL<-tot$MDLValue[match(new$comb,tot$comb,nomatch=NA)]
new$disMDL<-dis$MDLValue[match(new$comb,dis$comb,nomatch=NA)]


#calculate the difference, round to 3 figures
new$diff<-round(new$total-new$dissolved,3)

#calculate the percent of arsenic that is inorganic arsenic
new$perc<-(new$dissolved/new$total)*100

#round the percentage
new$perc<-round(new$perc,2)

#remove any that are greater than 100% (probably sampling error)
new<-subset(new, new$perc<=100)

#remove all data points where there is no comparison, just keep copper so we don't have duplicate rows
new<-unique(subset(new, !(is.na(new$perc))))

return(new)

}