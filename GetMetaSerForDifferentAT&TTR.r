
#### We start from the file alldetloc30km which lists all bird positions within 30km of a boat.
#### From there we will determine encounter events and perform analyses with different attraction thresholds (and possibly different time to return thresholds)
#### (analses performed in another script)

setwd("D:/Pro/These/Donnees/WAAL/BirdsToVessels/")

alldetloc=read.table("AllDetLoc30km.csv",sep=";",h=T)

### remark: if you want to create this alldetloc file
# open each bird track containing distance to each boat (as returned by BirdToBoatCorrespondence)
# determine the distance to the closest boat (apply(alltracks[,all colons of boat distance)],1,min,na.rm=T)) 
# extract relevant info (which boat is it, what activity, etc)
# Select all track lines (=locs) where Min Dist <30km (or any relevant max attraction threshold)
# store all that in one big file alldetloc, all tracks together.


# Definitions of: attraction range, attendance behaviour, attendance zone
at=c(15,20,25,30)   # we define attraction area as a 30km circle aound boats
sitting.threshold=10/3.6   # we consider locations with speed<10km/h to be sitting on water
attend.threshold=3   # birds sitting within 3km of vessels will be considered attending boats

# Time to return threshold
ttr=c(2,4,8,24,96) #how many locs in between encounters to consider them distinct: 2 (30min), 2h, 6h, 24h


### Determine encounter events for each attrac.thresh/time to return combinations
for (timetoreturn in ttr){
  for (attrac.threshold in at){
    # we keep only those locs within the new attraction thresold value
    allattrloc=alldetloc[alldetloc$DistToVessel<=attrac.threshold*1000,]  # *1000 because DistToVessel in meters
    
    # initiation
    serie=1 # the first loc of alldetloc necessarily belongs to the first encounter
    prevser=1
    indtr=allattrloc$File[1]
    prevloc=allattrloc$LocBird[1]
    # loop to assign encounter event number to each loc <attrac thresh
    for (l in 2:length(allattrloc$Latitude)){ 
      # if new loc is not the same track as previous loc, back to encounter event 1, otherwise check definition (ttr, attrac.thresh)
      serie=c(serie,ifelse(allattrloc$File[l]!=indtr,1,ifelse(((allattrloc$LocBird[l]-prevloc)<timetoreturn),prevser,prevser+1)))
      prevser=serie[l]
      indtr=allattrloc$File[l]
      prevloc=allattrloc$LocBird[l]
      # prevbat=allattrloc$Vessel[l]
    }
    
    allattrloc$Serie=serie
    
    write.table(allattrloc,file=paste("Test/","AllAttrLoc_",attrac.threshold,"km_",attend.threshold,"km_","TTR",timetoreturn,"locs.csv",sep=""),sep=";",row.names=F)
    
  }
}



### Establish encounter metadata for each combination

for (timetoreturn in ttr){
  for (attrac.threshold in at){
    #import correspnding det locs
    allattrloc=read.table(paste("Test/","AllAttrLoc_",attrac.threshold,"km_",attend.threshold,"km_","TTR",timetoreturn,"locs.csv",sep=""),sep=";",h=T)
    
    # initiates metadata table
    attrser=c()
    
    allindtr=unique(allattrloc$File)
    for (it in allindtr){ # for each track
      nbser=max(allattrloc$Serie[allattrloc$File==it],na.rm=T)
      for (ser in 1:nbser){ # for each encounter in the track
        ### Length of encounter event
        durser=sum(allattrloc$File==it & allattrloc$Serie==ser,na.rm=T)
        
        ### Parameters to establish correspondence to bird file
        startloc=allattrloc$LocBird[allattrloc$File==it & allattrloc$Serie==ser][1]
        endloc=allattrloc$LocBird[allattrloc$File==it & allattrloc$Serie==ser][durser]
        startdate=as.character(allattrloc$Date[allattrloc$File==it & allattrloc$Serie==ser][1])
        starttime=as.character(allattrloc$Time[allattrloc$File==it & allattrloc$Serie==ser][1])
        
        
        ### Parameters to establish correspondence to vessel file
        vessel=as.character(allattrloc$Vessel[allattrloc$File==it & allattrloc$Serie==ser][1])
        startlocbat=as.character(allattrloc$LocVessel[allattrloc$File==it & allattrloc$Serie==ser][1])
        endlocbat=as.character(allattrloc$LocVessel[allattrloc$File==it & allattrloc$Serie==ser][durser])
        
        ### Parameters to exclude night encounters, or encounters without flying locations
        day_prop=sum(allattrloc$DayNight[allattrloc$File==it & allattrloc$Serie==ser]=="jour",na.rm=T)/durser
        dayflight=ifelse(length(allattrloc$DayNight[allattrloc$File==it & allattrloc$Serie==ser & allattrloc$DayNight=="jour" & allattrloc$SitWater=="vol"])>0,1,0)
        daystart=as.character(allattrloc$DayNight[allattrloc$File==it & allattrloc$Serie==ser][1])
        
        ### How many boats are within detection range during the encounter
        avg_nbbatrangedet=mean(allattrloc$NbBatRangeDet[allattrloc$File==it & allattrloc$Serie==ser],na.rm=T)
        
        ### Attendance parameters
        #How long did it attend
        dur_attend=sum(allattrloc$File==it & allattrloc$Serie==ser & allattrloc$locint==1)
        #did it attend
        attend=ifelse(dur_attend>0,1,0)
        #what proportion of the encounter event it spent attending
        prop_attend=dur_attend/durser
        #at what average distance from boats when attending
        avgdist_attend=mean(allattrloc$DistToVessel[allattrloc$File==it & allattrloc$Serie==ser & allattrloc$locint==1],na.rm=T)
        
        #year
        year=as.character(allattrloc$Year[allattrloc$File==it & allattrloc$Serie==ser][1])
        
        attrser=rbind(attrser,c(it,ser,year,startloc,startlocbat,startdate,starttime,endloc,endlocbat,durser,day_prop,dayflight,daystart,vessel,avg_nbbatrangedet,attend,dur_attend,prop_attend,avgdist_attend))
      }# end for each encounter
    }# end for each track
    
    write.table(attrser,paste("Test/","DetectionSeries_",attrac.threshold,"km_",attend.threshold,"km_","TTR",timetoreturn,"locs.csv",sep=""),sep=";",row.names=F)
    
    
    
    ### Complete attrser with Boat activity parameters
    attrser=read.table(paste("Test/","DetectionSeries_",attrac.threshold,"km_",attend.threshold,"km_","TTR",timetoreturn,"locs.csv",sep=""),sep=";",h=T)
    names(attrser)=c("File","Serie","Year","startloc","startlocbat","startdate","starttime","endloc","endlocbat","durser","prop_day","dayflight","daystart","vessel","avg_nbbatrangedet","attend","dur_attend","prop_attend","avgdist_attend")
    
    time1=as.POSIXct(apply(cbind(as.character(attrser$startdate),as.character(attrser$starttime)),1,function(X){
      Date=unlist(strsplit(as.character(X[1]),"/"))
      Time=as.character(X[2])
      out=paste(paste(as.character(Date[3]),as.character(Date[2]),as.character(Date[1]), sep='-'),Time, sep=' ')
      return(out)
    }),tz='GMT')
    
    ### determine relevant boat files for each series
    bat=paste("D:/Pro/These/Donnees/Bateaux/VesselsData/InterpolCombinedReady/",attrser$Year,"/interpol_CORR_",attrser$Year,"_CRO_",attrser$vessel,"_Track-Pal.csv",sep='')
    
    lagvesact=c()
    actves=c()
    propact=c()
    for (ser in 1:length(attrser$Year)){
      vessel=read.table(bat[ser],h=T,sep=";")
      
      time2=as.POSIXct(apply(cbind(as.character(vessel$Date),as.character(vessel$Time)),1,function(X){
        Date=unlist(strsplit(as.character(X[1]),"/"))
        Time=as.character(X[2])
        out=paste(paste(as.character(Date[3]),as.character(Date[2]),as.character(Date[1]), sep='-'),Time, sep=' ')
        return(out)
      }),tz='GMT')
      
      startdate=time1[ser]
      
      loclagvesact=which.min(abs(difftime(startdate,time2[vessel$Type_loc!="track" & vessel$Type_loc!="int"],units="secs")))
      lagvesact=c(lagvesact,as.double(difftime(startdate,time2[vessel$Type_loc!="track" & vessel$Type_loc!="int"][loclagvesact],units="secs")))
      actves=c(actves,as.character(vessel$Type_loc[vessel$Type_loc!="track" & vessel$Type_loc!="int"][loclagvesact]))
      propact=c(propact,sum(vessel$Type_loc[attrser$startlocbat[ser]:attrser$endlocbat[ser]]!="track" & vessel$Type_loc[attrser$startlocbat[ser]:attrser$endlocbat[ser]]!="int")/(length(attrser$startlocbat[ser]:attrser$endlocbat[ser])))
      
    }#end for encounter
    
    attrser$lagvesact=-lagvesact
    attrser$closestvesact=actves
    attrser$propact=propact
    
    
    write.table(attrser,paste("Test/","DetectionSeries_",attrac.threshold,"km_",attend.threshold,"km_","TTR",timetoreturn,"locs.csv",sep=""),sep=";",row.names=F)
    
    
  }# end for attrac threshold
}# end for ttr




meta=read.table("MetaDataForAnalysisInCrozet.csv",sep=";",h=T)
meta$Filename=as.character(meta$Filename)

ERs=c() #Encounter Rates for different threshold values
for (timetoreturn in ttr){
  for (attrac.threshold in at){
    attrser=read.table(paste("Test/","DetectionSeries_",attrac.threshold,"km_",attend.threshold,"km_","TTR",timetoreturn,"locs.csv",sep=""),sep=";",h=T)
    
    attrser$FileShort=gsub("DistOis-Bat_j-n_","",gsub("_VitPoseCap.csv","",attrser$File))
    
    #count only encounters with daylight fly
    ER=c()
    for (X in 1:dim(meta)[1]){
      ER=c(ER,ifelse(sum(as.character(attrser$FileShort)==as.character(meta[X,5]))>0,
                     as.double(sum(attrser$dayflight[as.character(attrser$FileShort)==as.character(meta[X,5])])/meta[X,19]),
                     0))
    }
    ERs=cbind(ERs,ER)
    
    # add trip/indiv param to encounter metadata
    attrser$Duration=apply(attrser,1,function(X){meta$Duration[as.character(meta$Filename)==as.character(X[23])]})
    attrser$Sex=apply(attrser,1,function(X){meta$Sexe[as.character(meta$Filename)==as.character(X[23])]})
    attrser$Age=apply(attrser,1,function(X){meta$Age[as.character(meta$Filename)==as.character(X[23])]})
    attrser$Bague=apply(attrser,1,function(X){meta$Bague[as.character(meta$Filename)==as.character(X[23])]})
    attrser$MaxRange=apply(attrser,1,function(X){meta$MaxRange[as.character(meta$Filename)==as.character(X[23])]})
    attrser$avg_nbbat=apply(attrser,1,function(X){meta$avg_nbbat[as.character(meta$Filename)==as.character(X[23])]})
    
    write.table(attrser,paste("Test/","DetectionSeries_",attrac.threshold,"km_",attend.threshold,"km_","TTR",timetoreturn,"locs.csv",sep=""),sep=";",row.names=F)
    
  }# end for attrac threshold
}# end for ttr

colnames(ERs)=c("ER15-2","ER20-2","ER25-2","ER30-2","ER15-4","ER20-4","ER25-4","ER30-4","ER15-8","ER20-8","ER25-8","ER30-8","ER15-24","ER20-24","ER25-24","ER30-24","ER15-96","ER20-96","ER25-96","ER30-96")
meta=cbind(meta,ERs)

write.table(meta,file="Test/MetaWithSensitivityERs.csv",sep=";",row.names=F)

