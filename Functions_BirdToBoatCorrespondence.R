##### Functions to establish distance and direction from bird to vessel
##### Check also Long's package wildlifeDI which is similar but requires adehabitatLT hence projections into 2D geographic system




#### function to extract minimal time difference between a (bird) loc and a (vessel) track
minlag=function(loctime,tracktime){
  lagcarre=as.double(difftime(loctime,tracktime,units="secs"))^2
  minlag=sqrt(min(lagcarre))
  loctrack=which.min(lagcarre)
  return(list(minlag,loctrack))
}

#### function to calculate angle difference between headings in degrees (assumes of course they have the same referential)
diffcap=function(head1,head2){
  mincap=apply(cbind(head1,head2),1,min,na.rm=F)
  maxcap=apply(cbind(head1,head2),1,max,na.rm=F)

#   swtmp=mincap
#   mincap[is.na(swtmp)==F & swtmp>maxcap]=maxcap[is.na(swtmp)==F & swtmp>maxcap]
#   maxcap[is.na(swtmp)==F & swtmp>maxcap]=swtmp[is.na(swtmp)==F & swtmp>maxcap]

  diffcap=apply(cbind(mincap,maxcap),1,function(X){min(c((X[2]-X[1]),((360-X[2])+(X[1]-0))),na.rm=F)})
  return(diffcap)
}


#### function to estimate simultaneous bird-boat positions
### require(geosphere)
#bird = data.frame with at least columns Latitude, Longitude, Date, Time, Loc; with Date in the format dd/mm/yyyy and time hh:mm(:ss); Loc is simply a running index for all locs (from 1 to the number of GPS records of the track)
#vessel=data frame for vessel VMS with columns Latitude, Longitude, Date, Time (and, optional: TypeLoc for differentiating between real and interpolated points, and/or vessel fishing activity)
#dt= maximal time lag (in seconds) allowed for bird and vessel locations to be considered "simultaneous"
# optional: if Date and Time columns are not in the right format, you can pass on TIMEPOSIXct objects to birdtime and birdvessel instead

birdtovessel=function(bird,vessel,dt,vesseltime=NULL,birdtime=NULL){

  # convert boat date and time columns to POSIXct format unless specified in function arguments
  if (is.null(vesseltime)){
    # extract and combine vessel date and time
    vesseltime=as.POSIXct(apply(cbind(as.character(vessel$Date),as.character(vessel$Time)),1,function(X){
      Date=unlist(strsplit(as.character(X[1]),"/"))
      Time=as.character(X[2])
      out=paste(paste(as.character(Date[3]),as.character(Date[2]),as.character(Date[1]), sep='-'),Time, sep=' ')
      return(out)
    }),tz='GMT')
  }

  # convert bird date and time columns to POSIXct format unless specified in function arguments
  if(is.null(birdtime)){
    #extract and combine bird date and time
    birdtime=as.POSIXct(apply(cbind(as.character(bird$Date),as.character(bird$Time)),1,function(X){
      Date=unlist(strsplit(as.character(X[1]),"/"))
      Time=as.character(X[2])
      out=paste(paste(as.character(Date[3]),as.character(Date[2]),as.character(Date[1]), sep='-'),Time, sep=' ')
      return(out)
    }),tz='GMT')
  }

  # we calculate apparent speed and heading between one position and the next, using functions from package geosphere
  ApparentSpeed_metpersec=c(distCosine(cbind(bird$Longitude,bird$Latitude)[-dim(bird)[1],],cbind(bird$Longitude,bird$Latitude)[-1,])/as.double(-difftime(birdtime[-dim(bird)[1]],birdtime[-1],units="secs")),NA)
  Heading=as.double(c(bearing(cbind(bird$Longitude,bird$Latitude)[-dim(bird)[1],],cbind(bird$Longitude,bird$Latitude)[-1,]),NA))%%360

  # we extract for each bird location the boat location with minimum time lag
  listlag=lapply(as.list(birdtime),minlag,vesseltime)
  lags=unlist(lapply(listlag,function(X){X[1]})) # contains lag values
  locbats=unlist(lapply(listlag,function(X){X[2]})) # contains indices for corresponding boat locations

  # we exclude bird-boat correspondence that are above the lag value threshold defined by dt
  bird$Lag=ifelse(lags<=dt,lags,NA)
  bird$LocBat=ifelse(lags<=dt,locbats,NA)

#   # this is assigning an index to bird locations, used below
#   bird$Loc=c(1:dim(bird)[1])

  # Calculate the geographic distance (in km) from the bird location to the corresponding boat location (based on distCosine function from package geosphere, = great circle distance)
  bird$DistToVes_km=plyr::daply(bird,~Loc,function(X){ifelse(is.na(X$LocBat)==F,distCosine(c(X$Longitude,X$Latitude),c(vessel$Longitude[X$LocBat],vessel$Latitude[X$LocBat]))/1000,NA)})

  # Calculate the initial bearing to follow to reach the boat from the bird location (in degrees, 0Â°=North), based on function bearing from geosphere (again assuming great circle movements)
  bird$HeadToVes=plyr::daply(bird,~Loc,function(X){ifelse(is.na(X$LocBat)==F,bearing(c(X$Longitude,X$Latitude),c(vessel$Longitude[X$LocBat],vessel$Latitude[X$LocBat])),NA)})
  bird$HeadToVes=as.double(bird$HeadToVes)%%360

  # Calculate the angular difference between the direction to next bird position and direction to concurrent boat location (0° if bird flew in direction of vessel)
  bird$DiffHead=diffcap(Heading,bird$HeadToVes)

  # If vessel track contains information on fishing activity, in column TypeLoc, then it is added to the bird file.
  bird$VesLocType=rep(NA,dim(bird)[1])
  if(length(vessel$TypeLoc)>0){
    bird$VesLocType=plyr::daply(bird,~id,function(X){vessel$Type_Loc[X$LocBat]})
  }

  # The outcome is the bird track with added columns
  return(bird)
}





#### Script to establish bird-boat correspondences for all birds and boats

birdfiles=list.files("BirdsData/")   # folder containing all and only bird tracks
#vesselsdata="D:/Pro/These/Donnees/Bateaux/VesselsData/InterpolCombinedReady/Ker2011-2min/"
boatfiles=list.files("VesselsData/")  # folder containing all and only boat tracks

for (bird in birdfiles){ # for all bird tracks

  bi=read.table(paste("BirdsData/",bird,sep=""),sep=";",h=T) #read the bird track: the argument sep= will depend your file format and computer default language

  # We create a column with a running index for each loc
  bi$Loc=c(1:dim(bi)[1])

  # For this bird track, we will calculate correspondences to each of the boats in turn, and add corresponding variables (distance to vessel, heading to vessel etc) in new columns. If you have a huge number of boats you might consider alternatives

  for(boat in boatfiles){ # for all boat tracks
    bo=read.table(paste(vesselsdata,boat,sep=""),sep=";",h=T) # read one boat track
    namesini=names(bi)   # we store column names of your input bird track
    bi=birdtovessel(bi,bo,dt=301)  # we calculate correspondences to this boat
    i=which(boatfiles==boat)   # we assign a numerical index to this boat (in the order returned by list.files("VesselsData/"))

    # The function birdtovessel has added new columns to the bird track. We rename them to make them specific to boat i (bo), so that the same columns for next boats can be added (rather than overwrite previous ones)
    names(bi)=c(namesini,paste("Lag_",i,sep=""),paste("LocBat_",i,sep=""),paste("DistToVes_",i,sep=""),paste("HeadToVes_",i,sep=""),paste("DiffHead_",i,sep=""),paste("VesLocType_",i,sep=""))
  }
  beepr::beep(3)
  # write the outcome for each bird track in a file in the folder "BirdsToVessels"
  write.table(bi,file=paste("BirdsToVessels/AllBoats_",bird,sep=""),sep=";",row.names=F)
}














