##### Functions to establish distance from bird to vessel
### Here we assume there are many boats in the area
### and so you can not afford the "one bird - one boat" level of analysis
### Instead we simply extract for each bird location, the minimum distance to boats
### among all the boats for which a "simutaneous" (< dt) position is known
### it won't allow in depth examination of the bird behaviour towards one boat
### but it certainly tells whether your birds closely overlapped with boats

library(geosphere)
library(plyr)

#### function to keep all boat locs within dt
is.simultaneous=function(loctime,tracktime,dt){
  lagcarre=as.double(difftime(loctime,tracktime,units="secs"))^2
  lags=sqrt(lagcarre[lagcarre<(dt)^2])
  loctrack=which(lagcarre<(dt)^2)
  return(data.frame("lags"=lags,"loctrack"=loctrack))
}

#### function to extract along a track the minimum distance to "simultaneous" boat positions
### require(geosphere)
#bird = data.frame with at least columns Latitude, Longitude, (Date, Time), ((Loc)); with Date in the format dd/mm/yyyy and time hh:mm(:ss); Loc is simply a running index for all locs (from 1 to the number of GPS records of the track)
#vessel=data frame for vessel VMS with columns Latitude, Longitude, Date, Time (and, optional: TypeLoc for differentiating between real and interpolated points, and/or vessel fishing activity)
#dt= maximal time lag (in seconds) allowed for bird and vessel locations to be considered "simultaneous"
# optional: if Date and Time columns are not in the right format, you can pass on TIMEPOSIXct objects to birdtime and birdvessel instead

birdminvesseldist=function(bird,vessel,dt,birdtime=NULL,vesseltime=NULL){

  #convert boat date and time columns to POSIXct format unless specified in function arguments
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

  # this is assigning an index to bird locations, used below
  bird$Loc=c(1:dim(bird)[1])
  
  # we extract for each bird location the boat location with minimum time lag
  listlag=lapply(as.list(birdtime),minlag,vesseltime)
  lags=unlist(lapply(listlag,function(X){X[1]})) # contains lag values
  locbats=unlist(lapply(listlag,function(X){X[2]})) # contains indices for corresponding boat locations

  # we extract for each bird location, all the boat locations less than dt
  # then we calculate distance to each
  # and we return the minimum distance among those "simultaneous" boat positions
  bird$birdtime=birdtime
  mindist=plyr::daply(bird,~Loc,function(X){
#   for(loc in 1:dim(bird)[1]){
#     X=bird[loc,]
    simultloc=is.simultaneous(X$birdtime,vesseltime,dt)
    if(length(simultloc$loctrack)>0){
      veslon=vessel$Longitude[simultloc$loctrack]
      veslat=vessel$Latitude[simultloc$loctrack]
      dist=distCosine(c(X$Longitude,X$Latitude),cbind(veslon,veslat))/1000 # km
      mindist=min(dist)
      locmindist=simultloc$loctrack[which.min(dist)]
      lagmindist=simultloc$lags[which.min(dist)]
    }
    if(length(simultloc$loctrack)==0){
      mindist=NA
      locmindidst=NA
      lagmindist=NA
    }
    
#     bird$MinDist_km[loc]=mindist
#     bird$LocVes[loc]=locmindist
#     bird$Lag_sec[loc]=lagmindist
      return(c("mindist"=mindist,"locmindist"=locmindist,"lagmindist"=lagmindist))
  })
  
  bird$MinDist_km=mindist[,1]
  bird$LocVes=mindist[,2]
  bird$Lag_sec=mindist[,3]
  
  # The outcome is the bird track with added columns
  return(bird)
}




