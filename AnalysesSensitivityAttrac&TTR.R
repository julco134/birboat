
setwd("D:/Pro/These/Donnees/WAAL/BirdsToVessels/")

library(lme4)#,lib="D:/RLibrary")
library(MuMIn)



meta=read.table("Test/MetaWithSensitivityERs.csv",sep=";",h=T)
meta$Year=as.character(meta$Year)
meta$Sexe[meta$Sexe=='']=NA
meta$id=as.factor(c(1:dim(meta)[1]))

meta=meta[is.na(meta$avg_nbbat)==F & meta$avg_nbbat>0,]
meta=meta[is.na(meta$Sexe)==F &  is.na(meta$Age)==F & is.na(meta$Year)==F & is.na(meta$Bague)==F,]
meta$Sexe=as.factor(meta$Sexe)
meta$Year=as.factor(meta$Year)

meta$Agesq=(meta$Age-mean(meta$Age,na.rm=T))^2
meta$Agesq=meta$Agesq/sqrt(var(meta$Agesq,na.rm=T))


# Definitions of: attraction range, attendance behaviour, attendance zone
at=c(15,20,25,30)   # we define attraction area as a 30km circle aound boats
sitting.threshold=10/3.6   # we consider locations with speed<10km/h to be sitting on water
attend.threshold=3   # birds sitting within 3km of vessels will be considered attending boats

# Time to return threshold
ttr=c(2,4,8,24) #how many locs in between encounters to consider them distinct: 2 (30min), 2h, 6h, 24h

AnaER=list()
AnaPrAtt=list()
nE=c()
AnaPropEnc=list()
nEA=c()
i=0
### Analyses for each combination
for (timetoreturn in ttr){
  for (attrac.threshold in at){
    i=i+1
    #all encounters
    serdet=read.table(paste("Test/","DetectionSeries_",attrac.threshold,"km_",attend.threshold,"km_","TTR",timetoreturn,"locs.csv",sep=""),sep=";",h=T)
    
    time1=as.POSIXct(apply(cbind(as.character(serdet$startdate),as.character(serdet$starttime)),1,function(X){
      Date=unlist(strsplit(as.character(X[1]),"/"))
      Time=as.character(X[2])
      out=paste(paste(as.character(Date[3]),as.character(Date[2]),as.character(Date[1]), sep='-'),Time, sep=' ')
      return(out)
    }),tz='GMT')
    
    
    #removeencounters with no fly during daylight, or for which Age Sex or Bague is unknown
    serdet=serdet[serdet$dayflight==1,]
    serdet=serdet[is.na(serdet$Age)==F & is.na(serdet$Sex)==F & is.na(serdet$Bague)==F,]
    
    #Age squared
    serdet$Agesq=(serdet$Age-mean(meta$Age,na.rm=T))^2
    
    serdet$Year=as.factor(serdet$Year)
    serdet$Sex=as.factor(serdet$Sex)
    serdet$yeargrouped=ifelse(serdet$Year=="2013",1,0)
    
    
    serdet$Act=abs(serdet$lagvesact/60/60)
    
    ## encounters with attendance
    serdetcut=serdet[serdet$attend==1,]
    
    
    ### Analyses encounter rate
    erind=which(names(meta)==paste("ER",attrac.threshold,".",timetoreturn,sep=""))
    meta$nbserdetjour=meta$Duration*meta[,erind]
    mdet=glmer.nb(nbserdetjour~avg_nbbat+Sexe*(Age)+(1|Bague)+offset(log(nb_locjour)),data=meta)
    agesex=summary(mdet)$coefficients[dim(summary(mdet)$coefficient)[1],]
    coeff=summary(mdet)$coefficients
    if(agesex[4]>0.05){ # if interaction age*sex not significant, we extract estimates without interaction
      mdet=glmer.nb(nbserdetjour~avg_nbbat+Sexe+(Age)+(1|Bague)+offset(log(nb_locjour)),data=meta)
      coeff=summary(mdet)$coefficients
      coeff=rbind(coeff,agesex)
    }
    
    AnaER[[i]]=coeff
    
    
    ### Analyses proba attendance
    mint=glmer(attend~Act+(yeargrouped+Age+Sex)+avg_nbbatrangedet+(1|Bague)+(1|File),family="binomial",data=serdet)
    coeff=summary(mint)$coefficients
    mint=glmer(attend~Act+yeargrouped*Age+Sex+avg_nbbatrangedet+(1|Bague)+(1|File),family="binomial",data=serdet)
    coeff=rbind(coeff,summary(mint)$coefficients[7,])
    mint=glmer(attend~Act+yeargrouped+Age*Sex+avg_nbbatrangedet+(1|Bague)+(1|File),family="binomial",data=serdet)
    coeff=rbind(coeff,summary(mint)$coefficients[7,])
    mint=glmer(attend~Act+yeargrouped*Sex+Age+avg_nbbatrangedet+(1|Bague)+(1|File),family="binomial",data=serdet)
    coeff=rbind(coeff,summary(mint)$coefficients[7,])
    
    AnaPrAtt[[i]]=coeff
    nE=c(nE,dim(serdet)[1])
    
    
    ### Analyses prop encounter attending
    mpropintmi=glmer.nb(dur_attend~propact+(yeargrouped+Age+Sex)+avg_nbbatrangedet+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
    coeff=summary(mpropintmi)$coefficients
    mpropintmi=glmer.nb(dur_attend~propact+yeargrouped*Age+Sex+avg_nbbatrangedet+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
    coeff=rbind(coeff,summary(mpropintmi)$coefficients[7,])
    mpropintmi=glmer.nb(dur_attend~propact+yeargrouped+Age*Sex+avg_nbbatrangedet+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
    coeff=rbind(coeff,summary(mpropintmi)$coefficients[7,])
    mpropintmi=glmer.nb(dur_attend~propact+yeargrouped*Sex+Age+avg_nbbatrangedet+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
    coeff=rbind(coeff,summary(mpropintmi)$coefficients[7,])
    
    AnaPropEnc[[i]]=coeff
    nEA=c(nEA,dim(serdetcut)[1])
  }
  
}
# for ttr 96 problems introduced by accounting for avg nb bat range det
ttr=96
for (timetoreturn in ttr){
  for (attrac.threshold in at){
    i=i+1
    #all encounters
    serdet=read.table(paste("Test/","DetectionSeries_",attrac.threshold,"km_",attend.threshold,"km_","TTR",timetoreturn,"locs.csv",sep=""),sep=";",h=T)
    
    time1=as.POSIXct(apply(cbind(as.character(serdet$startdate),as.character(serdet$starttime)),1,function(X){
      Date=unlist(strsplit(as.character(X[1]),"/"))
      Time=as.character(X[2])
      out=paste(paste(as.character(Date[3]),as.character(Date[2]),as.character(Date[1]), sep='-'),Time, sep=' ')
      return(out)
    }),tz='GMT')
    
    
    #removeencounters with no fly during daylight, or for which Age Sex or Bague is unknown
    serdet=serdet[serdet$dayflight==1,]
    serdet=serdet[is.na(serdet$Age)==F & is.na(serdet$Sex)==F & is.na(serdet$Bague)==F,]
    
    #Age squared
    serdet$Agesq=(serdet$Age-mean(meta$Age,na.rm=T))^2
    
    serdet$Year=as.factor(serdet$Year)
    serdet$Sex=as.factor(serdet$Sex)
    serdet$yeargrouped=ifelse(serdet$Year=="2013",1,0)
    
    
    serdet$Act=abs(serdet$lagvesact/60/60)
    
    ## encounters with attendance
    serdetcut=serdet[serdet$attend==1,]
    
    
    ### Analyses encounter rate
    erind=which(names(meta)==paste("ER",attrac.threshold,".",timetoreturn,sep=""))
    meta$nbserdetjour=meta$Duration*meta[,erind]
    mdet=glmer.nb(nbserdetjour~avg_nbbat+Sexe*(Age)+(1|Bague)+offset(log(nb_locjour)),data=meta)
    agesex=summary(mdet)$coefficients[dim(summary(mdet)$coefficient)[1],]
    coeff=summary(mdet)$coefficients
    if(agesex[4]>0.05){ # if interaction age*sex not significant, we extract estimates without interaction
      mdet=glmer.nb(nbserdetjour~avg_nbbat+Sexe+(Age)+(1|Bague)+offset(log(nb_locjour)),data=meta)
      coeff=summary(mdet)$coefficients
      coeff=rbind(coeff,agesex)
    }
    
    AnaER[[i]]=coeff
    
    
    ### Analyses proba attendance
    mint=glmer(attend~Act+(yeargrouped+Age+Sex)+(1|Bague)+(1|File),family="binomial",data=serdet)
    coeff=summary(mint)$coefficients
    mint=glmer(attend~Act+yeargrouped*Age+Sex+(1|Bague)+(1|File),family="binomial",data=serdet)
    coeff=rbind(coeff,summary(mint)$coefficients[6,])
    mint=glmer(attend~Act+yeargrouped+Age*Sex+(1|Bague)+(1|File),family="binomial",data=serdet)
    coeff=rbind(coeff,summary(mint)$coefficients[6,])
    mint=glmer(attend~Act+yeargrouped*Sex+Age+(1|Bague)+(1|File),family="binomial",data=serdet)
    coeff=rbind(coeff,summary(mint)$coefficients[6,])
    
    AnaPrAtt[[i]]=coeff
    nE=c(nE,dim(serdet)[1])
    
    
    ### Analyses prop encounter attending
    mpropintmi=glmer.nb(dur_attend~propact+(yeargrouped+Age+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
    coeff=summary(mpropintmi)$coefficients
    mpropintmi=glmer.nb(dur_attend~propact+yeargrouped*Age+Sex+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
    coeff=rbind(coeff,summary(mpropintmi)$coefficients[6,])
    mpropintmi=glmer.nb(dur_attend~propact+yeargrouped+Age*Sex+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
    coeff=rbind(coeff,summary(mpropintmi)$coefficients[6,])
    mpropintmi=glmer.nb(dur_attend~propact+yeargrouped*Sex+Age+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
    coeff=rbind(coeff,summary(mpropintmi)$coefficients[6,])
    
    AnaPropEnc[[i]]=coeff
    nEA=c(nEA,dim(serdetcut)[1])
  }
}


seuil=rep(c(15,20,25,30),5)
away=c(rep(2,4),rep(4,4),rep(8,4),rep(24,4),rep(96,4))/4


### N
par(mfrow=c(1,2))
plot(nE~seuil,xlab="Attraction distance value (km)",ylab="Total number of encounters",cex.lab=1.5,cex.axis=1.5,pch=21,bg="grey",cex=1.2)
points(nE[8]~seuil[8],pch=21,bg="red",cex=1.35)
plot(nE~away,xlab="Time to return threshold (h)",,ylab="Total number of encounters",cex.lab=1.5,cex.axis=1.5,pch=21,bg="grey",cex=1.2)
points(nE[8]~away[8],pch=21,bg="red",cex=1.35)

### Proba attraction
matrix(nEA,4,5)/matrix(nE,4,5)

par(mfrow=c(1,2))
plot(nEA/nE~seuil,ylim=c(0,1),xlab="Attraction distance value (km)",ylab="Proportion of encounters with attendance",cex.lab=1.5,cex.axis=1.5,pch=21,bg="grey",cex=1.2)
points(nEA[8]/nE[8]~seuil[8],pch=21,bg="red",cex=1.35)
plot(nEA/nE~away,ylim=c(0,1),xlab="Time to return threshold (h)",ylab="Proportion of encounters with attendance",cex.lab=1.5,cex.axis=1.5,pch=21,bg="grey",cex=1.2)
points(nEA[8]/nE[8]~away[8],pch=21,bg="red",cex=1.35)


