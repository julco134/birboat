
setwd('D:/Pro/WA-Fisheries_AnalysesData2011-12-13/')





#### Call prep for analyses or
meta=read.table("MetaDataForAnalysisInCrozet.csv",sep=";",h=T)

serdet=read.table("metaserForAnalysisInCrozet.csv",sep=";",h=T)
serdet=serdet[serdet$voljn==1,]

serdetcut=read.table("metasercutintForAnalysisInCrozet.csv",sep=";",h=T)
serdetcut=serdetcut[serdetcut$voljn==1,]



meta$Year=as.character(meta$Year)
meta$Sexe[meta$Sexe=='']=NA
meta$id=as.factor(c(1:dim(meta)[1]))

meta=meta[is.na(meta$avg_nbbat)==F & meta$avg_nbbat>0,]
meta=meta[is.na(meta$Sexe)==F &  is.na(meta$Age)==F & is.na(meta$Year)==F & is.na(meta$Bague)==F,]
meta$Sexe=as.factor(meta$Sexe)
meta$Year=as.factor(meta$Year)

meta$Agesq=(meta$Age-mean(meta$Age,na.rm=T))^2
meta$Agesq=meta$Agesq/sqrt(var(meta$Agesq,na.rm=T))


serdet=serdet[is.na(serdet$Age)==F & is.na(serdet$Sex)==F & is.na(serdet$Bague)==F,]
serdet$Agesq=(serdet$Age-mean(meta$Age,na.rm=T))^2

serdetcut$Agesq=(serdetcut$Age-mean(meta$Age,na.rm=T))^2




library(lme4)#,lib="D:/RLibrary")
library(MuMIn)



## Create overview table
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# meta as in prep for analyses

tot=c()
nbt=dim(meta)[1]
nbi=length(unique(meta$Bague))
tot=c(tot,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(meta$Pers),2)
#std=round(stderr(meta$Pers),2)
#tot=c(tot,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(meta$MaxRange),0)
std=round(stderr(meta$MaxRange),0)
tot=c(tot,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(meta$TotDist),0)
std=round(stderr(meta$TotDist),0)
tot=c(tot,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(meta$Duration),1)
std=round(stderr(meta$Duration),1)
tot=c(tot,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(meta$prop_cro)*100,2)
std=round(stderr(meta$prop_cro)*100,2)
tot=c(tot,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(meta$avg_nbbat),2)
std=round(stderr(meta$avg_nbbat),2)
tot=c(tot,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(meta$nb_serdetjour)==F & meta$nb_serdetjour>0)
nbi=length(unique(meta$Bague[is.na(meta$nb_serdetjour)==F & meta$nb_serdetjour>0]))
tot=c(tot,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(meta$nb_serdetjour[is.na(meta$nb_serdetjour)==F & meta$nb_serdetjour>0]),2)
std=round(stderr(meta$nb_serdetjour[is.na(meta$nb_serdetjour)==F & meta$nb_serdetjour>0]),2)
tot=c(tot,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(meta$nb_int>0 & meta$nb_serdetjour>0)
nbi=length(unique(meta$Bague[meta$nb_int>0 & meta$nb_serdetjour>0]))
tot=c(tot,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(meta$nb_int[meta$nb_int>0 & meta$nb_serdetjour>0]/4),2)
std=round(stderr(meta$nb_int[meta$nb_int>0 & meta$nb_serdetjour>0]/4),2)
tot=c(tot,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(meta$propint[meta$nb_int>0 & meta$nb_serdetjour>0])*100,2)
std=round(stderr(meta$propint[meta$nb_int>0])*100,2)
tot=c(tot,paste(as.character(m),'+/-',as.character(std),sep=''))


metaold=meta[meta$Age>25,]

old=c()
nbt=dim(metaold)[1]
nbi=length(unique(metaold$Bague))
old=c(old,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metaold$Pers),2)
#std=round(stderr(metaold$Pers),2)
#old=c(old,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaold$MaxRange),0)
std=round(stderr(metaold$MaxRange),0)
old=c(old,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaold$TotDist),0)
std=round(stderr(metaold$TotDist),0)
old=c(old,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaold$Duration),1)
std=round(stderr(metaold$Duration),1)
old=c(old,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaold$prop_cro)*100,2)
std=round(stderr(metaold$prop_cro)*100,2)
old=c(old,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaold$avg_nbbat),2)
std=round(stderr(metaold$avg_nbbat),2)
old=c(old,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metaold$nb_serdetjour)==F & metaold$nb_serdetjour>0)
nbi=length(unique(metaold$Bague[is.na(metaold$nb_serdetjour)==F & metaold$nb_serdetjour>0]))
old=c(old,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metaold$nb_serdetjour[is.na(metaold$nb_serdetjour)==F & metaold$nb_serdetjour>0]),2)
std=round(stderr(metaold$nb_serdetjour[is.na(metaold$nb_serdetjour)==F & metaold$nb_serdetjour>0]),2)
old=c(old,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metaold$nb_int>0)
nbi=length(unique(metaold$Bague[metaold$nb_int>0]))
old=c(old,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metaold$nb_int[metaold$nb_int>0]/4),2)
std=round(stderr(metaold$nb_int[metaold$nb_int>0]/4),2)
old=c(old,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaold$propint[metaold$nb_int>0])*100,2)
std=round(stderr(metaold$propint[metaold$nb_int>0])*100,2)
old=c(old,paste(as.character(m),'+/-',as.character(std),sep=''))


metayoung=meta[meta$Age<=25,]

young=c()
nbt=dim(metayoung)[1]
nbi=length(unique(metayoung$Bague))
young=c(young,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metayoung$Pers),2)
#std=round(stderr(metayoung$Pers),2)
#young=c(young,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metayoung$MaxRange),0)
std=round(stderr(metayoung$MaxRange),0)
young=c(young,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metayoung$TotDist),0)
std=round(stderr(metayoung$TotDist),0)
young=c(young,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metayoung$Duration),1)
std=round(stderr(metayoung$Duration),1)
young=c(young,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metayoung$prop_cro)*100,2)
std=round(stderr(metayoung$prop_cro)*100,2)
young=c(young,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metayoung$avg_nbbat),2)
std=round(stderr(metayoung$avg_nbbat),2)
young=c(young,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metayoung$nb_serdetjour)==F & metayoung$nb_serdetjour>0)
nbi=length(unique(metayoung$Bague[is.na(metayoung$nb_serdetjour)==F & metayoung$nb_serdetjour>0]))
young=c(young,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metayoung$nb_serdetjour[is.na(metayoung$nb_serdetjour)==F & metayoung$nb_serdetjour>0]),2)
std=round(stderr(metayoung$nb_serdetjour[is.na(metayoung$nb_serdetjour)==F & metayoung$nb_serdetjour>0]),2)
young=c(young,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metayoung$nb_int>0)
nbi=length(unique(metayoung$Bague[metayoung$nb_int>0]))
young=c(young,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metayoung$nb_int[metayoung$nb_int>0]/4),2)
std=round(stderr(metayoung$nb_int[metayoung$nb_int>0]/4),2)
young=c(young,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metayoung$propint[metayoung$nb_int>0])*100,2)
std=round(stderr(metayoung$propint[metayoung$nb_int>0])*100,2)
young=c(young,paste(as.character(m),'+/-',as.character(std),sep=''))



metamale=meta[meta$Sexe=='M',]

male=c()
nbt=dim(metamale)[1]
nbi=length(unique(metamale$Bague))
male=c(male,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metamale$Pers),2)
#std=round(stderr(metamale$Pers),2)
#male=c(male,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamale$MaxRange),0)
std=round(stderr(metamale$MaxRange),0)
male=c(male,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamale$TotDist),0)
std=round(stderr(metamale$TotDist),0)
male=c(male,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamale$Duration),1)
std=round(stderr(metamale$Duration),1)
male=c(male,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamale$prop_cro)*100,2)
std=round(stderr(metamale$prop_cro)*100,2)
male=c(male,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamale$avg_nbbat),2)
std=round(stderr(metamale$avg_nbbat),2)
male=c(male,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metamale$nb_serdetjour)==F & metamale$nb_serdetjour>0)
nbi=length(unique(metamale$Bague[is.na(metamale$nb_serdetjour)==F & metamale$nb_serdetjour>0]))
male=c(male,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metamale$nb_serdetjour[is.na(metamale$nb_serdetjour)==F & metamale$nb_serdetjour>0]),2)
std=round(stderr(metamale$nb_serdetjour[is.na(metamale$nb_serdetjour)==F & metamale$nb_serdetjour>0]),2)
male=c(male,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metamale$nb_int>0)
nbi=length(unique(metamale$Bague[metamale$nb_int>0]))
male=c(male,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metamale$nb_int[metamale$nb_int>0]/4),2)
std=round(stderr(metamale$nb_int[metamale$nb_int>0]/4),2)
male=c(male,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamale$propint[metamale$nb_int>0])*100,2)
std=round(stderr(metamale$propint[metamale$nb_int>0])*100,2)
male=c(male,paste(as.character(m),'+/-',as.character(std),sep=''))


metafemale=meta[meta$Sexe=='F',]

female=c()
nbt=dim(metafemale)[1]
nbi=length(unique(metafemale$Bague))
female=c(female,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metafemale$Pers),2)
#std=round(stderr(metafemale$Pers),2)
#female=c(female,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafemale$MaxRange),0)
std=round(stderr(metafemale$MaxRange),0)
female=c(female,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafemale$TotDist),0)
std=round(stderr(metafemale$TotDist),0)
female=c(female,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafemale$Duration),1)
std=round(stderr(metafemale$Duration),1)
female=c(female,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafemale$prop_cro)*100,2)
std=round(stderr(metafemale$prop_cro)*100,2)
female=c(female,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafemale$avg_nbbat),2)
std=round(stderr(metafemale$avg_nbbat),2)
female=c(female,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metafemale$nb_serdetjour)==F & metafemale$nb_serdetjour>0)
nbi=length(unique(metafemale$Bague[is.na(metafemale$nb_serdetjour)==F & metafemale$nb_serdetjour>0]))
female=c(female,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metafemale$nb_serdetjour[is.na(metafemale$nb_serdetjour)==F & metafemale$nb_serdetjour>0]),2)
std=round(stderr(metafemale$nb_serdetjour[is.na(metafemale$nb_serdetjour)==F & metafemale$nb_serdetjour>0]),2)
female=c(female,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metafemale$nb_int>0)
nbi=length(unique(metafemale$Bague[metafemale$nb_int>0]))
female=c(female,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metafemale$nb_int[metafemale$nb_int>0]/4),2)
std=round(stderr(metafemale$nb_int[metafemale$nb_int>0]/4),2)
female=c(female,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafemale$propint[metafemale$nb_int>0])*100,2)
std=round(stderr(metafemale$propint[metafemale$nb_int>0])*100,2)
female=c(female,paste(as.character(m),'+/-',as.character(std),sep=''))


metaan1=meta[meta$Year==2011,]

an1=c()
nbt=dim(metaan1)[1]
nbi=length(unique(metaan1$Bague))
an1=c(an1,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metaan1$Pers),2)
#std=round(stderr(metaan1$Pers),2)
#an1=c(an1,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan1$MaxRange),0)
std=round(stderr(metaan1$MaxRange),0)
an1=c(an1,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan1$TotDist),0)
std=round(stderr(metaan1$TotDist),0)
an1=c(an1,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan1$Duration),1)
std=round(stderr(metaan1$Duration),1)
an1=c(an1,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan1$prop_cro)*100,2)
std=round(stderr(metaan1$prop_cro)*100,2)
an1=c(an1,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan1$avg_nbbat),2)
std=round(stderr(metaan1$avg_nbbat),2)
an1=c(an1,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metaan1$nb_serdetjour)==F & metaan1$nb_serdetjour>0)
nbi=length(unique(metaan1$Bague[is.na(metaan1$nb_serdetjour)==F & metaan1$nb_serdetjour>0]))
an1=c(an1,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metaan1$nb_serdetjour[is.na(metaan1$nb_serdetjour)==F & metaan1$nb_serdetjour>0]),2)
std=round(stderr(metaan1$nb_serdetjour[is.na(metaan1$nb_serdetjour)==F & metaan1$nb_serdetjour>0]),2)
an1=c(an1,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metaan1$nb_int>0)
nbi=length(unique(metaan1$Bague[metaan1$nb_int>0]))
an1=c(an1,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metaan1$nb_int[metaan1$nb_int>0]/4),2)
std=round(stderr(metaan1$nb_int[metaan1$nb_int>0]/4),2)
an1=c(an1,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan1$propint[metaan1$nb_int>0])*100,2)
std=round(stderr(metaan1$propint[metaan1$nb_int>0])*100,2)
an1=c(an1,paste(as.character(m),'+/-',as.character(std),sep=''))


metaan2=meta[meta$Year==2012,]

an2=c()
nbt=dim(metaan2)[1]
nbi=length(unique(metaan2$Bague))
an2=c(an2,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metaan2$Pers),2)
#std=round(stderr(metaan2$Pers),2)
#an2=c(an2,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan2$MaxRange),0)
std=round(stderr(metaan2$MaxRange),0)
an2=c(an2,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan2$TotDist),0)
std=round(stderr(metaan2$TotDist),0)
an2=c(an2,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan2$Duration),1)
std=round(stderr(metaan2$Duration),1)
an2=c(an2,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan2$prop_cro)*100,2)
std=round(stderr(metaan2$prop_cro)*100,2)
an2=c(an2,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan2$avg_nbbat),2)
std=round(stderr(metaan2$avg_nbbat),2)
an2=c(an2,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metaan2$nb_serdetjour)==F & metaan2$nb_serdetjour>0)
nbi=length(unique(metaan2$Bague[is.na(metaan2$nb_serdetjour)==F & metaan2$nb_serdetjour>0]))
an2=c(an2,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metaan2$nb_serdetjour[is.na(metaan2$nb_serdetjour)==F & metaan2$nb_serdetjour>0]),2)
std=round(stderr(metaan2$nb_serdetjour[is.na(metaan2$nb_serdetjour)==F & metaan2$nb_serdetjour>0]),2)
an2=c(an2,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metaan2$nb_int>0)
nbi=length(unique(metaan2$Bague[metaan2$nb_int>0]))
an2=c(an2,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metaan2$nb_int[metaan2$nb_int>0]/4),2)
std=round(stderr(metaan2$nb_int[metaan2$nb_int>0]/4),2)
an2=c(an2,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan2$propint[metaan2$nb_int>0])*100,2)
std=round(stderr(metaan2$propint[metaan2$nb_int>0])*100,2)
an2=c(an2,paste(as.character(m),'+/-',as.character(std),sep=''))


metaan3=meta[meta$Year==2013,]

an3=c()
nbt=dim(metaan3)[1]
nbi=length(unique(metaan3$Bague))
an3=c(an3,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metaan3$Pers),2)
#std=round(stderr(metaan3$Pers),2)
#an3=c(an3,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan3$MaxRange),0)
std=round(stderr(metaan3$MaxRange),0)
an3=c(an3,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan3$TotDist),0)
std=round(stderr(metaan3$TotDist),0)
an3=c(an3,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan3$Duration),1)
std=round(stderr(metaan3$Duration),1)
an3=c(an3,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan3$prop_cro)*100,2)
std=round(stderr(metaan3$prop_cro)*100,2)
an3=c(an3,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan3$avg_nbbat),2)
std=round(stderr(metaan3$avg_nbbat),2)
an3=c(an3,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metaan3$nb_serdetjour)==F & metaan3$nb_serdetjour>0)
nbi=length(unique(metaan3$Bague[is.na(metaan3$nb_serdetjour)==F & metaan3$nb_serdetjour>0]))
an3=c(an3,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metaan3$nb_serdetjour[is.na(metaan3$nb_serdetjour)==F & metaan3$nb_serdetjour>0]),2)
std=round(stderr(metaan3$nb_serdetjour[is.na(metaan3$nb_serdetjour)==F & metaan3$nb_serdetjour>0]),2)
an3=c(an3,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metaan3$nb_int>0)
nbi=length(unique(metaan3$Bague[metaan3$nb_int>0]))
an3=c(an3,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metaan3$nb_int[metaan3$nb_int>0]/4),2)
std=round(stderr(metaan3$nb_int[metaan3$nb_int>0]/4),2)
an3=c(an3,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metaan3$propint[metaan3$nb_int>0])*100,2)
std=round(stderr(metaan3$propint[metaan3$nb_int>0])*100,2)
an3=c(an3,paste(as.character(m),'+/-',as.character(std),sep=''))


metamo=meta[meta$Age>25 & meta$Sexe=="M",]

mo=c()
nbt=dim(metamo)[1]
nbi=length(unique(metamo$Bague))
mo=c(mo,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metamo$Pers),2)
#std=round(stderr(metamo$Pers),2)
#mo=c(mo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamo$MaxRange),0)
std=round(stderr(metamo$MaxRange),0)
mo=c(mo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamo$TotDist),0)
std=round(stderr(metamo$TotDist),0)
mo=c(mo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamo$Duration),1)
std=round(stderr(metamo$Duration),1)
mo=c(mo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamo$prop_cro)*100,2)
std=round(stderr(metamo$prop_cro)*100,2)
mo=c(mo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamo$avg_nbbat),2)
std=round(stderr(metamo$avg_nbbat),2)
mo=c(mo,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metamo$nb_serdetjour)==F & metamo$nb_serdetjour>0)
nbi=length(unique(metamo$Bague[is.na(metamo$nb_serdetjour)==F & metamo$nb_serdetjour>0]))
mo=c(mo,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metamo$nb_serdetjour[is.na(metamo$nb_serdetjour)==F & metamo$nb_serdetjour>0]),2)
std=round(stderr(metamo$nb_serdetjour[is.na(metamo$nb_serdetjour)==F & metamo$nb_serdetjour>0]),2)
mo=c(mo,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metamo$nb_int>0)
nbi=length(unique(metamo$Bague[metamo$nb_int>0]))
mo=c(mo,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metamo$nb_int[metamo$nb_int>0]/4),2)
std=round(stderr(metamo$nb_int[metamo$nb_int>0]/4),2)
mo=c(mo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamo$propint[metamo$nb_int>0])*100,2)
std=round(stderr(metamo$propint[metamo$nb_int>0])*100,2)
mo=c(mo,paste(as.character(m),'+/-',as.character(std),sep=''))


metamy=meta[meta$Age<=25 & meta$Sexe=="M",]

my=c()
nbt=dim(metamy)[1]
nbi=length(unique(metamy$Bague))
my=c(my,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metamy$Pers),2)
#std=round(stderr(metamy$Pers),2)
#my=c(my,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamy$MaxRange),0)
std=round(stderr(metamy$MaxRange),0)
my=c(my,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamy$TotDist),0)
std=round(stderr(metamy$TotDist),0)
my=c(my,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamy$Duration),1)
std=round(stderr(metamy$Duration),1)
my=c(my,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamy$prop_cro)*100,2)
std=round(stderr(metamy$prop_cro)*100,2)
my=c(my,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamy$avg_nbbat),2)
std=round(stderr(metamy$avg_nbbat),2)
my=c(my,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metamy$nb_serdetjour)==F & metamy$nb_serdetjour>0)
nbi=length(unique(metamy$Bague[is.na(metamy$nb_serdetjour)==F & metamy$nb_serdetjour>0]))
my=c(my,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metamy$nb_serdetjour[is.na(metamy$nb_serdetjour)==F & metamy$nb_serdetjour>0]),2)
std=round(stderr(metamy$nb_serdetjour[is.na(metamy$nb_serdetjour)==F & metamy$nb_serdetjour>0]),2)
my=c(my,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metamy$nb_int>0)
nbi=length(unique(metamy$Bague[metamy$nb_int>0]))
my=c(my,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metamy$nb_int[metamy$nb_int>0]/4),2)
std=round(stderr(metamy$nb_int[metamy$nb_int>0]/4),2)
my=c(my,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metamy$propint[metamy$nb_int>0])*100,2)
std=round(stderr(metamy$propint[metamy$nb_int>0])*100,2)
my=c(my,paste(as.character(m),'+/-',as.character(std),sep=''))


metafo=meta[meta$Age>25 & meta$Sexe=="F",]

fo=c()
nbt=dim(metafo)[1]
nbi=length(unique(metafo$Bague))
fo=c(fo,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metafo$Pers),2)
#std=round(stderr(metafo$Pers),2)
#fo=c(fo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafo$MaxRange),0)
std=round(stderr(metafo$MaxRange),0)
fo=c(fo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafo$TotDist),0)
std=round(stderr(metafo$TotDist),0)
fo=c(fo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafo$Duration),1)
std=round(stderr(metafo$Duration),1)
fo=c(fo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafo$prop_cro)*100,2)
std=round(stderr(metafo$prop_cro)*100,2)
fo=c(fo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafo$avg_nbbat),2)
std=round(stderr(metafo$avg_nbbat),2)
fo=c(fo,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metafo$nb_serdetjour)==F & metafo$nb_serdetjour>0)
nbi=length(unique(metafo$Bague[is.na(metafo$nb_serdetjour)==F & metafo$nb_serdetjour>0]))
fo=c(fo,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metafo$nb_serdetjour[is.na(metafo$nb_serdetjour)==F & metafo$nb_serdetjour>0]),2)
std=round(stderr(metafo$nb_serdetjour[is.na(metafo$nb_serdetjour)==F & metafo$nb_serdetjour>0]),2)
fo=c(fo,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metafo$nb_int>0)
nbi=length(unique(metafo$Bague[metafo$nb_int>0]))
fo=c(fo,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metafo$nb_int[metafo$nb_int>0]/4),2)
std=round(stderr(metafo$nb_int[metafo$nb_int>0]/4),2)
fo=c(fo,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafo$propint[metafo$nb_int>0])*100,2)
std=round(stderr(metafo$propint[metafo$nb_int>0])*100,2)
fo=c(fo,paste(as.character(m),'+/-',as.character(std),sep=''))


metafy=meta[meta$Age<=25 & meta$Sexe=="F",]

fy=c()
nbt=dim(metafy)[1]
nbi=length(unique(metafy$Bague))
fy=c(fy,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
#m=round(mean(metafy$Pers),2)
#std=round(stderr(metafy$Pers),2)
#fy=c(fy,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafy$MaxRange),0)
std=round(stderr(metafy$MaxRange),0)
fy=c(fy,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafy$TotDist),0)
std=round(stderr(metafy$TotDist),0)
fy=c(fy,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafy$Duration),1)
std=round(stderr(metafy$Duration),1)
fy=c(fy,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafy$prop_cro)*100,2)
std=round(stderr(metafy$prop_cro)*100,2)
fy=c(fy,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafy$avg_nbbat),2)
std=round(stderr(metafy$avg_nbbat),2)
fy=c(fy,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(is.na(metafy$nb_serdetjour)==F & metafy$nb_serdetjour>0)
nbi=length(unique(metafy$Bague[is.na(metafy$nb_serdetjour)==F & metafy$nb_serdetjour>0]))
fy=c(fy,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metafy$nb_serdetjour[is.na(metafy$nb_serdetjour)==F & metafy$nb_serdetjour>0]),2)
std=round(stderr(metafy$nb_serdetjour[is.na(metafy$nb_serdetjour)==F & metafy$nb_serdetjour>0]),2)
fy=c(fy,paste(as.character(m),'+/-',as.character(std),sep=''))
nbt=sum(metafy$nb_int>0)
nbi=length(unique(metafy$Bague[metafy$nb_int>0]))
fy=c(fy,paste(as.character(nbt),' (',as.character(nbi),')',sep=''))
m=round(mean(metafy$nb_int[metafy$nb_int>0]/4),2)
std=round(stderr(metafy$nb_int[metafy$nb_int>0]/4),2)
fy=c(fy,paste(as.character(m),'+/-',as.character(std),sep=''))
m=round(mean(metafy$propint[metafy$nb_int>0])*100,2)
std=round(stderr(metafy$propint[metafy$nb_int>0])*100,2)
fy=c(fy,paste(as.character(m),'+/-',as.character(std),sep=''))


#write.table(cbind(tot,young,old,male,female,mo,my,fo,fy,an1,an2,an3),file='Results/DataStructureOld25.csv',sep=';',row.names=F)







library(lme4)
# encounter rate
meta$yeargrouped=ifelse(meta$Year=="2013","2013",0)
mdet=glmer(nb_serdetjour~avg_nbbat+Sexe+Age+Agesq+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
mdetint1=glmer(nb_serdetjour~avg_nbbat+Sexe+Age+Agesq+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
mdetint2=glmer(nb_serdetjour~avg_nbbat+Sexe+Age+Agesq+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
mdetint3=glmer(nb_serdetjour~avg_nbbat+Sexe+Age+Agesq+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)

# Wald Z tests are fine if no overdispersion, otherwise, wald t tests
# Overdisersion => instead, use neg bin
mdet=glmer.nb(nb_serdetjour~avg_nbbat+Sexe*(Age)+(1|Bague)+offset(log(nb_locjour)),data=meta)


mdet=glmer(nb_serdetjour~avg_nbbat+Sexe*(Age+Agesq)+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
mdetmi=glmer(nb_serdetjour~avg_nbbat+Sexe+Age+Agesq+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
mdetms=glmer(nb_serdetjour~avg_nbbat+Age+Agesq+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
mdetma=glmer(nb_serdetjour~avg_nbbat+Sexe+Agesq+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
mdetmb=glmer(nb_serdetjour~Sexe*(Age+Agesq)+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
mdetmib=glmer(nb_serdetjour~Sexe+Age+Agesq+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
mdetmas=glmer(nb_serdetjour~avg_nbbat+Sexe+Age+(1|Bague)+offset(log(nb_locjour)),family='poisson',data=meta)
anova(mdetmi,mdet)
anova(mdetmi,mdetms)
anova(mdetmi,mdetma)
anova(mdetmi,mdetmib)
anova(mdetmi,mdetmas)
#remark: do not change if we look at rate considering only track portions over crozet shelf:
meta$loccrojour=meta$prop_cro*meta$nb_locjour
mdetbis=glmer(nb_serdetjour~avg_nbbat+Sexe+Age+(1|Bague)+offset(log(loccrojour)),family='poisson',data=meta)


# proba to interact or not

## Wald Z tests are fine, Laplace/GHQ estimation

serdet$Year=as.factor(serdet$Year)
serdet$Sex=as.factor(serdet$Sex)

#mint=glmer(int~before1stleavecro+Act+(Year+Age+Sex)^2+(1|Bague)+(1|File),family="binomial",data=serdet)
mint=glmer(int~Act+(Year+Age+Sex)^2+(1|Bague)+(1|File),family="binomial",data=serdet)
mintmaxs=glmer(int~Act+(Year+Age+Sex)^2-Age:Sex+(1|Bague)+(1|File),family="binomial",data=serdet,nAGQ=0)
mintmaxy=glmer(int~Act+(Year+Age+Sex)^2-Age:Year+(1|Bague)+(1|File),family="binomial",data=serdet)
mintmsxy=glmer(int~Act+(Year+Age+Sex)^2-Sex:Year+(1|Bague)+(1|File),family="binomial",data=serdet,nAGQ=0)
anova(mint,mintmaxs)
anova(mint,mintmaxy)
anova(mint,mintmsxy)
mintmi=glmer(int~Act+Year+Age+Sex+(1|Bague)+(1|File),family="binomial",data=serdet,nAGQ=0)
mintmis=glmer(int~Act+Year*Age+Sex+(1|Bague)+(1|File),family="binomial",data=serdet,nAGQ=0)
mintmia=glmer(int~Act+Year*Sex+Age+(1|Bague)+(1|File),family="binomial",data=serdet,nAGQ=0)
mintmiy=glmer(int~Act+Year+Age*Sex+(1|Bague)+(1|File),family="binomial",data=serdet,nAGQ=0)
anova(mintmi,mintmia)
anova(mintmi,mintmis)
anova(mintmi,mintmiy)
anova(mintmaxs,mintmi)
anova(mintmaxy,mintmi)
anova(mintmsxy,mintmi)

mintmiact=glmer(int~Year+Age+Sex+(1|Bague)+(1|File),family="binomial",data=serdet)
mintmiyear=glmer(int~Act+Age+Sex+(1|Bague)+(1|File),family="binomial",data=serdet)
mintmiage=glmer(int~Act+Year+Sex+(1|Bague)+(1|File),family="binomial",data=serdet)
mintmisex=glmer(int~Act+Year+Age+(1|Bague)+(1|File),family="binomial",data=serdet)
anova(mintmi,mintmiact)
anova(mintmi,mintmiage)
anova(mintmi,mintmiyear)
anova(mintmi,mintmisex)

# grouping 2011 and 2012 together
serdet$yeargrouped=ifelse(serdet$Year=="2013",1,0)

mintmi=glmer(int~Act+yeargrouped+Age+Sex+(1|Bague)+(1|File),family="binomial",data=serdet,nAGQ=0)
mintmis=glmer(int~Act+yeargrouped*Age+Sex+(1|Bague)+(1|File),family="binomial",data=serdet)
mintmia=glmer(int~Act+yeargrouped*Sex+Age+(1|Bague)+(1|File),family="binomial",data=serdet)
mintmiy=glmer(int~Act+yeargrouped+Age*Sex+(1|Bague)+(1|File),family="binomial",data=serdet)
anova(mintmi,mintmia)
anova(mintmi,mintmis)
anova(mintmi,mintmiy)
anova(mintmi,mintmiyear)






# efforts spent interacting

## USe Laplace/GHQ estimation. Wald Z if no overdispersion, Wald t otherwise

serdetcut$Year=as.factor(serdetcut$Year)
serdetcut$Sex=as.factor(serdetcut$Sex)
serdetcut$yeargrouped=ifelse(serdetcut$Year=="2013",1,0)
serdetcut=serdetcut[is.na(serdetcut$Age)==F,]

#mnbint=glmer(nb_int~before1stleavecro+prop_batinact+Year+Age*Sex+(1|Bague)+(1|File),family="poisson",data=serdetcut)
# mnbint=glmer(nb_int~prop_batinact+(Year+Age+Sex)^2+(1|Bague)+(1|File),family="poisson",data=serdetcut)
# mnbintmi=glmer(nb_int~prop_batinact+(Year+Age+Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut)
# mnbintmiy=glmer(nb_int~prop_batinact+(Year+Age*Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut,nAGQ=0)
# mnbintmis=glmer(nb_int~prop_batinact+(Year*Age+Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut,nAGQ=0)
# mnbintmia=glmer(nb_int~prop_batinact+(Year*Sex+Age)+(1|Bague)+(1|File),family="poisson",data=serdetcut,nAGQ=0)
# anova(mnbintmi,mnbintmiy)
# anova(mnbintmi,mnbintmia)
# anova(mnbintmi,mnbintmis)
#
# mnbintmiyear=glmer(nb_int~prop_batinact+Age+Sex+(1|Bague)+(1|File),family="poisson",data=serdetcut)
# mnbintmiage=glmer(nb_int~prop_batinact+(Year+Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut)
# mnbintmisex=glmer(nb_int~prop_batinact+(Year+Age)+(1|Bague)+(1|File),family="poisson",data=serdetcut)
# mnbintmiact=glmer(nb_int~(Year+Age+Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut)
# anova(mnbintmi,mnbintmiyear)
# anova(mnbintmi,mnbintmisex)
# anova(mnbintmi,mnbintmiage)
# anova(mnbintmi,mnbintmiact)

#grouping 2011 and 2012 against 2013
mnbintmi=glmer(nb_int~prop_batinact+(yeargrouped+Age+Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut,nAGQ=0)
mnbintmiy=glmer(nb_int~prop_batinact+(yeargrouped+Age*Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut,nAGQ=0)
mnbintmis=glmer(nb_int~prop_batinact+(yeargrouped*Age+Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut,nAGQ=0)
mnbintmia=glmer(nb_int~prop_batinact+(yeargrouped*Sex+Age)+(1|Bague)+(1|File),family="poisson",data=serdetcut,nAGQ=0)
anova(mnbintmi,mnbintmiy)
anova(mnbintmi,mnbintmia)
anova(mnbintmi,mnbintmis)
mnbintmiyear=glmer(nb_int~prop_batinact+Age+Sex+(1|Bague)+(1|File),family="poisson",data=serdetcut)
mnbintmiage=glmer(nb_int~prop_batinact+(yeargrouped+Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut)
mnbintmisex=glmer(nb_int~prop_batinact+(yeargrouped+Age)+(1|Bague)+(1|File),family="poisson",data=serdetcut)
mnbintmiact=glmer(nb_int~(yeargrouped+Age+Sex)+(1|Bague)+(1|File),family="poisson",data=serdetcut)
anova(mnbintmi,mnbintmiyear)
anova(mnbintmi,mnbintmisex)
anova(mnbintmi,mnbintmiage)
anova(mnbintmi,mnbintmiact)
anova(mnbintmi,mnbintmiyear)


#grouping 2011 and 2012 against 2013
mnbintmi=glmer.nb(nb_int~prop_batinact+(yeargrouped+Age+Sex)+(1|Bague)+(1|File),data=serdetcut,nAGQ=0)
mnbintmiy=glmer.nb(nb_int~prop_batinact+(yeargrouped+Age*Sex)+(1|Bague)+(1|File),data=serdetcut,nAGQ=0)
mnbintmis=glmer.nb(nb_int~prop_batinact+(yeargrouped*Age+Sex)+(1|Bague)+(1|File),data=serdetcut,nAGQ=0)
mnbintmia=glmer.nb(nb_int~prop_batinact+(yeargrouped*Sex+Age)+(1|Bague)+(1|File),data=serdetcut,nAGQ=0)


#mpropint=glmer(nb_int~AfterMR+prop_batinact+Year+Sex*Age+(1|Bague)+(1|File)+offset(log(durser)),family="poisson",data=serdetcut)
# nAGQ set to 0 for convergence issues with Laplace approximations. Does not affect results
# mpropint=glmer(nb_int~prop_batinact+(Year+Sex+Age)^2+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
# mpropintmi=glmer(nb_int~prop_batinact+(Year+Age+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
# mpropintmiy=glmer(nb_int~prop_batinact+(Year+Age*Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
# mpropintmis=glmer(nb_int~prop_batinact+(Year*Age+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
# mpropintmia=glmer(nb_int~prop_batinact+(Year*Sex+Age)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
# anova(mpropintmi,mpropintmiy)
# anova(mpropintmi,mpropintmia)
# anova(mpropintmi,mpropintmis)
#
# mpropintmiyear=glmer(nb_int~prop_batinact+Age+Sex+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
# mpropintmiage=glmer(nb_int~prop_batinact+(Year+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
# mpropintmisex=glmer(nb_int~prop_batinact+(Year+Age)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
# mpropintmiact=glmer(nb_int~(Year+Age+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
# anova(mpropintmi,mpropintmiyear)
# anova(mpropintmi,mpropintmisex)
# anova(mpropintmi,mpropintmiage)
# anova(mpropintmi,mpropintmiact)

mpropintmi=glmer(nb_int~prop_batinact+(yeargrouped+Age+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
mpropintmiy=glmer(nb_int~prop_batinact+(yeargrouped+Age*Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
mpropintmis=glmer(nb_int~prop_batinact+(yeargrouped*Age+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
mpropintmia=glmer(nb_int~prop_batinact+(yeargrouped*Sex+Age)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
anova(mpropintmi,mpropintmiy)
anova(mpropintmi,mpropintmia)
anova(mpropintmi,mpropintmis)
mpropintmisex=glmer(nb_int~prop_batinact+(yeargrouped+Age)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
mpropintmiage=glmer(nb_int~prop_batinact+(yeargrouped+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
mpropintmiyear=glmer(nb_int~prop_batinact+Age+Sex+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
mpropintmiact=glmer(nb_int~(yeargrouped+Age+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,family="poisson",data=serdetcut)
anova(mpropintmi,mpropintmiyear)
anova(mpropintmiage,mpropintmi)
anova(mpropintmiact,mpropintmi)
anova(mpropintmisex,mpropintmi)


#neg bin
mpropintmi=glmer.nb(nb_int~prop_batinact+(yeargrouped+Age+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
mpropintmiy=glmer.nb(nb_int~prop_batinact+(yeargrouped+Age*Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
mpropintmis=glmer.nb(nb_int~prop_batinact+(yeargrouped*Age+Sex)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)
mpropintmia=glmer.nb(nb_int~prop_batinact+(yeargrouped*Sex+Age)+(1|Bague)+(1|File)+offset(log(durser)),nAGQ=0,data=serdetcut)

# mdistint=lmer(avg_distpose~prop_batinact+(Year+Age+Sex)^2+(1|Bague)+(1|File),data=serdetcut)
# mdistintmi=lmer(avg_distpose~prop_batinact+(Year+Age+Sex)+(1|Bague)+(1|File),data=serdetcut)
# mdistintmiy=lmer(avg_distpose~prop_batinact+(Year+Age*Sex)+(1|Bague)+(1|File),data=serdetcut)
# mdistintmia=lmer(avg_distpose~prop_batinact+(Year*Sex+Age)+(1|Bague)+(1|File),data=serdetcut)
# mdistintmis=lmer(avg_distpose~prop_batinact+(Year*Age+Sex)+(1|Bague)+(1|File),data=serdetcut)
# anova(mdistintmiy,mdistintmi)
# anova(mdistintmi,mdistintmia)
# anova(mdistintmi,mdistintmis)
#
# mdistintmisms=lmer(avg_distpose~prop_batinact+Year*Age+(1|Bague)+(1|File),data=serdetcut)
# mdistintmismact=lmer(avg_distpose~(Year*Age+Sex)+(1|Bague)+(1|File),data=serdetcut)
# anova(mdistintmisms,mdistintmis)
# anova(mdistintmismact,mdistintmis)
#mdistint=lme(avg_distpose~prop_batinact+Year*Age+Sex,random=~1|Bague/File,data=serdetcut)


## REML, F tests: to estimate df: to be found
mdistintmi=lmer(avg_distpose~prop_batinact+(yeargrouped+Age+Sex)+(1|Bague)+(1|File),data=serdetcut,REML=T)
mdistintmiy=lmer(avg_distpose~prop_batinact+(yeargrouped+Age*Sex)+(1|Bague)+(1|File),data=serdetcut,REML=T)
anova(mdistintmiy,mdistintmi)
mdistintmis=lmer(avg_distpose~prop_batinact+(yeargrouped*Age+Sex)+(1|Bague)+(1|File),data=serdetcut,REML=T)
mdistintmispys=lmer(avg_distpose~prop_batinact+yeargrouped*(Age+Sex)+(1|Bague)+(1|File),data=serdetcut,REML=T)
mdistintmisms=lmer(avg_distpose~prop_batinact+yeargrouped*Age+(1|Bague)+(1|File),data=serdetcut,REML=T)
mdistintmismact=lmer(avg_distpose~(yeargrouped*Age+Sex)+(1|Bague)+(1|File),data=serdetcut,REML=T)
anova(mdistintmi,mdistintmis,refit=F)
anova(mdistintmisms,mdistintmis)
anova(mdistintmismact,mdistintmis)
anova(mdistintmis,mdistintmispys)
mdistintmiy2=lmer(avg_distpose~prop_batinact+Age*(yeargrouped+Sex)+(1|Bague)+(1|File),data=serdetcut,REML=T)




########## Mass gain analyses
setwd("D:/Pro/These/Donnees/WAAL/")
allmass=read.table("BirdsData/MassMeasures11-12-13.csv",sep=";",h=T)

# add a corrected mass gain column to metadata
meta$CorrectedMassGain=rep(NA,dim(meta)[1])
meta$NonCorrMG=rep(NA,dim(meta)[1])
meta$MassAtDep=rep(NA,dim(meta)[1])
allmass$Bague=as.character(allmass$Bague)

for (i in 1:dim(meta)[1]){
  b=as.character(meta$Bague[i])
  y=meta$Year[i]

  if (sum(allmass$Year==y & allmass$Bague==b)==1){
    fi=meta$Filename[i]
    tr=read.table(paste("BirdsData/",y,"/j-n_",fi,"_VitPoseCap.csv",sep=""),sep=";",h=T)

    i.mass=which(allmass$Year==y & allmass$Bague==b)

    datepose=as.POSIXct(strptime(as.character(allmass$DatePose[i.mass]),format="%d/%m/%Y %H:%M"),tz="GMT")
    daterecup=as.POSIXct(strptime(as.character(allmass$DateRecup[i.mass]),format="%d/%m/%Y %H:%M"),tz="GMT")

    datedep=as.POSIXct(strptime(paste(tr$Date[1],tr$Time[1],sep=" "),format="%d/%m/%Y %H:%M:%S"),tz="GMT")
    dateret=as.POSIXct(strptime(paste(tr$Date[dim(tr)[1]],tr$Time[dim(tr)[1]],sep=" "),format="%d/%m/%Y %H:%M:%S"),tz="GMT")

    lagdep=difftime(datedep,datepose,units="days")
    lagret=difftime(daterecup,dateret,units="days")

    corr.rate=ifelse(meta$Sexe[i]=="F",70,100)

    corrdep=corr.rate*lagdep
    corrret=ifelse(lagret>1,corr.rate*(2+lagret-1),2*corr.rate*lagret)

    meta$MassAtDep[i]=allmass$MassePose_g[i.mass]-corrdep

    meta$CorrectedMassGain[i]=(allmass$MasseRecup_g[i.mass]+corrret)-(allmass$MassePose_g[i.mass]-corrdep)
    meta$NonCorrMG[i]=allmass$MasseRecup_g[i.mass]-allmass$MassePose_g[i.mass]

    if (meta$Bague[i]=="BS11218"){
      meta$CorrectedMassGain[i]=NA
      meta$NonCorrMG[i]=NA
    }
  }
}
meta$AnomMassAtDep=ifelse(meta$Sexe=="F",meta$MassAtDep-mean(meta$MassAtDep[meta$Sexe=="F"],na.rm=T),meta$MassAtDep-mean(meta$MassAtDep[meta$Sexe=="M"],na.rm=T))


massmeta=meta[is.na(meta$CorrectedMassGain)==F,]

massmeta$PropMG=massmeta$CorrectedMassGain/massmeta$Duration #en g/J
massmeta$PropInt=massmeta$nb_int/4/massmeta$Duration   #en h/J
massmeta$Int=ifelse(massmeta$PropInt>0,1,0)

massmeta$resInt=residuals(lm(Int~Duration,data=massmeta))
massmeta$resPropInt=residuals(lm(PropInt~Duration,data=massmeta))

#mass gain models
mgmi=lm(CorrectedMassGain~AnomMassAtDep+Duration+Sexe*Int+Year,data=massmeta)
mgmp=lm(CorrectedMassGain~AnomMassAtDep+Duration+Sexe*PropInt+Year,data=massmeta[massmeta$Int==1,])

mgmi=lm(CorrectedMassGain~AnomMassAtDep+Duration+Sexe*resInt+Year,data=massmeta)
mgmp=lm(CorrectedMassGain~AnomMassAtDep+Duration+Sexe*resPropInt+Year,data=massmeta[massmeta$Int==1,])

#mass gain rate models
mgrmi=lm(PropMG~AnomMassAtDep+Duration+Sexe*Int+Year,data=massmeta)
mgrmp=lm(PropMG~AnomMassAtDep+Duration+Sexe*PropInt+Year,data=massmeta[massmeta$Int==1,])

mgrmi=lm(PropMG~AnomMassAtDep+Duration+Sexe*resInt+Year,data=massmeta)
mgrmp=lm(PropMG~AnomMassAtDep+Duration+Sexe*resPropInt+Year,data=massmeta[massmeta$Int==1,])


library(classInt)
library(RColourBrewer)

## Figure 2: only the average number of boats influence encounter rates
# clas=classIntervals(meta$Age,5,style="quantile")
# colcodef=findColours(clas,brewer.pal(5,"Greens"))
# colcodem=findColours(clas,brewer.pal(5,"Reds"))

symcode=ifelse(meta$yeargrouped=="2013",21,24)

ER=meta$nb_serdetjour/meta$Duration
par(mfrow=c(1,1))
par(mar=c(5,5,1,1))
plot(ER~meta$avg_nbbat,col="white",xlab="Average number of vessels during trip",ylab="Number of encounter events per day of trip",cex.axis=1.5,cex.lab=1.2)
points(ER[meta$Sexe=="F"]~meta$avg_nbbat[meta$Sexe=="F"],pch=symcode[meta$Sexe=="F"],bg="light grey",cex=1.2)#paste(colcodef[meta$Sexe=="F"],"99",sep="")
points(ER[meta$Sexe=="M"]~meta$avg_nbbat[meta$Sexe=="M"],pch=symcode[meta$Sexe=="M"],bg="black",cex=1.2)#paste(colcodem[meta$Sexe=="M"],"99",sep="")





### Figure 3: proba to int
act=serdet$lagvesact/3600
categ=c(-24,-12,-6,-3,-2,-1,-0.5,-0.25,0,0.25,0.5,1,2,3,6,12)
propint=c()
avgtim=c()
ncat=c()
for (i in 2:length(categ)){
  propint=c(propint,sum(serdet$int==1 & act>=categ[i-1] & act<categ[i],na.rm=T)/sum(act>=categ[i-1] & act<categ[i],na.rm=T))
  avgtim=c(avgtim,mean(act[act>=categ[i-1] & act<categ[i]],na.rm=T))
  ncat=c(ncat,sum(act>=categ[i-1] & act<categ[i],na.rm=T))
}
plot(propint~avgtim,xlab="Time to next boat activity (hours)",ylab="Probability to join a boat upon encounter",pch=16,ylim=c(0,1),font=2)
lines(c(0,0),c(-0.5,1.5),lty=2)

mintact=glm(int~Act,family="binomial",data=serdet)
ymod=predict(mintact,type="response",se.fit=F)[order(act)]
ordact=act[order(act)]
lines(ordact[ordact>0],ymod[ordact>0],col="red",lwd=2)
lines(ordact[ordact<0],ymod[ordact<0],col="red",lwd=2)

text(avgtim,propint+0.03,as.character(ncat),cex=0.65)



# Figure 4:: female effects
par(mfrow=c(2,2),mar=c(5,7,1,1))
plot(NULL)#plot(c(1,2),c(1,2),col="white",xaxt=NULL,yaxt=NULL)
nbint=serdetcut$dur_attend/4
propint=serdetcut$dur_attend/serdetcut$durser
boxplot(nbint~serdetcut$Sex,xlab="Sex",ylab=paste("Time spent attending","\n", "boats per encounter (h)",sep=""),col="grey",cex.lab=1.2,cex.axis=1.2)
points(c(1,2),c(mean(fitted(mnbint)[serdetcut$Sex=="F"])/4,mean(fitted(mnbint)[serdetcut$Sex=="M"])/4),pch=16,col="red")
par(mar=c(5,7,1,1))
boxplot(propint~serdetcut$Sex,xlab="Sex",ylab=paste("Proportion of the encounter","\n", "spent attending boats",sep=""),col="grey",cex.lab=1.2,cex.axis=1.2)
points(c(1,2),c(mean(fitted(mpropint)[serdetcut$Sex=="F"])/mean(serdetcut$durser[serdetcut$Sex=="F"]),mean(fitted(mpropint)[serdetcut$Sex=="M"])/mean(serdetcut$durser[serdetcut$Sex=="M"])),pch=16,col="red")
boxplot(serdetcut$avgdist_attend~serdetcut$Sex,xlab="Sex",ylab=paste("Distance from boats","\n", "when attending them (m)",sep=""),col="grey",cex.lab=1.2,cex.axis=1.2)
points(c(1,2),c(mean(fitted(mdistint)[serdetcut$Sex=="F"]),mean(fitted(mdistint)[serdetcut$Sex=="M"])),pch=16,col="red")


# Figure 5: Age and year
par(mfrow=c(1,3),mar=c(5,7,1,1))
boxplot(nbint~serdetcut$Year,xlab="Year",ylab=paste("Time spent attending","\n", "boats per encounter (h)",sep=""),col="grey",cex.lab=1.5,cex.axis=1.2)
# symcode=ifelse(serdetcut$yeargrouped==1,16,21)
 colcode=ifelse(serdetcut$yeargrouped==1,"black","grey")
# plot(propint~serdetcut$Age,xlab="Age",ylab=paste("Proportion of the encounter","\n", "spent sitting within 3km",sep=""),pch=symcode,bg="dark grey",cex.lab=1.2,cex.axis=1.2)
boxplot(propint~serdetcut$Year,xlab="Year",ylab=paste("Proportion of the encounter","\n", "spent attending boats",sep=""),col="grey",cex.lab=1.5,cex.axis=1.2)
plot(serdetcut$avgdist_attend~serdetcut$Age,xlab="Age",ylab=paste("Distance from boats","\n", "when attending them",sep=""),pch=21,bg=colcode,cex.lab=1.5,cex.axis=1.2,cex=1.2)
lines(c(0,60),c(1677,1677+60*(5.802-19.981)),col="black",lwd=2)

# Figure 6: Mass gain
par(mfrow=c(1,1))
par(mar=c(5,5,1,1))
mmnbint=massmeta$nb_int/4
plot(massmeta$CorrectedMassGain~mmnbint,pch=16,xlab=paste("Time spent interacting ", "over the trip (hours)",sep=""),ylab="Mass gained at the end of the trip (g)",cex.lab=1.2,cex.axis=1.2)
points(massmeta$CorrectedMassGain[massmeta$Sexe=="F"]~mmnbint[massmeta$Sexe=="F"],pch=21,bg="light grey")


# Figure 7: Foraging efficiency
par(mfrow=c(1,2))
plot(massmeta$PropMG~massmeta$Duration,pch=16,xlab="Trip Duration (hours)",ylab="Foraging efficiency (g/day of trip)",cex.lab=1.2,cex.axis=1.2)
points(massmeta$PropMG[massmeta$Sexe=="F"]~massmeta$Duration[massmeta$Sexe=="F"],pch=21,bg="light grey")
boxplot(meta$Duration~meta$int,col="grey",xlab="Interaction during the trip",ylab="Trip Duration (hours)",cex.lab=1.2,cex.axis=1.2)







### Figure 4: nb int/encount
nbint=serdetcut$nb_int/4
symcode=ifelse(serdetcut$yeargrouped==1,21,24)

plot(nbint~serdetcut$Age,col="white",xlab="Age",ylab="Time spent sitting within 3km of a boat at each encounter (h)")
points(nbint[serdetcut$Sex=="F"]~serdetcut$Age[serdetcut$Sex=="F"],pch=symcode[serdetcut$Sex=="F"],bg="light grey")#paste(colcodef[meta$Sexe=="F"],"99",sep="")
points(nbint[serdetcut$Sex=="M"]~serdetcut$Age[serdetcut$Sex=="M"],pch=symcode[serdetcut$Sex=="M"],bg="black")#paste(colcodef[meta$Sexe=="F"],"99",sep="")


age

### Figure 5: prop int
age

### Figure 6: dist int
age

### Figure 7: mass gain
MG nbint
Emg propint



















## Figure 2: 2013 effects
par(mfrow=c(2,2),mar=c(3,7,1,1))

plot(serdetcut$prop_batinact~serdetcut$Year,ylab=paste("Proportion of encounter time","\n", "with vessel in activity",sep=""),col="grey",xlab="",cex.lab=1.5,cex.axis=1.5)

propint=serdetcut$nb_int/serdetcut$durser
respropint=residuals(lm(propint~serdetcut$prop_batinact+serdetcut$Sex+serdetcut$Age))
plot(respropint~serdetcut$Year,ylab=paste("Anomaly in proportion of the ","\n", "encounter spent interacting",sep=""),col="grey",xlab="",cex.lab=1.5,cex.axis=1.5)
lines(c(0,3000),c(0,0),lty=3,lwd=2)

par(mar=c(5,7,1,1))
#resdist=residuals(lm(avg_distpose~prop_batinact+Sex+Age,data=serdetcut))
#plot(resdist~serdetcut$Year,ylab=paste("Average anomaly in distance","\n", "from boats when interacting (m)",sep=""),col="grey",xlab="",cex.lab=1.5,cex.axis=1.5)
#lines(c(0,3000),c(0,0),lty=3,lwd=2)
resdist=residuals(lm(avg_distpose~prop_batinact+Sex,data=serdetcut))
symcode=c(0,2,1)[as.factor(serdetcut$Year)]
plot(resdist~serdetcut$Age,ylab=paste("Average anomaly in distance","\n", "from boats when interacting (m)",sep=""),pch=symcode,col="black",xlab="Age",cex.lab=1.5,cex.axis=1.5)
points(resdist[serdetcut$Year==2013]~serdetcut$Age[serdetcut$Year==2013],pch=19,col="black",cex=1.5)
lines(c(0,3000),c(0,0),lty=3,lwd=2)
lines(c(0,50),c(756,756-50*22.93),lwd=3,col='black')


resint=residuals(lm(nb_int~prop_batinact+Age+Sex,data=serdetcut))
plot(resint/4~serdetcut$Year,ylab=paste("Anomaly in absolute amount","\n", "of time spent interacting (h)",sep=""),col="grey",xlab="",cex.lab=1.5,cex.axis=1.5)
lines(c(0,3000),c(0,0),lty=3,lwd=2)



## Figure 3: Female effects
par(mfrow=c(2,2),mar=c(5,7,1,1))

resint=residuals(lm(nb_int~prop_batinact+Year,data=serdetcut))
plot(resint/4~serdetcut$Age,ylab=paste("Anomaly in absolute amount","\n", "of time spent interacting (h)",sep=""),col="white",xlab="Age",cex.lab=1.5,cex.axis=1.5)
points(resint[serdetcut$Sex=="M"]/4~serdetcut$Age[serdetcut$Sex=="M"],pch=symcode,col="red")
points(resint[serdetcut$Sex=="F"]/4~serdetcut$Age[serdetcut$Sex=="F"],pch=symcode,col="green")
lines(c(0,3000),c(0,0),lty=3,lwd=2)

par(mar=c(3,7,1,1))

resdist=residuals(lm(avg_distpose~prop_batinact+Year*Age,data=serdetcut))
plot(resdist~serdetcut$Sex,ylab=paste("Average anomaly in distance","\n", "from boats when interacting (m)",sep=""),col="dark grey",xlab="",cex.lab=1.5,cex.axis=1.5)
lines(c(0,3000),c(0,0),lty=3,lwd=2)


propint=serdetcut$nb_int/serdetcut$durser
respropint=residuals(lm(propint~serdetcut$prop_batinact+serdetcut$Year+serdetcut$Age))
symcode=c(15,17,19)[as.factor(serdetcut$Year)]
# plot(respropint~serdetcut$Age,ylab=paste("Anomaly in proportion of the ","\n", "encounter spent interacting",sep=""),col="white",xlab="Age",cex.lab=1.5,cex.axis=1.5)
# points(respropint[serdetcut$Sex=="M"]~serdetcut$Age[serdetcut$Sex=="M"],pch=symcode,col="red")
# points(respropint[serdetcut$Sex=="F"]~serdetcut$Age[serdetcut$Sex=="F"],pch=symcode,col="green")
plot(respropint~serdetcut$Sex,ylab=paste("Anomaly in proportion of the ","\n", "encounter spent interacting",sep=""),col="grey",xlab="",cex.lab=1.5,cex.axis=1.5)
lines(c(0,3000),c(0,0),lty=3,lwd=2)





