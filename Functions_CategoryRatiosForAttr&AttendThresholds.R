###############################################################################################################
#####  These functions will serve to establish attraction and attendance thresholds for your data
#####  The idea is to separate your data (of interest, see below) into 2 broad categories: those fulfilling a test, and the others.
#####  For instance, for the attending threshold, we use a test of "foraging behaviour" or not, in Collet et al 2015 this test was a speed threshold (true if <10km/h = foraging, false if faster)
#####  For the attraction threshold, a test based on whether the flight direction is towards boat was used (with a threshold value of 10° in Collet et al 2015)
#####  Then these functions will establish in a relatively computer-efficient way (based on plyr package and apply functions)
#####  the proportion of points fulfilling the test (compared to not fulfilling) for discrete categories of distance to the boat
#####  and then plot them

#####  For most users, only makeratio and plot.ratio should be used, the 2 others are internal calls by makeratio

#####  Note that *YOU* must provide the test ("test" argument of function makeratio) and the distance categories ("cat" argument for all functions)
#####  test is a binary vector with 1 if the test is fulfilled, 0 otherwise, for each of your datapoint
#####  cat is a vector containing the limits of your categories (e.g. c(0:150) for categorizing into 1km bins from 0 to 150km)
#####  note that "cat" should contain the lower bound of the smallest category, as well as the upper bond of the higher category (e.g. to have bins 0-1km, 1-2km, ... 149-150km, cat=c(0:150))
#####  cat needs not be regularly spaced, you could create bins of varying width (e.g. cat=c(seq(0,1.5,0.5),seq(2,4,1),seq(5,150,5)))
#####  for the plot, the x-axis takes the midpoint value of your bin (e.g. sum(bin bonds)/2)



### Functions to establish (distance) categorisations
categ.suplim=function(X,cat){ ### returns the born this number is just under
  if(X[2]==1){ ### if the number to be categorized is equal to one of the bonds
    i=which(cat==X[1])+1
    if(i>length(cat) | i<=1){out=NA} else{out=(cat[i]+cat[i-1])/2}
    return(out)
  }
  if(X[2]==0){
    all=c(cat,X[1])
    i=which(all[order(all)]==X[1])
    if(i>length(cat) | i<=1){out=NA} else{out=(cat[i]+cat[i-1])/2}
    return(out)
  }
}

categorize=function(x,cat){
  pilecat=ifelse(x%in%cat,1,0)
  out=apply(cbind(x,pilecat),1,categ.suplim,cat)
}




### Function to establish ratio per category
makeratio=function(x,test,cat,ratio=F){
  categorizedx=categorize(x,cat)
  tab=data.frame('x'=x,'test'=test,'categorizedx'=factor(categorizedx,levels=as.character((cat[-1]+cat[-length(cat)])/2)))[is.na(categorizedx)==F,]

  if(ratio==T){
    rapport=plyr::ddply(tab,~categorizedx,function(X){out=c((sum(X$test==1)/sum(X$test==0)),length(X$test))},.drop=F)
    names(rapport)=c("category","ratio","n")
  }
  if (ratio==F){
    rapport=plyr::ddply(tab,~categorizedx,function(X){c((sum(X$test==1)/length(X$test)),length(X$test))},.drop=F)
    names(rapport)=c("category","rapport","n")
  }
  out=list(tab,rapport,list(cat,x,ratio))

  return(out)
}




### Function to plot the results
plot.ratio=function(ratio,expect=NA){   #ratio an object returned by makeratio
  cat=ratio[[3]][[1]]
  abscisse=(cat[1:(length(cat)-1)]+cat[2:length(cat)])/2
  maxi=ifelse(ratio[[3]][[3]]==F,1,ceiling(max(ratio[[2]][,2],na.rm=T)))
  if(is.infinite(maxi)==T | is.na(maxi)==T){
    print("WARNING, infinite value occurred, or no values at all: plot y-bounded to 0-10")
    maxi=10
  }

  plot(ratio[[2]][,2]~abscisse,pch=21,bg="grey",ylim=c(0,maxi),xaxp=c(cat[1],cat[length(cat)],((length(cat)-1)/10)),xlab="Distance (km)",ylab=ifelse(ratio[[3]][[3]]==F,"Proportion","Ratio"))
  if(is.na(expect)==F){
    lines(c(min(cat),max(cat)),c(expect,expect),lwd=2,col="red")
  }
}


### Fit a broken-line model to estimate the break point
library(segmented)

abscisse=(cat[1:(length(cat)-1)]+cat[2:length(cat)])/2
rapport=ratio[[2]][,2]
estimate=c()
for (i in 1:100){  # in former version of segmented, instability in the break point estimation: several "peak" of values.
  fit.seg=segmented(lm(rapport~abscisse),~abscisse,5)
  estimate=c(estimate,fit.seg$psi[2])
}

fit.seg=segmented(lm(rapport~abscisse),~abscisse,5)
fit.seg
confint.segmented(fit.seg)

# to add model lines to the plot
lines(fit.seg)
lines(abscisse,broken.line(fit.seg)$fit)
lines(abscisse,(broken.line(fit.seg)$fit+2*broken.line(fit.seg)$se.fit),lty=3)
lines(abscisse,(broken.line(fit.seg)$fit-2*broken.line(fit.seg)$se.fit),lty=3)






