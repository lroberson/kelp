library(BiodiversityR)
library(permute) # required by vegan
library(vegan) #biodiversity indices for ecology

#save pivot table as csv with no headings, fill blanks with zeros
#read.table("DyerFishMV.csv",sep=",")
#reads sample number as rows and fish species as variables called V1 etc

names(data)
summary(data)

#Calculate Shannon Wiener for each sample
H=diversity(data) #using vegan

<><><><><><><><><><><><><><SPECIES ACCUMULATION CURVE><><><><><><><><><>

sp1 = specaccum(data)
sp2 = specaccum(data, method = "random", permutations = 100)

## comm: community data set
## method: Spp accum method--> "collector" adds sites in the order they happen to be in the data, "random" adds in random order, "coleman" finds the expected richness following Coleman et al. 1982, and "rarefaction" finds the mean when accumulating individuals instead of sites.
## permutations: number of perms with method = "random"
## conditioned: Estimation of standard deviation is conditional on the empirical dataset for the method = "exact" SAC
## gamma: Method for estimating the total extrapolated number of species in the survey area by function specpool
## x: a specaccum result object 
## ... : other parameters to functions

## S3 method for class 'specaccum':
quartz
#plot(sp1, ci = 2, ci.type = "line", col = par("fg"), ci.col = col, ci.lty = 1, xlab = "Sites", ylab = "Species")
   #type can be bar or polygon as well
plot(sp2, col="black", ci.type="polygon", lwd=2, ci.col="grey", border = FALSE, xlab = "Number of samples", ylab = "Species richness") 
abline(h = (.95*26), v = 11, reg = NULL, coef = NULL, untf = FALSE, lty="dotted") #OMG this worked!
#plot(sp2, add = TRUE, xlab = "Number of samples", ylab = "Species richness",abline(h=(.95*26), col="red",),abline(v=11))
#ci.lty=0, ci.col="black"
## S3 method for class 'specaccum'

sp = predict.specaccum(object, newdata, interpolation = "spline")

#plot(x, add = FALSE, random = FALSE, ci = 2, 
   # ci.type = c("bar", "line", "polygon"), col = par("fg"), ci.col = col, 
    #ci.lty = 1, xlab, ylab = x$method, ylim, 
    # xvar = c("sites", "individuals", "effort"), ...)
## S3 method for class 'specaccum'
# boxplot(x, add = FALSE, ...)
#fitspecaccum(object, model, method = "random", ...)
## S3 method for class 'fitspecaccum'
# plot(x, col = par("fg"), lty = 1, xlab = "Sites", 
    # ylab = x$method, ...) 
## S3 method for class 'specaccum'
# predict(object, newdata, interpolation = c("linear", "spline"), ...)
## S3 method for class 'fitspecaccum'
#predict(object, newdata, ...)
specslope(object, at)

specaccum(data, method = "random", permutations = 100,
          conditioned = FALSE, gamma = "jack1",  w = NULL)

## S3 method for class 'specaccum':
boxplot((x, add = FALSE, ...)
        fitspecaccum(object, model, method = "random", ...))

## S3 method for class 'fitspecaccum':
plot((x, col = par("fg"), lty = 1, xlab = "Sites", 
      ylab = x$method, ...))

## S3 method for class 'specaccum':
predict(object, newdata, interpolation = c("spline"))

## S3 method for class 'fitspecaccum':
predict(object, newdata, ...)

<><><><><><><><>ANOTHER OPTION <<><><><><><><><><><><><><>
sp1 = specaccum(data)
sp2 = specaccum(data, "random")
sp2
summary(sp2)

plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

mod1 = fitspecaccum(sp1, "lomolino")
coef(mod1)
fitted(mod1)
quartz
plot(sp1)
## Fit Lomolino model to the exact accumulation
plot(mod1, add = TRUE, col=2, lwd=2)
  ## Fit Arrhenius models to all random accumulations
mods = fitspecaccum(sp2, "arrh")
quartz
plot(mods, col="hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)
     ## Use nls() methods to the list of models
sapply(mods$models, AIC)

specslope


<><><><><><><><><><<><> HENNINGS WAY <><><><><><><><><><><><><><><><><><><><
padat = ifelse(data==0,0,1)
# accumulation curve
acc.curve = NULL
for(i in 1:nrow(data))
{
acc.curve = c(acc.curve,sum(ifelse(apply(data[1:i,],2,sum)==0,0,1)))
}

plot(1:nrow(data),acc.curve,type="l",xlab="Sample",ylab="Species",lwd=2,col="black")

for(i in 1:20) lines(1:nrow(data),rcurves[i,])

apply(rcurves,2,quantile,c(0.975,0.95,0.9))/ncol(data)

names(data)

plot(1:nrow(data),acc.curve,type="l",xlab="Number of samples",ylab="Species richness",lwd=2,col=2,abline(h=(.95*26),coef=NULL,untf=FALSE, \dots))

apply(rcurves,2,quantile,c(0.025,0.975))/ncol(data)

nboot=1000
rcurves = mat.or.vec(nboot,nrow(data))
for(k in 1:nboot)
{
# resample data
rdat = data[sample(nrow(data)),]

for(i in 1:nrow(data))
{
rcurves[k,i]= sum(ifelse(apply(rdat[1:i,],2,sum)==0,0,1))
}
}
apply(rcurves,2,quantile,c(0.975,0.95,0.9))/ncol(data)


CIs = apply(rcurves,2,quantile,c(0.025,0.975))


plot(1:nrow(data),acc.curve,type="l",xlab="Number of samples",ylab="Species richness",col="black",lwd=2,abline(h=(.95*26),coef=NULL,untf=FALSE, \dots))
lines(1:nrow(data),CIs[1,],lty=3,col=2)
lines(1:nrow(data),CIs[2,],lty=3,col=2)



#Arguments for BIODIVERSITY R

# x: Community data frame with sites as rows, species as columns and species abundance as cell values
# y: Environmental data frame. 
# factor: Variable of the environmental data frame that defines subsets to calculate diversity statistics for. 
# level: Level of the variable to create the subset to calculate diversity statistics. 
# index: Type of diversity statistic with "richness" to calculate species richness, "abundance" to calculate abundance, "Shannon" to calculate the Shannon diversity index, "Simpson" to calculate 1-Simpson concentration index, "inverseSimpson" to calculate the reciprocal Simpson diversity index, "Logalpha" to calculate the log series alpha diversity index, "Berger" to calculate the reciprocal Berger-Parker diversity index, "Jevenness" to calculate one Shannon evenness index, "Eevenness" to calculate another Shannon evenness index, "jack1" to calculate the first-order jackknife gamma diversity estimator, "jack2" to calculate the second-order jackknife gamma diversity estimator, "chao" to calculate the Chao gamma diversity estimator and "boot" to calculate the bootstrap gamma diversity estimator. 
# method: Method of calculating the diversity statistics: "all" calculates the diversity of the entire community (all sites pooled), "s" calculates the diversity of each site separatedly, "mean" calculates the average diversity of the sites, "sd" calculates the standard deviation of the diversity of the sites, whereas "jackknife" calculates the jackknifed diversity for the entire data frame. Method "s" is not allowed for diversitycomp. 
# sortit: Sort the sites by increasing values of the diversity statistic. 
# digits: Number of digits in the results. 
# factor1: Variable of the environmental data frame that defines subsets to calculate diversity statistics for
# factor2 : Optional second variable of the environmental data frame that defines subsets to calculate diversity statistics for in a crosstabulation with the other variable of the environmental data frame

