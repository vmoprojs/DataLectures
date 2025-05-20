#-----------------------------------------------------------------
# Enterprise: FLACSO
# Proyect: Impacto de un programa de movilidad no motorizado: BiciQ
# Data: df1
# Author: Víctor Morales Oñate
# Date: 14-08-2018
#-----------------------------------------------------------------
rm(list = ls())
graphics.off()
setwd("~/Documents/DataBase/ENEMDU")
load("BiciQ.RData")

# Librerias
library(rio)
library(Hmisc)
library(Synth)


yrs <- 2006:2017



treat.id <- which(unique(df1$ciudad)==1701)#treatment.identifier

dataprep.out<-  
  dataprep(
    foo = df1,
    predictors = c("propia", "urbano"),
    predictors.op = "mean",
    dependent = "bici",
    unit.variable = "id",
    time.variable = "year",
    special.predictors = list(
      list("bici", 2011, "mean"),
      list("bici", 2012, "mean"),
      list("bici", 2013, "mean")
    ),
    treatment.identifier = treat.id,
    controls.identifier = unique(df1$id)[-treat.id],
    time.predictors.prior = c(2007:2009),
    time.optimize.ssr = c(2010:2011),
    unit.names.variable = "ciudad",
    time.plot = 2007:2014
  )




## run the synth command to identify the weights
## that create the best possible synthetic 
## control unit for the treated.
synth.out <- synth(dataprep.out)


## there are two ways to summarize the results
## we can either access the output from synth.out directly
round(synth.out$solution.w,2)
round(synth.out$solution.w,2)[round(synth.out$solution.w,2)!=0]
which(round(synth.out$solution.w,2)!=0)
# contains the unit weights or
synth.out$solution.v 
## contains the predictor weights. 

## the output from synth opt 
## can be flexibly combined with 
## the output from dataprep to 
## compute other quantities of interest
## for example, the period by period 
## discrepancies between the 
## treated unit and its synthetic control unit
## can be computed by typing
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
) ; gaps

## also there are three convenience functions to summarize results.
## to get summary tables for all information 
## (V and W weights plus balance btw. 
## treated and synthetic control) use the 
## synth.tab() command
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

## to get summary plots for outcome trajectories 
## of the treated and the synthetic control unit use the 
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out,
          Ylab = c("Bici"),
          Xlab = c("Años"),Legend=c("Quito","Quito sintético"))
abline(v = 2012, col = "lightblue")

# str(dataprep.out$Y0plot)
# str(synth.out)
# plot(dataprep.out$Y1plot)

## plot the gaps (treated - synthetic)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)
abline(v = 2012)


synthY0 <- (dataprep.out$Y0%*%synth.out$solution.w)
QY0 <- dataprep.out$Y1plot

dat.aux <- data.frame(Q = as.numeric(QY0), S = as.numeric(synthY0), D = as.numeric((QY0-synthY0)), per = c(rep(0,5),rep(1,3)))
aggregate(dat.aux[,-4],list(dat.aux$per),mean)


round(0.001956137*100,2) #diferencia antes
round(0.014634894*100,2)# diferencia despues





### Figure 6: Leave-one-out distribution of the synthetic control for West Germany

# loop over leave one outs
storegaps <- 
  matrix(NA,
         length(2007:2014),
         4)
colnames(storegaps) <- c(16,18,24,60)
co <- unique(df1$id)[-treat.id]

for(k in 1:4){
  
  # data prep for training model
  omit <- c(16,18,24,60)[k]  
  dataprep.out<-  
    dataprep(
      foo = df1,
      predictors = c("propia", "urbano"),
      dependent = "bici",
      unit.variable = "id",
      time.variable = "year",
      special.predictors = list(
        list("bici", 2011, "mean"),
        list("bici", 2012, "mean"),
        list("bici", 2013, "mean")
      ),
      treatment.identifier = treat.id,
      controls.identifier = co[-which(co==omit)],
      time.predictors.prior = c(2007:2009),
      time.optimize.ssr = c(2010:2011),
      unit.names.variable = "ciudad",
      time.plot = 2007:2014
    )
  
  # fit training model
  synth.out <- synth(
    data.prep.obj=dataprep.out,
    Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
  )
  
  # data prep for main model
  dataprep.out <-
    dataprep(
      foo = df1,
      predictors    = c("propia", "urbano"),
      dependent     = "bici",
      unit.variable = "id",
      time.variable = "year",
      special.predictors = list(
        list("bici", 2011, "mean"),
        list("bici", 2012, "mean"),
        list("bici", 2013, "mean")
      ),
      treatment.identifier = treat.id,
      controls.identifier = co[-which(co==omit)],
      time.predictors.prior = c(2010:2011),
      time.optimize.ssr = c(2007:2011),
      unit.names.variable = "ciudad",
      time.plot = 2007:2014
    )
  
  # fit main model 
  synth.out <- synth(
    data.prep.obj=dataprep.out,
    custom.v=as.numeric(synth.out$solution.v)
  )
  storegaps[,k] <- (dataprep.out$Y0%*%synth.out$solution.w)
  print(k)
} # close loop over leave one outs

Text.height <- .3
Cex.set <- .8
#pdf(file = "1jackknife2.pdf", width = 5.5, height = 5.5, family = "Times",pointsize = 12)
plot(2007:2014,dataprep.out$Y1plot,
     type="l",ylim=c(0.2,.38),col="black",lty="solid",
     ylab ="Bici",
     xlab ="Años",
     xaxs = "i", yaxs = "i",lwd=2
)

abline(v=2012,lty="dotted")
# arrows(2010,Text.height,2012,Text.height,col="black",length=.1)
for(i in 1:4){
  lines(2007:2014,storegaps[,i],col="darkgrey",lty="solid")
}
lines(2007:2014,synthY0,col="black",lty="dashed",lwd=2)
lines(2007:2014,dataprep.out$Y1plot,col="black",lty="solid",lwd=2)
legend(x="bottomright",
       legend=c("Quito",
                "Quito sintético",
                "Quito (leave-one-out)")
       ,lty=c("solid","dashed","solid"),
       col=c("black","black","darkgrey")
       ,cex=.8,bg="white",lwdc(2,2,1))

