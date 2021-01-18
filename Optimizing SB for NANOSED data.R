# RIVM 2017 - Joris Quik
# Basic script for accessing SimpleBox from R

library(openxlsx)
library(ggplot2)

# adjust to correct location of SimpleBox xls file
sb4n.loc <- paste(getwd(),"/data/SimpleBox4.0-nano/SimpleBox4.01-nano.xlsm",sep="")

# readin the required data:
SB.K <- as.matrix(read.xlsx(sb4n.loc,colNames=FALSE, namedRegion ="K")) # matrix of rate constants "k"
SB.e <- read.xlsx(sb4n.loc,colNames=FALSE, namedRegion ="emis") # Emission rates for each compartment
SB.m0 <- read.xlsx(sb4n.loc,colNames=FALSE, namedRegion ="Dyn_m0") # Initial mass of each compartment
SB.names <- read.xlsx(sb4n.loc,colNames=FALSE, namedRegion ="box_names") #Names for each compartment
SB.v <- read.xlsx(sb4n.loc,colNames=FALSE, namedRegion ="v") #Volumes

# Running solver on SimpleBox matrix for dynamic Run of SimpleBox
library(deSolve)

# The function to run SimpleBox:
SimpleBoxODE <- function(t, m, parms) {
  dm <- with(parms, K %*% m + e)
  return(list(dm))
}

tend1 <- 60*60*24*365*1000 # in seconds (default 1000 years)
tend2 <- 2*tend1
emis2 <- rep(0,length.out=length(SB.e))
tset1 <- seq(from=0, to=tend1, length.out=50)
tset2 <- seq(from=tend1, to=tend2, length.out=50)

# list of input parameters part 1:
Parms1 <- list(e=SB.e, K=as.matrix(SB.K))
Rpar.SBdyn1 <- list(tset=tset1, m=as.numeric(SB.m0), parms=Parms1)
# output part 1:
out1 <- with(Rpar.SBdyn1, {
  ode(y=m,times=tset,func=SimpleBoxODE,parms=parms,method="lsode")
})

# list of input parameters part 2:
Parms2 <- list(e=emis2, K=as.matrix(SB.K))
Rpar.SBdyn2 <- list(tset=tset2, m=as.numeric(c(out1[length(out1[,1]),2:length(out1[1,])])),
                    parms=Parms2)
# output part 2:
out2 <- with(Rpar.SBdyn2, {
  ode(y=m,times=tset,func=SimpleBoxODE,parms=parms,method="lsode")
})


Mt <- rbind (out1,out2[-1,])
colnames(Mt)<-c("Time",SB.names)
R.Minf <- -as.vector(solve(SB.K,tol=1e-30)%*%as.numeric(SB.e))

R.Mt.fracMinf <- cbind(Mt[,1],t(t(Mt[,2:c(1+length(SB.e))])/R.Minf))
colnames(R.Mt.fracMinf)<-c("Time",SB.names)
names(R.Minf)<-SB.names

d <- melt(as.data.frame(R.Mt.fracMinf), id.vars="Time")
d <- d[!is.na(d$value),]


p <- ggplot(d, aes(Time,value, col=variable)) +
  geom_line()
p + theme(legend.position = "none") # plot without legend because it is too large.


##Optimization
ObjFunc<-function(parms){
out<-lsoda(y=m,times=tset,func=SimpleBoxODE,parms=parms)
mse<-
return(mse)
}

fit.p<-optim(parms,ObjFunc)
p1<-fit.p



