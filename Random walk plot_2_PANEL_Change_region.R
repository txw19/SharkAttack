###############################################################
 # Read in BUGS output
out <- readRDS("regions.bugs.rds")
dat <- readRDS("regions.dat.rds")

# Number of regions
N <- length(unique(dat$REGION))
N

T <- length(unique(dat$YEAR))
T
###########################
# SITE-SPECIFIC TRENDS
###########################
# u = site intercept
# xi = site trend

# Site=specific trend
SiteTrend <- array(NA, c(out$n.sims, T,N) )
for(t in 1:T){
	for(n in 1:N){
		SiteTrend[,t,n] <- exp(out$sims.list$xi[,n,t] + out$sims.list$u[,n])
	}
}	


# Per million
SiteTrend <- SiteTrend * 1000000

SiteTrend2 <- matrix(NA, nrow=T, ncol=N)
for(t in 1:T){
	for(n in 1:N){
		SiteTrend2[t,n] <- mean(SiteTrend[,t,n])
	}
}	

head(SiteTrend2)
dim(SiteTrend2)

# Site=specific trend
SiteTrendLCI <- matrix(NA, nrow=T, ncol=N)
for(t in 1:T){
	for(n in 1:N){
		SiteTrendLCI[t,n] <- quantile(SiteTrend[,t,n],0.025)
	}
}	


head(SiteTrendLCI)

SiteTrendUCI <- matrix(NA, nrow=T, ncol=N)
for(t in 1:T){
	for(n in 1:N){
		SiteTrendUCI[t,n] <- quantile(SiteTrend[,t,n],0.975)
	}
}	

##################################################
### Calculate difference (change) among years
##################################################
SiteTrendDelta <- array(NA, c(out$n.sims, T-1,N) )
for(t in 1:(T-1)){
  for(n in 1:N){
    SiteTrendDelta[,t,n] <- SiteTrend[, (t+1) ,n] - SiteTrend[, t, n]
  }
}	

SiteTrendDeltaMean <- matrix(NA, nrow=T, ncol=N)
for(t in 1:T-1){
  for(n in 1:N){
    SiteTrendDeltaMean[t,n] <- mean(SiteTrendDelta[,t,n])
  }
}	

SiteTrendDeltaUCI <- matrix(NA, nrow=T, ncol=N)
for(t in 1:T-1){
  for(n in 1:N){
    SiteTrendDeltaUCI[t,n] <- quantile(SiteTrendDelta[,t,n], 0.975)
  }
}	

SiteTrendDeltaLCI <- matrix(NA, nrow=T, ncol=N)
for(t in 1:T-1){
  for(n in 1:N){
    SiteTrendDeltaLCI[t,n] <- quantile(SiteTrendDelta[,t,n], 0.025)
  }
}	


# SiteTrendDeltaMean[,1]
# SiteTrendDeltaUCI[,1]
# SiteTrendDeltaLCI[,1]
# SiteTrend2[,1]

## Calculate probability of an annual increase from year t to t+1
SiteTrendDeltaMeanProb <- matrix(NA, nrow=T, ncol=N)
for(t in 1:T-1){
  for(n in 1:N){
    SiteTrendDeltaMeanProb[t,n] <- mean(SiteTrendDelta[,t,n] > 0)
  }
}	

# SiteTrendDeltaMeanProb[,1]

dim(SiteTrendDeltaMeanProb)
# Remove NA's
SiteTrendDeltaMeanProb <- SiteTrendDeltaMeanProb[1:45,]

# Plot subset of data
N <- 8

# Ration per mill
# dat$ratio2 <- dat$ATTACKS/(dat$POP/1000000)

# Region names

region_names <- c('Eastern US','Western US','Hawaii',
                  'Eastern Africa','Southwest Africa','Southern Australia','Northern Australia')

########### PLOT
res <- 6

sitePlot <- c(1:N)

name_figure <- 'shark_bite_regions_probs.png'
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1.0
x.label = 'Year'
y.label = 'Probability of annual increase in shark attacks'

plotYear <- min(dat$YEAR):max(dat$YEAR)


nf <- layout(matrix( c(1:(N)),nrow=4,ncol=2,byrow=T),  TRUE) 
layout.show(nf)
par(mar=c(0.5,0.5,0.5,0.5),oma=c(3,3,0,1),mai=c(0.05,0.3,0.1,0) )	

for(i in 1:7){

plot(c(1970:2014),SiteTrendDeltaMeanProb[,i], axes=F, ylim=c(0,1), ylab='', xlab='', type='n')

  if( i <=5){
    axis(side=1,cex.axis=1 , mgp=c(1,0,0),tck= -0.02, at=seq(1970,2015,by=5),labels=F ) 
  } else {
    axis(side=1, cex.axis=1, at=seq(1970,2015,by=5), 
         labels=c('1970', '1975', '1980', '1985', '1990', '1995', '2000', '2005',
                  '2010', '2015'),
         tck=-0.01, mgp=c(1,0,0) ) 
  }
  

# Add fitted lines
points(c(1970:2014),SiteTrendDeltaMeanProb[,i], pch=16,type='l',lty=1.5)

text(1991,0.9,region_names[i],cex=1.2)

#axis(side=1, cex.axis=1, at=plotYear,  tck=-0.01, mgp=c(0,0.2,0) ) 
axis(side=2,cex.axis=1,font=1 ,tck=-0.01, mgp=c(0,0.18,0), las=1) 
mtext(x.label, line = 1, side = 1, cex = size.text, outer=T)
mtext(y.label, line = 0.3, side = 2, cex = size.text, outer=T)
box()
}

par(def.par)
dev.off()



