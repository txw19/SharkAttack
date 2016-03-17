###############################################################
 # Read in BUGS output
out <- readRDS("country.bugs.rds")
dat <- readRDS("country.dat.rds")


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



# Plot subset of data
N <- 14

# Ration per mill
dat$ratio2 <- dat$ATTACKS/(dat$POP/1000000)

# State names
dat$country2 <- as.numeric(dat$COUNTRY)
state_names <- unique(dat$COUNTRY)

########### PLOT
res <- 6

sitePlot <- c(1:N)

name_figure <- 'shark_bite_country.png'
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
def.par <- par(no.readonly = TRUE)

size.labels = 1
size.text = 1.0
x.label = 'Year'
y.label = 'Shark attacks/million people'

plotYear <- min(dat$YEAR):max(dat$YEAR)


nf <- layout(matrix( c(1:(N)),nrow=7,ncol=2,byrow=T),  TRUE) 
layout.show(nf)
par(mar=c(0.5,0.5,0.5,0.5),oma=c(3,3,0,1),mai=c(0.1,0.3,0.1,0) )	

for(i in 1:N){

plot(dat$YEAR,dat$ratio2, axes=F, ylim=c(0,max(SiteTrendUCI[,i])), ylab='', xlab='', type='n')

i.for <- order( plotYear )
i.back <- order( plotYear , decreasing = TRUE )

x.polygon <- c( plotYear[i.for] , plotYear[i.back] )
y.polygon <- c( SiteTrendLCI[,i][i.for] , SiteTrendUCI[,i][i.back] )

polygon( x.polygon , y.polygon , col = "gray" , border = NA )


# Add fitted lines
points(plotYear,SiteTrend2[,i], cex=0.8, pch=16,type='l',lty=1.5)

# Add data
points(dat$YEAR[dat$country2==i], dat$ratio2[dat$country2==i], pch=16, cex=0.8)

text(1991,max(SiteTrendUCI[,i])-(max(SiteTrendUCI[,i])/6),state_names[i],cex=1.2)

axis(side=1, cex.axis=1, at=plotYear,  tck=-0.01, mgp=c(0,0.2,0) ) 
axis(side=2,cex.axis=1,font=1 ,tck=-0.01, mgp=c(0,0.18,0), las=1) 
mtext(x.label, line = 1, side = 1, cex = size.text, outer=T)
mtext(y.label, line = 0.3, side = 2, cex = size.text, outer=T)
box()
}

par(def.par)
dev.off()


#plot(dat$Year[dat$state2==1], dat$ratio[dat$state2==1], pch=16, cex=0.8)
#plot(dat$Year[dat$state2==9], dat$ratio[dat$state2==9], pch=16, cex=0.8)

# max(SiteTrendUCI[,9])-(max(SiteTrendUCI[,9])/5)

# max(SiteTrendUCI[,9])+1e-06
