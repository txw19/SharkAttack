# rm(list=ls())
# library(R2jags)
library(arm)
library(R2WinBUGS)
library(plyr)

dat <- read.csv('countries.csv')
head(dat)
dim(dat)
length(unique(dat$COUNTRY))
range(dat$YEAR)

dat <- dat[dat$YEAR < 2015,]

dat$ratio <- dat$ATTACKS/dat$POP

# plot(ratio~YEAR, data=dat, subset=COUNTRY=='Australia')


# This should equal dim(dat) above
length(unique(dat$COUNTRY)) * length(unique(dat$YEAR))


#################################################################
########## BUGS CODE ############################################
#################################################################

# Define the model in the BUGS language and write a text file
sink("model.txt")
cat("
model {


### Area-specific Model 2
for (i in 1:N) {
	for (t in 1:T) {
# Logistic part
    W3[i,t] ~ dbern(psi.min3[i])
		y2[i,t] ~ dpois(temp2[i,t])
   temp2[i,t] <- W3[i,t] * temp2a[i,t]
   log(temp2a[i,t]) <- max(-20, min(20, eta.mu3[i,t]))
	 eta.mu3[i,t] <- 1 * offset[i,t] + u[i] + xi[i,t]

	}
# area-specific trends
	xi[i,1:T] ~ car.normal(adj.tm[],weights.tm[],num.tm[],prec.xi[i])
# area-specific intercepts 
	# u[i] ~ dnorm(0, prec.eta)
# area-specific intercepts (no smoothing)
	u[i] ~ dnorm(0,0.001)

# hierarchical modelling of the local temporal variability
	prec.xi[i] <- pow(var.xi[i],-1)
	var.xi[i] <- exp(log.var.xi[i])
	log.var.xi[i] ~ dnorm(mean.log.var.xi,prec.log.var.xi)
	sigma.xi[i] <- pow(var.xi[i],0.5)
}

# hyper priors
# prec.eta <- pow(sigma.eta, -2)
# sigma.eta ~ dunif(0,10)
mean.log.var.xi ~ dnorm(0,0.001)
prec.log.var.xi <- pow(var.log.var.xi,-1)
var.log.var.xi <- pow(sd.log.var.xi,2)
#sd.log.var.xi ~ dunif(0,5)
sd.log.var.xi ~ dnorm(0,prec.sd.log.var.xi)I(0,)
sd.sd.log.var.xi <- 2.5
prec.sd.log.var.xi <- pow(sd.sd.log.var.xi,-2)


#####################
# Prior on logit prob of false zero
for(i in 1:N){
    gamma3[i] ~ dnorm(0, 0.001)
  
    # Derived quantities
  
    psi.min3[i] <- min(0.99999, max(0.00001,(1 - psi3[i])))
    # model the probability of a false zero. if gamma3 is a large negative value then the prob of false zero is 
    # very low and the model reverts back to a Poisson model
    eta.psi3[i] <- gamma3[i]  
    logit(psi3[i]) <- max(-20, min(20, eta.psi3[i]))   
}
#####################


  # Specify weight matrix and adjacency matrix corresponding to RW(1) prior 

             for(t in 1:1) {
                weights.tm[t] <- 1;
               adj.tm[t] <- t+1;
               num.tm[t] <- 1
             }
             for(t in 2:(T-1)) {
                weights.tm[2+(t-2)*2] <- 1;
               adj.tm[2+(t-2)*2] <- t-1
                weights.tm[3+(t-2)*2] <- 1;
               adj.tm[3+(t-2)*2] <- t+1;
               num.tm[t] <- 2
             }
             for(t in T:T) {
                weights.tm[(T-2)*2 + 2] <- 1;
               adj.tm[(T-2)*2 + 2] <- t-1;
               num.tm[t] <- 1
             }



} # end model
",fill = TRUE)
sink()

########################
# Sort
dat <- dat[order(dat$COUNTRY),]
dat$country2 <- as.numeric(dat$COUNTRY)

# plot(ratio~Year, data=dat, subset=state2==2)


# Number of states
N <- length(unique(dat$COUNTRY))
N

T <- length(unique(dat$YEAR))
T


N*T

y <- dat$ATTACKS

# Page 103 - Bayesian modeling using WinBUGS
Y <- matrix(y, c(N,T), byrow=T)
#write.csv(Y, 'Y.csv')


y.s <- structure(
	.Data=c(Y),
	.Dim=c(N,T)
	)


dim(y.s)
head(y.s)

### Create offset
offset <- dat$POP
E1 <- matrix(offset, c(N,T), byrow=T)
E2 <- structure(
  .Data=c(E1),
  .Dim=c(N,T)
)

offset2 <- log(E2)

# Initial values
# Need to supply a vector of W's of zero's and ones
w1 <- dat$ATTACKS
w1b <- matrix(w1, c(N,T), byrow=T)
W1 <- structure(
  .Data=c(w1b),
  .Dim=c(N,T)
)
W1c <- structure(
  .Data=c(w1b),
  .Dim=c(N,T)
)
W1[W1c > 0] <- 1



# Load data
data <- list(y2=y.s, T = T, N = N, offset = offset2, W3 = W1)


# Initial values
inits <- function (){
  list (log.var.xi=rnorm(N), u=rnorm(N,-15,1),gamma3=rnorm(N) )
}


# Parameters monitored
parameters <- c('u','xi','gamma3')


# MCMC settings
ni <- 80000
nt <- 3
nb <- 50000
nc <- 3



bugs.dir <- "C:/Program Files/WinBUGS14/"
source('bugsParallel.R')

start.time = Sys.time()         # Set timer 
# Call BUGS from R 

out <- bugs(data = data, inits = inits, parameters.to.save = parameters, 
            model.file = "model.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb,debug = F, bugs.directory=bugs.dir)

# out <- bugsParallel(data=data, inits=inits, parameters.to.save = parameters, model.file = "model.txt", n.chains = nc, 
#                     n.iter = ni, n.burnin = nb, n.thin = nt, program="WinBUGS", 
#                     bugs.directory = bugs.dir, n.proc = nc)

# 
end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# Calculate computation time

# Summarize posteriors
print(out, dig = 3)

sum1 <- out$summary
# write.csv(sum1,'zipout3.csv')

# Find which parameters, if any, have Rhat > 1.1
which(out$summary[, c("Rhat")] > 1.1)

# Or see what max Rhat value is
max(out$summary[, c("Rhat")])

str(out)

# Save BUGS output
saveRDS(out, file="country.bugs.rds")
saveRDS(dat, file="country.dat.rds")


# out.country <- readRDS("country.rds")

# write.csv(z.vals,'z_clus_1.csv',row.names=F)

