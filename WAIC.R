library(loo)
M = 20000 #length of chain (no. of iterations)

#Need to split coda chain into loglik and posterior samples
#the coda includes the burn-in

reducedcoda <- read.delim("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/reducedcoda.txt", header=FALSE)
reduced_posteriorsamples <- reducedcoda[25660001:25900000,2]
reduced_loglik <- reducedcoda[1:25660000,2]

reducedchain = matrix(reduced_posteriorsamples,M,12,byrow = F)
#reducedchain = matrix(reducedchain[,2],M,12)

reduced_loglikmat = matrix(reduced_loglik, M, 1283, byrow = F)

#remove burn-in
#reducedchain = reducedchain[10001:20000,]
#reduced_loglikmat = reduced_loglikmat[10001:20000,]

colnames(reducedchain) = c('beta0', 'beta1', 'beta2', 'beta3[1]', 'beta3[2]', 'beta3[3]', 'beta3[4]', 'beta4[1]', 'beta4[2]', 'beta4[3]', 'beta5', 'tau')
names = c('beta0', 'beta1', 'beta2', 'beta3[1]', 'beta3[2]', 'beta3[3]', 'beta3[4]', 'beta4[1]', 'beta4[2]', 'beta4[3]', 'beta5', 'tau')

x11()
par(mfrow=c(6,2))
for (i in 1:12) {
  plot(reducedchain[,i], xlab="", ylab="", main="")
  title(names[i])
}


waic_reduced <- waic(reduced_loglikmat)
waic_reduced #3884.3

fullcoda1000 <- read.delim("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/fullcoda1000.txt", header=FALSE)
full1000_loglik <- fullcoda1000[,2]
full1000_loglikmat = matrix(full1000_loglik, 1000, 30194, byrow = F)
waic_full1000 <- waic(full1000_loglikmat)
waic_full1000 #18656.2

fullcoda3000 <- read.delim("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/fullcoda3000.txt", header=FALSE)
full3000_loglik <- fullcoda3000[,2]
full3000_loglikmat = matrix(full3000_loglik, 3000, 30189, byrow = F)
waic_full3000 <- waic(full3000_loglikmat)
waic_full3000 #18612.0 

reducedcoda3000 <- read.delim("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/reducedcoda3000.txt", header=FALSE)
reduced3000_loglik <- reducedcoda3000[,2]
reduced3000_loglikmat <- matrix(reduced3000_loglik, 3000, 1283, byrow = F)
waic_red3000 <- waic(reduced3000_loglikmat)
waic_red3000 #3880.9

#20000 iterations no interaction term
reducedcodanoint <- read.delim("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/reducedcodanoint.txt", header=FALSE)
reducedcodanoint_loglik <- reducedcodanoint[,2]
reducedcodanoint_loglikmat <- matrix(reducedcodanoint_loglik, 20000, 1283, byrow = F)
waic_rednoint <- waic(reducedcodanoint_loglikmat)
waic_rednoint #3904.1

#Poisson gamma reduced
pgreducedcoda <- read.delim("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/pgreducedcoda.txt", header=FALSE)
pgreducedcoda_loglik <- pgreducedcoda[,2]
pgreduced_loglikmat <- matrix(pgreducedcoda_loglik, 10000, 1283, byrow = F)
waic_pgreduced <- waic(pgreduced_loglikmat)
waic_pgreduced #3882.5
