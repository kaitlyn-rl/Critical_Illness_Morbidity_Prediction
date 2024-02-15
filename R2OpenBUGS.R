#R2OpenBUGs
library(R2OpenBUGS)

#Run 'reducing datasets' script first

#All covars
data<-list('LivesExposure', 'IncurredClaims', 'SmokerStatus', 'Age.c', 'D_channel', 'Duration_grp', 'ProductCategory', 'Gender', 'AssuredBand')
initial<-function(){
  list(theta = rep(0.5, 30194),
       beta0 = 0,
       beta1 = 0,
       beta2 = 0,
       beta3 = 0,
       beta4 = c(0,0,0,NA),
       beta5 = c(0,0,NA),
       beta6 = c(0,0,0,0,0,0,NA),
       beta7 = c(0,0,0,0,NA),
       beta8 = 0,
       tau = 1)
  
}

parameters <- c("theta", "beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7", "beta8", "tau", "LogLik")
model.file <- system.file("~/MAC-MIGS Year 2/PhD Project/ACI full pl.odc", package = "R2OpenBUGS")

pl.full <- bugs(data, initial, parameters, model.file = "~/MAC-MIGS Year 2/PhD Project/ACI full pl.odc", n.chains = 1, n.iter = 100, n.burnin = 100)
  
  bugs(data, inits, parameters, model.file,
       n.chains = 3, n.iter = 1000,
       working.directory = NULL)


FullACI2010subset <- ungroup(FullACI2010subset)
Fourcovars_ACI2010subset <- select(FullACI2010subset, -c(ProductCategory_num, Gender_num, SumAssuredBand_num))

# Group data by remaining variables and summarize
Fourcovars_ACI2010subset <- Fourcovars_ACI2010subset %>%
  group_by(A.std, SmokerStatus_num, D_channel_num, Duration_grp_num) %>%
  summarize(IncurredClaims = sum(IncurredClaims), LivesExposure = sum(LivesExposure))

LivesExposure <- Fourcovars_ACI2010subset$LivesExposure
IncurredClaims <- Fourcovars_ACI2010subset$IncurredClaims
D_channel <- Fourcovars_ACI2010subset$D_channel_num
SmokerStatus <- Fourcovars_ACI2010subset$SmokerStatus_num
Duration_grp <- Fourcovars_ACI2010subset$Duration_grp_num
Age.c <- Fourcovars_ACI2010subset$A.std

data4<-list('LivesExposure', 'IncurredClaims', 'SmokerStatus', 'Age.c', 'D_channel', 'Duration_grp')
initial4<-function(){
  list(theta = rep(0.5, 1283),
       beta0 = 0,
       beta1 = 0,
       beta2 = 0,
       beta3 = c(0,0,0,NA),
       beta4 = c(0,0,NA),
       beta5 = 0,
       tau = 1)
  

