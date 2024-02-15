library(lattice)
library(dplyr)  
library(MASS)
library(lmtest)
library(AER)
library(ggplot2)
library(coefplot)
x1 <- final_data2011

str(x1)

x1$Duration <- ifelse(x1$Duration == 0, '0', ifelse(x1$Duration <= 4, '1-4', '5+'))
# Change unknown smoker to status to non-smoker
x1$SmokerStatus[x1$SmokerStatus == "U"] <- "N"

# Ensure variable types
x1$DistributionChannel <- factor(x1$DistributionChannel, levels = c('B', 'I', 'M', 'N', 'S', 'U'))
x1$Duration <- factor(x1$Duration, levels = c('0', '1-4', '5+'))
x1$ProductType <- as.factor(x1$ProductType)
x1$BenefitAmountEndOfYear <- as.factor(x1$BenefitAmountEndOfYear)
x1$Gender <- as.factor(x1$Gender)
x1$SmokerStatus <- factor(x1$SmokerStatus, levels = c('N', 'S'))

# # Set contrasts for all categorical variables
# contrasts(x1$DistributionChannel) <- contr.sum(6, contrasts = TRUE)
# contrasts(x1$Duration) <- contr.sum(3, contrasts = TRUE)
# contrasts(x1$BenefitAmountEndOfYear) <- contr.sum(4, contrasts = TRUE)
# contrasts(x1$ProductType) <- contr.sum(4, contrasts = TRUE)
# contrasts(x1$Gender) <- contr.sum(2, contrasts = TRUE)  # Assuming Gender is binary
# contrasts(x1$SmokerStatus) <- contr.sum(2, contrasts = TRUE)

# Group by relevant variables and summarize
x1 <- x %>% 
  group_by(ProductType, Age, Age.std, SmokerStatus, Duration, DistributionChannel, BenefitAmountEndOfYear, Gender) %>%
  summarize(IncurredClaims = sum(IncurredClaims), Exposure = sum(Exposure))

# Adjust Exposure to be daily
x1$Exposure <- x1$Exposure / 365

x1 <- x1[x1$Exposure != 0, , drop = FALSE]

x1 <- na.omit(x1)

# Check the structure of the data
str(x1)

x1$DistributionChannel <- relevel(x1$DistributionChannel, ref = "U")
x1$Duration <- relevel(x1$Duration, ref = "5+")
x1$ProductType <- relevel(x1$ProductType, ref="T_L")
x1$BenefitAmountEndOfYear <- relevel(x1$BenefitAmountEndOfYear, ref="£125,001+")
x1$Gender <- relevel(x1$Gender, ref="M")
x1$SmokerStatus <- relevel(x1$SmokerStatus, ref = "N")

summary(x1)
var(x1$IncurredClaims)
mean(x1$IncurredClaims)

attach(x1)

#using gamlss
NBI.reg<- gamlss::gamlss(AdmnNb ~ offset(logExp) +Age.c + REGION + SEX + UR + EECLASS + EESTATU + EMPREL + PLANTYP,
                         data = learn,
                         family = NBI)
#using glm.nb
NBI.reg<- MASS::glm.nb(AdmnNb ~ offset(logExp) + Age.c + REGION + SEX + UR + EECLASS + EESTATU + EMPREL + PLANTYP,
                       data = learn,
                       link=log)


# Fit Poisson log-normal GLM
model_pois <- glm(IncurredClaims ~ Age.std + SmokerStatus + DistributionChannel + Duration + 
               ProductType + BenefitAmountEndOfYear + Gender + Age.std:SmokerStatus + offset(log(Exposure+1e-10)),
             data = x1, family = poisson, maxit = 100)
summary(model_pois)

#quasi-poisson for underdispersed data
model_quasipois <- glm(IncurredClaims ~ Age.std + SmokerStatus + DistributionChannel + Duration + 
                    ProductType + BenefitAmountEndOfYear + Gender + Age.std:SmokerStatus + offset(log(Exposure+1e-10)),
                  data = x1, family = quasipoisson, maxit = 100)
summary(model_quasipois)

#Conway-Maxwell poisson model for underdispersed data
library(COMPoissonReg)
model_cmpois <- glm.cmp(IncurredClaims ~ Age.std + SmokerStatus + DistributionChannel + Duration + 
                          ProductType + BenefitAmountEndOfYear + Gender + Age.std:SmokerStatus + offset(log(Exposure+1e-10)),
                        data = x1)

# Negative binomal GLM
model_nb <- glm.nb(x1$IncurredClaims ~ Age.std + SmokerStatus + DistributionChannel + Duration + 
                     ProductType + BenefitAmountEndOfYear + Gender + offset(log(Exposure+1e-10)) +
                     Age.std:SmokerStatus, data=x1, link = log, epsilon = 1e-08, maxit =100)
summary(model_nb)

model_nbnew <- glm.nb(x1$IncurredClaims ~ Age.std + SmokerStatus + DistributionChannel + Duration + 
                     ProductType + BenefitAmountEndOfYear + Gender + offset(log(Exposure+1e-10)) +
                     Age.std:SmokerStatus, data=x1, link = log, maxit = 100, init.theta = 100)
summary(model_nbnew)

model_nbglm <- glm(IncurredClaims ~ Age.std + SmokerStatus + DistributionChannel + Duration + 
                     ProductType + BenefitAmountEndOfYear + Gender + Age.std:SmokerStatus + offset(log(Exposure+1e-10)),
                   data = x1, family = negative.binomial(theta = 60.7913), maxit = 100)
summary(model_nbglm)

#Coefficients Plot
coefficients_pois <- coef(model_pois)
coefficients_nb <- coef(model_nbnew)
coefplot(model_pois, intercept = FALSE, innerCI =0, lwdOuter = 1)
coefplot(model_nbnew, intercept = FALSE, innerCI =0, lwdOuter =1)


#Pearson and Deviance residuals plot

residualplot1 <- ggplot(aes(x1=Age,y=resid(model_nbnew,type="pear")), data=x1)+ 
  geom_point()+
  geom_hline(yintercept=0)+
  labs(x1="Age", y="Residuals") +
  ggtitle("Pearson residuals for Negative binomial model")
residualplot1

residualplot2 <- ggplot(aes(x1=Age,y=resid(model_nbnew,type="deviance")), data=x1) + 
  geom_point() + 
  geom_hline(yintercept=0)+ 
  labs(x1="Age", y="Residuals") + 
  ggtitle("Deviance residuals for Negative binomial model")
residualplot2

residualplot3 <- ggplot(aes(x1=Age,y=resid(model_pois,type="pear")), data = x1) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  labs(x1 = "Age", y = "Residuals") + 
  ggtitle("Pearson residuals for Poisson model")
residualplot3

residualplot4 <- ggplot(aes(x1 = x1$Age, y = resid(model_pois, type = "deviance")), data = x1) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x1 = "Age", y = "Residuals") +
  ggtitle("Deviance residuals for Poisson model")
residualplot4

#Comparison coefficients plot
z <- cbind(coef(model_pois),coef(model_nbnew))
colnames(z)<-c("Poisson","Negative binomial")
multiplot(model_nbnew, model_pois, names = c("Negative binomial","Poisson"), intercept = FALSE)
multiplot(model_nbnew, model_pois, names = c("Negative binomial","Poisson"), intercept = FALSE, innerCI = 0, lwdOuter = 1)
multiplot(model_nbnew, model_pois, names = c("Negative binomial","Poisson"), coefficients=c("(Intercept)"), outerCI = 0)

x1s <- subset(x1, SmokerStatus == "S" & DistributionChannel == "B" & Duration == "1-4", select = c(IncurredClaims, Age, Exposure))
C <- tapply(x1s$IncurredClaims, x1s$Age, FUN=sum)
E <- tapply(x1s$Exposure, x1s$Age, FUN=sum)
range(x1s$Age)
df <- data.frame(18:68, C, E)
names(df) <- c("Age", "Claims", "Exposure")
df$Obs <- NA
df$Obs <- log((df$Claims + 1e-10) / (df$Exposure + 1e-10))
df

plot(df$Age, df$Obs, x1lab = "Age", ylab = "Logarithmic inception rate",
     main = "Logarithmic inception rate \n for Smoker with policy duration 1-4 and Dist channel B",
     ylim = c(-8, -2))
lines(x1$Age, model_pois$coefficients[1] + model_pois$coefficients[2]*x1$Age.std + model_pois$coefficients[3] + model_pois$coefficients[4] + model_pois$coefficients[10] + model_pois$coefficients[18]*x1$Age.std, col="blue", lwd=2)
lines(x1$Age, model_nbnew$coefficients[1] + model_nbnew$coefficients[2]*x1$Age.std + model_nbnew$coefficients[3] + model_nbnew$coefficients[4] + model_nbnew$coefficients[10] + model_nbnew$coefficients[18]*x1$Age.std, col="red", lty=2, lwd=2)
legend(20, -2.5, legend = c("Poisson", "Negative binomial"), col =c("Blue","Red"),lty=1,cex1=0.8,lwd=c(2,2))

x1n <- subset(x1,SmokerStatus == "N" & DistributionChannel == "B" & Duration == "1-4", select = c(IncurredClaims, Age, Exposure))
Cn <- tapply(x1n$IncurredClaims, x1n$Age, FUN=sum)
En <- tapply(x1n$Exposure, x1n$Age, FUN=sum)
range(x1n$Age)
df2 <- data.frame(18:69,Cn,En)
names(df2) <- c("Age","Claims","Exposure")
df2$Obs <- NA
df2$Obs <- log((df2$Claims + 1e-10) / (df2$Exposure + 1e-10))
df2

plot(df2$Age, df2$Obs, x1lab = "Age", ylab = "Logarithmic inception rate",
     main = "Logarithmic inception rate \n for Non-Smoker with policy duration 1-4 and Dist channel B",
     ylim = c(-9,-2))
lines(x1$Age, model_pois$coefficients[1] + model_pois$coefficients[2]*x1$Age.std + model_pois$coefficients[4] + model_pois$coefficients[10] + model_pois$coefficients[18]*x1$Age.std, col="blue", lwd=2)
lines(x1$Age, model_nbnew$coefficients[1] + model_nbnew$coefficients[2]*x1$Age.std + model_nbnew$coefficients[4] + model_nbnew$coefficients[10] + model_nbnew$coefficients[18]*x1$Age.std, col="red", lty=2, lwd=2)
legend(20, -2, legend = c("Poisson", "Negative binomial"), col =c("Blue","Red"),lty=1,cex1=0.8,lwd=c(2,2))

x15 <- subset(x1,SmokerStatus == "N" & Duration == "5+", select = c(IncurredClaims, Age, Exposure))
C5 <- tapply(x15$IncurredClaims, x15$Age, FUN=sum)
E5 <- tapply(x15$Exposure, x15$Age, FUN=sum)
range(x15$Age)
df5 <- data.frame(20:81,C5,E5)
names(df5) <- c("Age","Claims","Exposure")
df5$Obs <- NA
df5$Obs <- log((df5$Claims + 1e-10) / (df5$Exposure + 1e-10))
df5

x15s <- subset(x1, SmokerStatus == "S" & Duration == "5+", select = c(IncurredClaims, Age, Exposure))
C5s <- tapply(x15s$IncurredClaims, x15s$Age, FUN = sum)
E5s <- tapply(x15s$Exposure, x15s$Age, FUN = sum)
range(x15s$Age)
df5s <- data.frame(23:76,C5s,E5s)
names(df5s) <- c("Age","Claims","Exposure")
df5s$Obs <- NA
df5s$Obs <-log((df5s$Claims +1e-10) / (df5s$Exposure + 1e-10))
df5s

x10 <- subset(x1,SmokerStatus == "N" & Duration == "0", select = c(IncurredClaims, Age, Exposure))
C0 <- tapply(x10$IncurredClaims, x10$Age, FUN=sum)
E0 <- tapply(x10$Exposure, x10$Age, FUN=sum)
range(x10$Age)
df0 <- data.frame(15:69,C0,E0)
names(df0) <- c("Age","Claims","Exposure")
df0$Obs <- NA
df0$Obs <- log((df0$Claims + 1e-10) / (df0$Exposure + 1e-10))
df0

x10s <- subset(x1, SmokerStatus == "S" & Duration == "0", select = c(IncurredClaims, Age, Exposure))
C0s <- tapply(x10s$IncurredClaims, x10s$Age, FUN=sum)
E0s <- tapply(x10s$Exposure, x10s$Age, FUN=sum)
range(x10s$Age)
df0s <- data.frame(17:65, C0s, E0s)
names(df0s) <- c("Age","Claims", "Exposure")
df0s$Obs <- NA
df0s$Obs <- log((df0s$Claims +1e-10) / (df0s$Exposure+1e-10))
df0s

plot(df5$Age, df5$Obs, x1lab = "Age", ylab = "Logarithmic inception rate",
     main = "Logarithmic inception rate \n for Non-Smoker with Dist channel B",
     ylim = c(-10,-3.5))
points(df0$Age, df0$Obs, col = "red")
points(df2$Age, df2$Obs, col = "blue")
legend("bottomright", legend = c("Policy Duration 5+", "Policy Duration 1-4", "Policy Duration 0"), col = c("black", "blue", "red"), pch = 1)
lines(x1$Age, model_pois$coefficients[1] + model_pois$coefficients[2]*x1$Age.std + model_pois$coefficients[4] + model_pois$coefficients[18]*x1$Age.std, col="black", lwd=2)
lines(x1$Age, model_pois$coefficients[1] + model_pois$coefficients[2]*x1$Age.std + model_pois$coefficients[4] + model_pois$coefficients[9] +  model_pois$coefficients[18]*x1$Age.std, col="red", lwd=2)
lines(x1$Age, model_pois$coefficients[1] + model_pois$coefficients[2]*x1$Age.std + model_pois$coefficients[4] + model_pois$coefficients[10] + model_pois$coefficients[18]*x1$Age.std, col="blue", lwd=2)

plot(df5s$Age, df5s$Obs, x1lab = "Age", ylab = "Logarithmic inception rate",
     main = "Logarithmic inception rate \n for Smoker with Dist channel B",
     ylim = c(-10,-3), x1lim = c(20,70))
points(df0s$Age, df0s$Obs, col = "red")
points(df$Age, df$Obs, col = "blue")
legend("bottomright", legend = c("Policy Duration 5+", "Policy Duration 1-4", "Policy Duration 0"), col = c("black", "blue", "red"), pch = 1)
lines(x1$Age, model_pois$coefficients[1] + model_pois$coefficients[2]*x1$Age.std + model_pois$coefficients[3] + model_pois$coefficients[4] + model_pois$coefficients[18]*x1$Age.std, col="black", lwd=2)
lines(x1$Age, model_pois$coefficients[1] + model_pois$coefficients[2]*x1$Age.std + model_pois$coefficients[3] + model_pois$coefficients[4] + model_pois$coefficients[9] +  model_pois$coefficients[18]*x1$Age.std, col="red", lwd=2)
lines(x1$Age, model_pois$coefficients[1] + model_pois$coefficients[2]*x1$Age.std + model_pois$coefficients[3] + model_pois$coefficients[4] + model_pois$coefficients[10] + model_pois$coefficients[18]*x1$Age.std, col="blue", lwd=2)

library(coda)
library(broom.mix1ed)
pois_bayes <- read.coda("Poisson coda.tx1t", "Poisson coda index1.tx1t")

# Ex1tract coefficients
coefficients_bayes <- as.matrix1(pois_bayes)
summary_pois <- summary(pois_bayes)

sumstats_bayes <- as.data.frame(summary_pois$statistics)
quantile_bayes <- as.data.frame(summary_pois$quantiles)

# Combine data frames
combined_data <- cbind(sumstats_bayes, quantile_bayes)
rownames(combined_data) <- c("Intercept", "Age", "Smoker Status", "Duration 0", "Duration 1-4", "D.Channel B", "D.Channel I", "D.Channel M", "D.Channel N", "D.Channel S", "Product T_D", "Product T_FIB", "Product T_I", "Benefit £0-£25000", "Benefit £25001-£75000", "Benefit £75001-$125000", "Gender", "Age:SmokerStatus")

combined_data <- combined_data[rownames(combined_data) != "Intercept", ]

ggplot(combined_data, aes(x1 = Mean, y = row.names(combined_data))) +
  geom_point(stat = "identity", fill = "skyblue", color = "black", size = 3) +
  geom_errorbarh(aes(x1min = `2.5%`, x1max1 = `97.5%`), height = 0.2, color = "black", size = 1) +
  labs(title = "Mean Values of Variables with 95% CI", x1 = "Mean", y = "Variable") +
  theme_minimal() +
  theme(ax1is.tex1t.y = element_tex1t(hjust = 1))
