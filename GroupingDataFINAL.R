x<- final_data2011
#x<-subset(x, x$Exposure != 0)
# Create a new variable to categorize durations
x$Duration <- cut(x$Duration, 
                  breaks = c(-1, 0, 4, Inf), 
                  labels = c("0", "1-4", "5+"), 
                  include.lowest = TRUE)

# Change unknown smoker to status to non-smoker
x$SmokerStatus[x$SmokerStatus == "U"] <- "N"

x$D_channel_num <- as.numeric(ifelse(x$DistributionChannel == 'B', 1,
                                            ifelse(x$DistributionChannel == 'I', 2,
                                                   ifelse(x$DistributionChannel == 'M', 3,
                                                          ifelse(x$DistributionChannel == 'N', 4,
                                                                 ifelse(x$DistributionChannel == 'S', 5,
                                                                        ifelse(x$DistributionChannel == 'U', 6, 'NA')))))))


x$SmokerStatus_num <- as.numeric(ifelse(x$SmokerStatus == 'N', 0,
                                               ifelse(x$SmokerStatus == 'S', 1, 'NA')))
x$Duration_grp_num <- as.numeric(ifelse(x$Duration == '0', 1,
                                        ifelse(x$Duration == '1-4', 2,
                                               ifelse(x$Duration == '5+', 3, 'NA'))))

x$ProductType_num <- as.numeric(ifelse(x$ProductType == 'T_D', 1,
                                              ifelse(x$ProductType == 'T_FIB', 2,
                                                     ifelse(x$ProductType == 'T_I', 3,
                                                            ifelse(x$ProductType == 'T_L', 4, 'NA')))))

x$Gender_num <- as.numeric(ifelse(x$Gender == 'M', 0, 1))

x$Sum_num <- as.numeric(ifelse(x$BenefitAmountEndOfYear == '£0-£25,000', 1,
                                      ifelse(x$BenefitAmountEndOfYear == '£125,001+', 4,
                                             ifelse(x$BenefitAmountEndOfYear == '£25,001-£75,000', 2,
                                                    ifelse(x$BenefitAmountEndOfYear == '£75,001-£125,000', 3, 'NA')))))


subset2011 <- subset(x, select = c(ProductType_num, Gender_num, SmokerStatus_num, Age, Age.std, D_channel_num, Duration_grp_num, Sum_num, Exposure, IncurredClaims))

subset2011 <- subset2011 %>% 
  group_by(ProductType_num, Age, Age.std, SmokerStatus_num, Duration_grp_num, D_channel_num, Sum_num, Gender_num) %>%
  summarize(IncurredClaims = sum(IncurredClaims), Exposure = sum(Exposure), n = n()) 
#subset2011[is.na(subset2011)] <- 0
subset2011$Exposure <- subset2011$Exposure/365

subset2011 <- na.omit(subset2011)
subset2011 <- subset(subset2011, subset2011$Exposure != 0)
#subset2011$Exposure <- subset2011$Exposure/365

total_population <- sum(subset2011$n)
subset2011$crudeRate <- subset2011$IncurredClaims/subset2011$n

Exposure <- subset2011$Exposure
IncurredClaims <- as.integer(subset2011$IncurredClaims)
Age <- subset2011$Age.std
SmokerStatus <- as.integer(subset2011$SmokerStatus_num)
Duration <- as.integer(subset2011$Duration_grp_num)
D_channel <- as.integer(subset2011$D_channel_num)
Product <- as.integer(subset2011$ProductType_num)
BenefitAmount <- as.integer(subset2011$Sum_num)
Gender <- as.integer(subset2011$Gender_num)

data <- list('Exposure', 'IncurredClaims', 'Age', 'SmokerStatus', 'Duration', 'D_channel', 'Product', 'BenefitAmount', 'Gender')
initial <- function(){
  list(beta0 = 0,
       beta1 = 0,
       beta2 = 0,
       beta3 = c(0,0,NA),
       beta4 = c(0,0,0,0,0,NA),
       beta5 = c(0,0,0,0,NA),
       beta6 = c(0,0,0,NA),
       beta7 = 0,
       beta8 = 0,
       tau = 1)
}
initialpoisson <- function(){
  list(beta0 = 0,
       beta1 = 0,
       beta2 = 0,
       beta3 = c(0,0,NA),
       beta4 = c(0,0,0,0,0,NA),
       beta5 = c(0,0,0,0,NA),
       beta6 = c(0,0,0,NA),
       beta7 = 0)
}

# Create the initial values structure
initial <- function(){
  list(theta = rep(0.5, 29204),
    beta0 = coefficients_pois["(Intercept)"],
    beta1 = coefficients_pois["Age.std"],
    beta2 = coefficients_pois["SmokerStatusS"],
    beta3 = c(coefficients_pois["Duration0"], coefficients_pois["Duration1-4"], coefficients_pois["Duration5+"]),  # Adjust based on your actual structure
    beta4 = c(coefficients_pois["DistributionChannelB"],
      coefficients_pois["DistributionChannelI"],
      coefficients_pois["DistributionChannelM"],
      coefficients_pois["DistributionChannelN"],
      coefficients_pois["DistributionChannelS"],
      coefficients_pois["DistributionChannelU"]
    ),
    beta5 = c(
      coefficients_pois["ProductType1"],
      coefficients_pois["ProductType2"],
      coefficients_pois["ProductType3"],
      coefficients_pois["ProductType4"]
    ),
    beta6 = c(coefficients_pois["BenefitAmountEndOfYear£0-£25,000"],
      coefficients_pois["BenefitAmountEndOfYear£25,001-£75,000"],
      coefficients_pois["BenefitAmountEndOfYear£75,001-£125,000"],
      coefficients_pois["BenefitAmountEndOfYear£125,001+"]
    ),
    beta7 = coefficients_pois["GenderF"],
    beta8 = coefficients_pois["Age.std:SmokerStatusS"],
    tau = 1
  )
}

# Create the initial values structure
initialpoisson <- function(){
  list(
    beta0 = coefficients_pois["(Intercept)"],
    beta1 = coefficients_pois["Age.std"],
    beta2 = coefficients_pois["SmokerStatusS"],
    beta3 = c(coefficients_pois["Duration0"], coefficients_pois["Duration1-4"], coefficients_pois["Duration5+"]),  # Adjust based on your actual structure
    beta4 = c(coefficients_pois["DistributionChannelB"],
              coefficients_pois["DistributionChannelI"],
              coefficients_pois["DistributionChannelM"],
              coefficients_pois["DistributionChannelN"],
              coefficients_pois["DistributionChannelS"],
              coefficients_pois["DistributionChannelU"]
    ),
    beta5 = c(
      coefficients_pois["ProductTypeT_D"],
      coefficients_pois["ProductTypeT_FIB"],
      coefficients_pois["ProductTypeT_I"],
      coefficients_pois["ProductTypeT_L"]
    ),
    beta6 = c(coefficients_pois["BenefitAmountEndOfYear£0-£25,000"],
              coefficients_pois["BenefitAmountEndOfYear£25,001-£75,000"],
              coefficients_pois["BenefitAmountEndOfYear£75,001-£125,000"],
              coefficients_pois["BenefitAmountEndOfYear£125,001+"]
    ),
    beta7 = coefficients_pois["GenderF"],
    beta8 = coefficients_pois["Age.std:SmokerStatusS"]
  )
}

library(R2OpenBUGS)
bugs.data(data, dir = "C:/Users/louth/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA", data.file = "data.txt")
bugs.data(initial(), dir ="C:/Users/louth/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA", data.file = "initialpl.txt")
