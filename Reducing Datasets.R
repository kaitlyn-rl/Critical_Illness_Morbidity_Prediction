library(dplyr)

library(readxl)
ACI_Datasheet_2010 <- read_excel("ACI Datasheet 2010.xlsx")

FullACI2010 <- as.data.frame(ACI_Datasheet_2010)

FullACI2010$DistributionChannel[FullACI2010$DistributionChannel == 'M'] <- 'M&S'
FullACI2010$DistributionChannel[FullACI2010$DistributionChannel == 'S'] <- 'M&S'

FullACI2010$D_channel_num <- as.numeric(ifelse(FullACI2010$DistributionChannel == 'B', 1,
                                               ifelse(FullACI2010$DistributionChannel == 'I', 2,
                                                      ifelse(FullACI2010$DistributionChannel == 'M&S', 3,
                                                             ifelse(FullACI2010$DistributionChannel == 'U', 4, 'NA')))))

FullACI2010$SmokerStatus_num <- as.numeric(ifelse(FullACI2010$SmokerStatus == 'N', 0,
                                                  ifelse(FullACI2010$SmokerStatus == 'S', 1, 'NA')))

FullACI2010$Duration_grp_num <- as.numeric(ifelse(FullACI2010$Duration == 0, 1,
                                                  ifelse(FullACI2010$Duration > 4,3,2)))

FullACI2010$ProductCategory_num <- as.numeric(ifelse(FullACI2010$ProductCategory == 'ACI_P_E_X', 1,
                                                     ifelse(FullACI2010$ProductCategory == 'ACI_P_W_X', 2,
                                                            ifelse(FullACI2010$ProductCategory == 'ACI_T_D_X', 3,
                                                                   ifelse(FullACI2010$ProductCategory == 'ACI_T_FIB', 4,
                                                                          ifelse(FullACI2010$ProductCategory == 'ACI_T_I', 5,
                                                                                 ifelse(FullACI2010$ProductCategory == 'ACI_T_L_X', 6,
                                                                                        ifelse(FullACI2010$ProductCategory == 'ACI_T_X', 7, 'NA'))))))))

FullACI2010$Gender_num <- as.numeric(ifelse(FullACI2010$Gender == 'F', 0, 1))

FullACI2010$SumAssuredBand_num <- as.numeric(ifelse(FullACI2010$SumAssuredBand == '£0-£25,000', 1,
                                                    ifelse(FullACI2010$SumAssuredBand == '£125,001+', 4,
                                                           ifelse(FullACI2010$SumAssuredBand == '£25,001-£75,000', 2,
                                                                  ifelse(FullACI2010$SumAssuredBand == '£75,001-£125,000', 3,
                                                                         ifelse(FullACI2010$SumAssuredBand == 'Unknown', 5, 'NA'))))))

age_mean <- mean(FullACI2010$Age)
age_sd <- sd(FullACI2010$Age)
FullACI2010$A.std <- (FullACI2010$Age - age_mean)/age_sd

FullACI2010subset <- subset(FullACI2010, select = -c(ProductCategory, Gender, SmokerStatus, Age, DistributionChannel, Duration, ComparatorTable, SumAssuredBand, AmountsExposure, AmountIncurred, ExpectedClaims, ExpectedAmountClaims))
FullACI2010subset <- FullACI2010subset %>% 
  group_by(ProductCategory_num, A.std, SmokerStatus_num, Duration_grp_num, D_channel_num, SumAssuredBand_num, Gender_num) %>%
  summarize(IncurredClaims = sum(IncurredClaims), LivesExposure = sum(LivesExposure))

LivesExposure <- FullACI2010subset$LivesExposure
IncurredClaims <- FullACI2010subset$IncurredClaims
D_channel <- FullACI2010subset$D_channel_num
SmokerStatus <- FullACI2010subset$SmokerStatus_num
Duration_grp <- FullACI2010subset$Duration_grp_num
Age.c <- FullACI2010subset$A.std
ProductCategory <- FullACI2010subset$ProductCategory_num
Gender <- FullACI2010subset$Gender_num
AssuredBand <- FullACI2010subset$SumAssuredBand_num

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

bugs.data(data,dir="C:/Users/louth/OneDrive/Documents/MAC-MIGS YEAR 2/PhD Project/Variable Selection",digits=8, data.file = "datafull.txt") 
bugs.data(initial(),dir="C:/Users/louth/OneDrive/Documents/MAC-MIGS YEAR 2/PhD Project/Variable Selection",digits=3, data.file = "initialfull.txt")


# Remove the gender variable
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
}

bugs.data(data4,dir="C:/Users/louth/OneDrive/Documents/MAC-MIGS YEAR 2/PhD Project/Variable Selection",digits=8, data.file = "datareduced.txt") 
bugs.data(initial4(),dir="C:/Users/louth/OneDrive/Documents/MAC-MIGS YEAR 2/PhD Project/Variable Selection",digits=3, data.file = "initialreduced.txt")
