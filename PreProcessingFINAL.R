library(dplyr)
library(lubridate)

CMI2011data <- read.csv("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA/OneDrive_1_16-10-2023/CMI CI term assurances 2011 data/CMI CI term assurances Males 2011 data.csv")
CMI2012data <- read.csv("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA/OneDrive_1_16-10-2023/CMI CI term assurances 2012 data/CMI CI term assurances 2013 data.csv")
CMI2013data <- read.csv("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA/OneDrive_1_16-10-2023/CMI CI term assurances 2013 data/CMI CI term assurances 2014 data.csv")
CMI2014data <- read.csv("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA/OneDrive_1_16-10-2023/CMI CI term assurances 2014 data/CMI CI term assurances 2015 data.csv")
CMI2015data <- read.csv("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA/OneDrive_1_16-10-2023/CMI CI term assurances 2015 data/CMI CI term assurances 2016 data.csv")
CMI2016data <- read.csv("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA/OneDrive_1_16-10-2023/CMI CI term assurances 2016 data/CMI CI term assurances 2017 data.csv")
CMI2017data <- read.csv("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA/OneDrive_1_16-10-2023/CMI CI term assurances 2017 data/CMI CI term assurances 2018 data.csv")
CMI2018data <- read.csv("~/OneDrive/Documents/MAC-MIGS Year 2/PhD Project/THE DATA/OneDrive_1_16-10-2023/CMI CI term assurances 2018 data/CMI CI term assurances 2018 data.csv")

#Remove observation number column
data2011 <- CMI2011data[,2:20]
data2012 <- CMI2012data[,2:20]
data2013 <- CMI2013data[,2:20]
data2014 <- CMI2014data[,2:20]
data2015 <- CMI2015data[,2:20]
data2016 <- CMI2016data[,2:20]
data2017 <- CMI2017data[,2:20]
data2018 <- CMI2018data[,2:20]

replace_NA_with_U <- function(dataset) {
  columns_to_replace <- c(17, 18, 19)  # Adjust column indices as needed
  
  dataset <- dataset %>%
    mutate(across(all_of(columns_to_replace), ~ ifelse(is.na(.), 'U', .)))
  
  dataset <- dataset %>%
    mutate(across(all_of(columns_to_replace), ~ ifelse(trimws(.) == '""""', 'U', .)))
  
  return(dataset)
}

data2011 <- replace_NA_with_U(data2011)
data2012 <- replace_NA_with_U(data2012)
data2013 <- replace_NA_with_U(data2013)
data2014 <- replace_NA_with_U(data2014)
data2015 <- replace_NA_with_U(data2015)
data2016 <- replace_NA_with_U(data2016)
data2017 <- replace_NA_with_U(data2017)
data2018 <- replace_NA_with_U(data2018)

# List of dataset names
dataset_names <- c("data2011", "data2012", "data2013", "data2014", "data2015", "data2016", "data2017", "data2018")

# Define a function to calculate age
calculate_age <- function(dataset) {
  birth_years <- year(dataset$DateOfBirth)
  birth_months <- month(dataset$DateOfBirth)
  birth_days <- day(dataset$DateOfBirth)
  
  age <- numeric(nrow(dataset))
  
  for (i in 1:nrow(dataset)) {
    if (is.na(dataset$DateOfClaim[i])) {
      year_of_dataset <- dataset$Year[i]
      
      age[i] <- year_of_dataset - birth_years[i] - ifelse(birth_months[i] > 1 | 
                                                            (birth_months[i] == 1 & birth_days[i] > 1), 1, 0)
    } else {
      claim_year <- year(dataset$DateOfClaim[i])
      claim_month <- month(dataset$DateOfClaim[i])
      claim_day <- day(dataset$DateOfClaim[i])
      
      age_claim <- claim_year - birth_years[i] - ifelse(birth_months[i] > claim_month | 
                                                          (birth_months[i] == claim_month & birth_days[i] > claim_day), 1, 0)
      
      age[i] <- age_claim
    }
  }
  
  dataset$Age <- age
  
  # Standardize age
  age_mean <- mean(dataset$Age)
  age_sd <- sd(dataset$Age)
  dataset$Age.std <- (dataset$Age - age_mean) / age_sd
  
  return(dataset)
}

# Iterate through each dataset
for (dataset_name in dataset_names) {
  # Calculate age
  assign(dataset_name, calculate_age(get(dataset_name)))
}

# Define a function to add IncurredClaims column
add_IncurredClaims <- function(dataset) {
  dataset$IncurredClaims <- ifelse(!is.na(dataset$DateOfClaim), 1, 0)
  return(dataset)
}

# Apply the function to each dataset
data2011 <- add_IncurredClaims(data2011)
data2012 <- add_IncurredClaims(data2012)
data2013 <- add_IncurredClaims(data2013)
data2014 <- add_IncurredClaims(data2014)
data2015 <- add_IncurredClaims(data2015)
data2016 <- add_IncurredClaims(data2016)
data2017 <- add_IncurredClaims(data2017)
data2018 <- add_IncurredClaims(data2018)

# Define a function to replace BenefitAmountEndOfYear
replace_benefit_amount <- function(dataset) {
  dataset$BenefitAmountEndOfYear <- cut(dataset$BenefitAmountEndOfYear, 
                                        breaks = c(0, 25000, 75000, 125000, Inf),
                                        labels = c("£0-£25,000", "£25,001-£75,000", "£75,001-£125,000", "£125,001+"),
                                        include.lowest = TRUE, 
                                        right = TRUE)
  return(dataset)
}

# Apply the function to all datasets
data2011 <- replace_benefit_amount(data2011)
data2012 <- replace_benefit_amount(data2012)
data2013 <- replace_benefit_amount(data2013)
data2014 <- replace_benefit_amount(data2014)
data2015 <- replace_benefit_amount(data2015)
data2016 <- replace_benefit_amount(data2016)
data2017 <- replace_benefit_amount(data2017)
data2018 <- replace_benefit_amount(data2018)

calculate_duration <- function(dataset) {
  dataset$Duration <- dataset$Year[1] - year(dataset$DateOfBenefitCommencement)
  return(dataset)
}

for (dataset_name in dataset_names) {
  # Calculate duration
  assign(dataset_name, calculate_duration(get(dataset_name)))
}

# 
# # Define a function to categorize conditions
# categorise_conditions <- function(dataset) {
#   dataset <- dataset %>%
#     mutate(
#       Category = case_when(
#         grepl("Malignant", CICauseOfClaim, ignore.case = TRUE) ~ "Cancers",
#         grepl("Hodgkin's", CICauseOfClaim, ignore.case = TRUE) ~ "Cancers",
#         grepl("Leukaemia", CICauseOfClaim, ignore.case = TRUE) ~ "Other",
#         CICauseOfClaim == "Angioplasty" ~ "Other",
#         grepl("cancer", CICauseOfClaim, ignore.case = TRUE) ~ "Cancers",
#         CICauseOfClaim == 'Total Permanent Disability (TPD)' ~ "Terminal Illness or Disability",
#         CICauseOfClaim == 'Terminal Illness' ~ "Terminal Illness or Disability",
#         CICauseOfClaim == 'Paralysis / Paraplegia' ~ "Loss / Paralysis of Limb",
#         CICauseOfClaim == 'Loss of limbs' ~ "Loss / Paralysis of Limb",
#         CICauseOfClaim == 'Parkinsons Disease' ~ "Parkinson's Disease",
#         CICauseOfClaim == 'Alzheimers Disease' ~ "Alzheimer's Disease",
# 
#         TRUE ~ as.character(CICauseOfClaim)  # Keep other categories as is
#       )
#     )
# 
#   return(dataset)
# }

# # Apply the function to all datasets
# for (year in 2011:2018) {
#   dataset_name <- paste0("data", year)
#   assign(dataset_name, categorise_conditions(get(dataset_name)))
# }

# Define a function to categorize the CauseOfClaim variable
categorize_cause_of_claim <- function(dataset) {
  dataset$Category <- ifelse(is.na(dataset$CICauseOfClaim), NA,
                             ifelse(dataset$CICauseOfClaim %in% c(
    "Cancer - site not specified", "Benign Brain Tumour", "Malignant neoplasm of brain",
    "Malignant neoplasm of colon", "Malignant neoplasm of female breast", 
    "Malignant neoplasm of kidney and other urinary organs", 
    "Malignant neoplasm of other sites", 
    "Malignant neoplasm of ovary and uterine adnexa", 
    "Malignant neoplasm of prostate", 
    "Malignant neoplasm of testis", 
    "Malignant neoplasm of trachea, bronchus and lung"
  ), "Cancers", 
  ifelse(dataset$CICauseOfClaim %in% "Heart Attack", "HA",
         ifelse(dataset$CICauseOfClaim %in% "Stroke", "Stroke",
                ifelse(dataset$CICauseOfClaim %in% "Deaths", "Death",
                       ifelse(dataset$CICauseOfClaim %in% "Coronary Artery Bypass Graft (CABG)", "CABG",
                              ifelse(dataset$CICauseOfClaim %in% "Total Permanent Disability (TPD)", "TPD",
                                     ifelse(dataset$CICauseOfClaim %in% "Unknown", 'Unknown',
                                            ifelse(dataset$CICauseOfClaim %in% "Kidney Failure", "KF",
                                                   ifelse(dataset$CICauseOfClaim %in% "Major Organ Transplant (MOT)", "MOT",
                                                          ifelse(dataset$CICauseOfClaim %in% "Multiple Sclerosis", "MS", "Other")))))))))))
  
  return(dataset)
}

# Apply the function to each dataset
for (dataset_name in dataset_names) {
  assign(dataset_name, categorize_cause_of_claim(get(dataset_name)))
}


# Iterate over the dataset names
for (i in seq_along(dataset_names)) {
  # Access the dataset by its name and apply the transformation
  assign(dataset_names[i], get(dataset_names[i]) %>%
           mutate(CICauseOfClaim = ifelse(!is.na(DateOfClaim) & is.na(CICauseOfClaim), "Unknown", CICauseOfClaim))
  )
}


# Define a loop to add columns for all corresponding ages in the dataset 
for (dataset_name in dataset_names) {
  
  # Load the dataset
  dataset <- get(dataset_name)
  
  # Add columns '15' to '85' filled with zeros
  dataset[, as.character(seq(15, 90))] <- 0
  
  # Save the updated dataset
  assign(dataset_name, dataset)
}

#Change 'Year' to correspond with the year in date of claim 
for (dataset_name in dataset_names) {
  
  # Load the dataset
  dataset <- get(dataset_name)
  
  # Convert DateOfClaim to Date format
  dataset$DateOfClaim <- as.Date(dataset$DateOfClaim, format="%Y-%m-%d")
  
  # Update Year column based on DateOfClaim
  dataset$Year[!is.na(dataset$DateOfClaim)] <- as.integer(format(dataset$DateOfClaim[!is.na(dataset$DateOfClaim)], "%Y"))
  
  # Save the updated dataset
  assign(dataset_name, dataset)
}

#Calculate how many claims are settled in the same year and the corresponding proportions out of total claims 
for (dataset_name in dataset_names) {
  
  # Load the dataset
  dataset <- get(dataset_name)
  
  # Convert DateOfClaim and DateOfSettlement to Date format
  dataset$DateOfClaim <- as.Date(dataset$DateOfClaim, format="%Y-%m-%d")
  dataset$DateOfSettlement <- as.Date(dataset$DateOfSettlement, format="%Y-%m-%d")
  
  # Count the number of claims settled in the same year
  claims_same_year <- sum(!is.na(dataset$DateOfSettlement) & !is.na(dataset$DateOfClaim) & as.integer(format(dataset$DateOfSettlement, "%Y")) == as.integer(format(dataset$DateOfClaim, "%Y")))
  
  # Count the total number of claims
  total_claims <- sum(!is.na(dataset$DateOfClaim))
  
  # Calculate the proportion of claims settled in the same year
  proportion_same_year <- claims_same_year / total_claims
  
  # Print the results
  cat(sprintf("Number of claims settled in the same year in %s: %d\n", dataset_name, claims_same_year))
  cat(sprintf("Total number of claims in %s: %d\n", dataset_name, total_claims))
  cat(sprintf("Proportion of claims settled in the same year in %s: %.2f%%\n", dataset_name, proportion_same_year * 100))
  
  # Save the updated dataset
  assign(dataset_name, dataset)
}

#Create a StartOfYear column
for (dataset_name in dataset_names) {
  
  # Load the dataset
  dataset <- get(dataset_name)
  
  # Create StartOfYear column
  dataset$StartOfYear <- as.Date(ISOdate(dataset$Year, 1, 1))
  
  # Save the updated dataset
  assign(dataset_name, dataset)
}

#Create an EndOfYear column
for (dataset_name in dataset_names) {
  
  # Load the dataset
  dataset <- get(dataset_name)
  
  # Create EndOfYear column
  dataset$EndOfYear <- as.Date(ISOdate(dataset$Year, 12, 31))
  
  # Save the updated dataset
  assign(dataset_name, dataset)
}

#data2011 exposure only
data2011EXPOSURES <- data2011
data2011EXPOSURES$DaysFromStartOfYearToBirthday <- as.numeric(difftime(as.Date(ISOdate(data2011EXPOSURES$Year, month(data2011EXPOSURES$DateOfBirth), day(data2011EXPOSURES$DateOfBirth))), data2011EXPOSURES$StartOfYear, units = "days"))
data2011EXPOSURES$DaysFromBirthdayToEndOfYear <- as.numeric(difftime(data2011EXPOSURES$EndOfYear, as.Date(ISOdate(data2011EXPOSURES$Year, month(data2011EXPOSURES$DateOfBirth), day(data2011EXPOSURES$DateOfBirth))), units = "days"))
data2011EXPOSURES$DaysToClaim <- as.numeric(difftime(data2011EXPOSURES$DateOfClaim, data2011EXPOSURES$StartOfYear, units = "days"))
data2011EXPOSURES$DaysFromClaimToBirthday <- as.numeric(difftime(as.Date(ISOdate(data2011EXPOSURES$Year, month(data2011EXPOSURES$DateOfBirth), day(data2011EXPOSURES$DateOfBirth))), data2011EXPOSURES$DateOfClaim, units = "days"))
has_claim <- any(!is.na(data2011EXPOSURES$DateOfClaim))

#this code works but takes ages to run
for (i in 1:nrow(data2011EXPOSURES)) {
  has_claim <- !is.na(data2011EXPOSURES$DateOfClaim[i])
  
  if (!has_claim) {
    age_col_name <- as.character(data2011EXPOSURES$Age[i])
    next_age <- as.character(as.numeric(age_col_name) + 1)
    
    exposure_days_birth <- data2011EXPOSURES$DaysFromStartOfYearToBirthday[i]
    exposure_days_birth_next_age <- data2011EXPOSURES$DaysFromBirthdayToEndOfYear[i]
    
    data2011EXPOSURES[i, age_col_name] <- exposure_days_birth
    data2011EXPOSURES[i, next_age] <- exposure_days_birth_next_age
  } else {
    exposure_days_birth <- data2011EXPOSURES$DaysFromStartOfYearToBirthday[i]
    exposure_days_claim <- data2011EXPOSURES$DaysToClaim[i]
    birthday_to_claim <- data2011EXPOSURES$DaysFromClaimToBirthday[i]
    
    data2011EXPOSURES[i, age_col_name] <- ifelse(birthday_to_claim > 0, exposure_days_birth, 0)
    data2011EXPOSURES[i, next_age] <- ifelse(birthday_to_claim < 0, exposure_days_claim, 0)
  }
}

#try vectorising instead
# Identify rows without claims
no_claim_rows <- is.na(data2011EXPOSURES$DateOfClaim)
claim_rows <- !is.na(data2011EXPOSURES$DateOfClaim)

age_values <- as.character(15:90)
age_col_indices <- which(names(data2011EXPOSURES) %in% age_values)
age_col_names <- names(data2011EXPOSURES)[age_col_indices]
#next_age_indices <- which(names(data2011EXPOSURES) %in% age_values+1)
next_age_values <- as.character(as.numeric(age_values) + 1)
next_age_indices <- which(names(data2011EXPOSURES) %in% next_age_values)
next_age_col_names <- names(data2011EXPOSURES)[next_age_indices]

# For rows with claims
#age_col_indices_claims <- which(names(data2011EXPOSURES) %in% as.character(data2011EXPOSURES$Age[claim_rows]))
#next_age_indices_claims <- which(names(data2011EXPOSURES) %in% as.character(as.numeric(data2011EXPOSURES$Age[claim_rows]) + 1))

# Create a vector to store exposure values
exposure_days_birth <- data2011EXPOSURES$DaysFromStartOfYearToBirthday[no_claim_rows]
exposure_days_birth_next_age <- data2011EXPOSURES$DaysFromBirthdayToEndOfYear[no_claim_rows]
claim_days <- data2011EXPOSURES$DaysToClaim[claim_rows]
claim_days_tobirth <- data2011EXPOSURES$DaysFromClaimToBirthday[claim_rows]
claim_days_birthday <- data2011EXPOSURES$DaysFromStartOfYearToBirthday[claim_rows]

# Select positive entries
positive_entries <- claim_days_tobirth[claim_days_tobirth > 0]
# Select negative entries
negative_entries <- claim_days_tobirth[claim_days_tobirth < 0]


# Calculate and store exposures for rows with positive claim_days_tobirth
for (i in seq_along(claim_days_tobirth)) {
  claim_row_index <- which(claim_rows)[i]
  
  if (claim_days_tobirth[i] >= 0) {
    # Positive value: calculate exposure up to DateOfClaim and store in corresponding age
    exposure_value <- claim_days[i]
    
    # Store exposure in the corresponding age column
    age_column <- as.character(data2011EXPOSURES$Age[claim_row_index])
    data2011EXPOSURES[claim_row_index, age_column] <- exposure_value
    
  } else {
    # Negative value: calculate exposure up to birthday and from birthday up to DateOfClaim
    exposure_up_to_birthday <- min(claim_days_birthday[i], claim_days_tobirth[i] + claim_days[i])
    exposure_from_birthday <- max(0, abs(claim_days_tobirth[i]))
    
    # Store exposures in the corresponding age and next age columns
    age_column <- as.character(data2011EXPOSURES$Age[claim_row_index])
    next_age_column <- as.character(data2011EXPOSURES$Age[claim_row_index] + 1)
    
    data2011EXPOSURES[claim_row_index, age_column] <- exposure_up_to_birthday
    data2011EXPOSURES[claim_row_index, next_age_column] <- exposure_from_birthday
  }
}

data2011claims <- data2011EXPOSURES[claim_rows,]

# Use conditional statements to assign values to the correct columns
for (i in seq_along(age_col_names)) {
    age_col_index <- age_col_names[i]
   
    data2011EXPOSURES[no_claim_rows, as.character(age_col_index)] <- ifelse(data2011EXPOSURES$Age[no_claim_rows] == age_col_index, exposure_days_birth, 0)
}

for (i in seq_along(next_age_col_names)) {
  next_age_index <- next_age_col_names[i]
  
  current_column <- as.character(next_age_index)
  
  data2011EXPOSURES[no_claim_rows, current_column] <- ifelse(
    data2011EXPOSURES$Age[no_claim_rows] == as.numeric(next_age_index) - 1,
    exposure_days_birth_next_age,
    data2011EXPOSURES[no_claim_rows, current_column]  # Keep existing values if the condition is not met
  )
}

# Duplicate each row and add 1 to the 'Age' column for duplicates
result_data2011 <- data2011EXPOSURES[rep(seq_len(nrow(data2011EXPOSURES)), each = 2), ]
result_data2011$Age <- rep(data2011EXPOSURES$Age, each = 2) + c(0, 1)

# Create a new column 'Exposure' by matching 'Age' with the corresponding column
result_data2011 <- result_data2011 %>%
  rowwise() %>%
  mutate(Exposure = get(as.character(Age)))

#result_data2011 <- result_data2011[, -c(25:100)]

#result_data2011 <- result_data2011 %>%
#  mutate(Exposure = ifelse(Exposure == 0 & DaysToClaim != 0, DaysToClaim, Exposure))
# Set IncurredClaims to 0 based on specified conditions
result_data2011$IncurredClaims[which(result_data2011$DateOfBirth > result_data2011$DateOfClaim)[seq(1, length.out = nrow(result_data2011), by = 2)]] <- 0
result_data2011$IncurredClaims[which(result_data2011$DateOfBirth <= result_data2011$DateOfClaim)[seq(2, length.out = nrow(result_data2011), by = 2)]] <- 0


checkingexposures <- subset(result_data2011, select = c(DateOfBirth, DateOfBenefitCommencement, DateOfClaim, DateOfNotification, DateOfAdmission, DateOfSettlement, Age, DaysFromStartOfYearToBirthday, DaysFromBirthdayToEndOfYear, DaysToClaim, DaysFromClaimToBirthday, Exposure, StartOfYear, EndOfYear))
final_data2011 <- subset(result_data2011, select = c(Exposure, Office, Gender, SmokerStatus, BenefitType, ProductType, DistributionChannel, SingleJointLife, BenefitAmountEndOfYear, Category, NationalIMDDecile, Age, Age.std, IncurredClaims, Duration, DateOfClaim, DateOfBirth, DateOfSettlement, DateOfNotification))
age_mean <- mean(final_data2011$Age)
age_sd <- sd(final_data2011$Age)
final_data2011$Age.std <- (final_data2011$Age - age_mean) / age_sd

# Function to calculate exposure in days
calculate_exposure <- function(dataset) {
  
  # Convert DateOfBirth and DateOfClaim to Date format
  dataset$DateOfBirth <- as.Date(dataset$DateOfBirth, format="%Y-%m-%d")
  dataset$DateOfClaim <- as.Date(dataset$DateOfClaim, format="%Y-%m-%d")
  
  # Extract the year from the dataset name
  dataset_year <- as.numeric(gsub("dataset", "", substr(deparse(substitute(dataset)), 1, 9)))
  
  # Loop through each row
  for (i in 1:nrow(dataset)) {
    
    # If DateOfClaim is NA, calculate exposure based on DateOfBirth
    if (is.na(dataset$DateOfClaim[i])) {
      # Count days from start of the year to DateOfBirth
      exposure_days <- as.numeric(difftime(dataset$DateOfBirth[i], as.Date(paste(dataset$Year[1], "-01-01"), "%Y-%m-%d")))
      # Store the exposure in the corresponding age column
      dataset[i, as.character(seq(15, 85))] <- exposure_days
    } else {
      # Count days from start of the year to DateOfBirth
      exposure_days_birth <- as.numeric(difftime(dataset$DateOfBirth[i], as.Date(paste(dataset$Year[1], "-01-01"), "%Y-%m-%d")))
      # Count days from start of the year to DateOfClaim
      exposure_days_claim <- as.numeric(difftime(dataset$DateOfClaim[i], as.Date(paste(dataset$Year[1], "-01-01"), "%Y-%m-%d")))
      
      # If DateOfClaim comes after the birthday, store in two corresponding age columns
      if (any(exposure_days_birth < exposure_days_claim)) {
        dataset[i, as.character(seq(15, 85))] <- c(exposure_days_birth, exposure_days_claim)
      } else {
        # Store only in the corresponding age column (DateOfClaim is not considered)
        dataset[i, as.character(seq(15, 85))] <- exposure_days_birth
      }
    }
  }
  
  return(dataset)
}

# Loop through each dataset
for (dataset_name in datasets) {
  
  # Load the dataset
  dataset <- get(dataset_name)
  
  # Calculate exposure in days for each entry
  dataset <- calculate_exposure(dataset)
  
  # Save the updated dataset
  assign(dataset_name, dataset)
}


calculate_exposure <- function(dataset) {
  dataset$Exposure <- as.numeric(difftime(dataset$DateOfClaim, dataset$DateOfBenefitCommencement, units = "days"))
  return(dataset)
}

# Iterate through each dataset
for (dataset_name in dataset_names) {
  # Calculate exposure
  assign(dataset_name, calculate_exposure(get(dataset_name)))
}

# Iterate through each dataset
for (dataset_name in dataset_names) {
  # Replace NAs with 365 in Exposure column
  assign(dataset_name, transform(get(dataset_name), Exposure = ifelse(is.na(Exposure), 365, Exposure)))
}

# Combine datasets
combined_data <- rbind(data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018)

# Check proportions of Durations
duration_proportions <- prop.table(table(combined_data$Duration)) * 100

# Create a new variable to categorize durations
combined_data$Duration_Category <- cut(combined_data$Duration, 
                                       breaks = c(-1, 0, 4, Inf), 
                                       labels = c("0", "1-4", "5+"), 
                                       include.lowest = TRUE)

# Calculate proportions and raw counts for Duration Category
duration_table <- table(combined_data$Duration_Category)
duration_proportions <- prop.table(duration_table) * 100
duration_counts <- as.vector(duration_table)
duration_data <- data.frame(Category = names(duration_table), Proportion = duration_proportions, Count = duration_counts)

# Calculate proportions and raw counts for Product Type
product_type_table <- table(combined_data$ProductType)
product_type_proportions <- prop.table(product_type_table) * 100
product_type_counts <- as.vector(product_type_table)
product_type_data <- data.frame(Type = names(product_type_table), Proportion = product_type_proportions, Count = product_type_counts)

# Calculate proportions and raw counts for Gender
gender_table <- table(combined_data$Gender)
gender_proportions <- prop.table(gender_table) * 100
gender_counts <- as.vector(gender_table)
gender_data <- data.frame(Gender = names(gender_table), Proportion = gender_proportions, Count = gender_counts)

# Calculate proportions and raw counts for Distribution Channel
distribution_channel_table <- table(combined_data$DistributionChannel)
distribution_channel_proportions <- prop.table(distribution_channel_table) * 100
distribution_channel_counts <- as.vector(distribution_channel_table)
distribution_channel_data <- data.frame(Channel = names(distribution_channel_table), Proportion = distribution_channel_proportions, Count = distribution_channel_counts)

# Calculate proportions and raw counts for Single/Joint Life
single_joint_life_table <- table(combined_data$SingleJointLife)
single_joint_life_proportions <- prop.table(single_joint_life_table) * 100
single_joint_life_counts <- as.vector(single_joint_life_table)
single_joint_life_data <- data.frame(LifeType = names(single_joint_life_table), Proportion = single_joint_life_proportions, Count = single_joint_life_counts)

# Calculate proportions and raw counts for Benefit Type
benefit_type_table <- table(combined_data$BenefitType)
benefit_type_proportions <- prop.table(benefit_type_table) * 100
benefit_type_counts <- as.vector(benefit_type_table)
benefit_type_data <- data.frame(BenefitType = names(benefit_type_table), Proportion = benefit_type_proportions, Count = benefit_type_counts)

# Calculate proportions and raw counts for Office
office_table <- table(combined_data$Office)
office_proportions <- prop.table(office_table) * 100
office_counts <- as.vector(office_table)
office_data <- data.frame(Office = names(office_table), Proportion = office_proportions, Count = office_counts)

# Calculate proportions and raw counts for Smoker Status
smoker_status_table <- table(combined_data$SmokerStatus)
smoker_status_proportions <- prop.table(smoker_status_table) * 100
smoker_status_counts <- as.vector(smoker_status_table)
smoker_status_data <- data.frame(SmokerStatus = names(smoker_status_table), Proportion = smoker_status_proportions, Count = smoker_status_counts)

# Calculate proportions and raw counts for National IMD
nationalIMD_table <- table(combined_data$NationalIMDDecile)
nationalIMD_proportions <- prop.table(nationalIMD_table) * 100
nationalIMD_counts <- as.vector(nationalIMD_table)
nationalIMD_data <- data.frame(Decile = names(nationalIMD_table), Proportion = nationalIMD_proportions, Count = nationalIMD_counts)

# Calculate proportions and raw counts for UK IMD
UK_IMD_table <- table(combined_data$UKIMDDecile)
UK_IMD_proportions <- prop.table(UK_IMD_table) * 100
UK_IMD_counts <- as.vector(UK_IMD_table)
UK_IMD_data <- data.frame(Decile = names(UK_IMD_table), Proportion = UK_IMD_proportions, Count = UK_IMD_counts)

# Calculate proportions and raw counts for Region
region_table <- table(combined_data$Region)
region_proportions <- prop.table(region_table) * 100
region_counts <- as.vector(region_table)
region_data <- data.frame(Region = names(region_table), Proportion = region_proportions, Count = region_counts)

# Calculate proportions and raw counts for Cause (with non-NA)
cause_table_nonna <- table(combined_data_no_na$Category)
cause_proportions_nonna <- prop.table(cause_table_nonna) * 100
cause_counts_nonna <- as.vector(cause_table_nonna)
cause_data_nonna <- data.frame(Category = names(cause_table_nonna), Proportion = cause_proportions_nonna, Count = cause_counts_nonna)

# Calculate proportions and raw counts for Benefit Amount
benefit_amount_table <- table(combined_data$BenefitAmountEndOfYear)
benefit_amount_proportions <- prop.table(benefit_amount_table) * 100
benefit_amount_counts <- as.vector(benefit_amount_table)
benefit_amount_data <- data.frame(BenefitAmount = names(benefit_amount_table), Proportion = benefit_amount_proportions, Count = benefit_amount_counts)


combined_data$Sum_num <- as.numeric(ifelse(combined_data$BenefitAmountEndOfYear == '£0-£25,000', 1,
                                      ifelse(combined_data$BenefitAmountEndOfYear == '£125,001+', 4,
                                             ifelse(combined_data$BenefitAmountEndOfYear == '£25,001-£75,000', 2,
                                                    ifelse(combined_data$BenefitAmountEndOfYear == '£75,001-£125,000', 3, 'NA')))))

# Create a table of counts for each category
sum_counts <- prop.table(table(combined_data$Sum_num))*100

# Remove NA values from Category column
combined_data_no_na <- combined_data[!is.na(combined_data$Category),]

# Calculate proportions
cause_proportionsnonna <- prop.table(table(combined_data_no_na$Category)) * 100

labels <- c('CABG', 'Cancers','Death', 'HA', 'KF', 'MOT', 'MS', 'Other', 'Stroke', 'TPD', 'Unknown')
pie(cause_proportionsnonna, labels = labels, col = colors, main = "Proportions of Cause of Claim", cex=0.6)

# Define the labels and colors
labels <- c('£0-£25,000', '£125,001+', '£25,001-£75,000', '£75,001-£125,000')
pastel_palette <- brewer.pal(11, "Spectral")
palette(pastel_palette)
colors <- pastel_palette[1:length(labels)]

# Create a pie chart with labels and colors
pie(sum_counts, labels = labels, col = colors,
    main = "Proportions of Benefit Amount Categories")

# Create a legend with formatted labels
legend("topleft", legend = labels, fill = colors, cex = 0.8)

# Define a list of proportions and corresponding category names
proportions_list <- list(
  Policy_Duration = duration_proportions,
  Product_Type = product_type_proportions,
  Gender = gender_proportions,
  Distribution_Channel = distribution_channel_proportions,
  Joint_Single_Life = single_joint_life_proportions,
  Benefit_Type = benefit_type_proportions,
  Office = office_proportions,
  Smoker_Status = smoker_status_proportions,
  National_IMD = nationalIMD_proportions,
  UK_IMD = UK_IMD_proportions,
  Region = region_proportions,
  Cause = cause_proportions,
  Benefit_Amount = benefit_proportions,
  Claim_Causes = cause_proportionsnonna
)

# Use "Pastel1" color palette from RColorBrewer
pastel_palette <- brewer.pal(11, "Set3")

# Create pie charts with proportions as labels
for (prop_name in names(proportions_list)) {
  # Set up plotting space
  par(mfrow=c(1,1))
  
  # Create a pie chart with proportions as labels and pastel colors
  pie(
    proportions_list[[prop_name]], 
    labels = paste0(round(proportions_list[[prop_name]], 1), "%"),
    col = pastel_palette,
    main = paste("Proportions of", prop_name),
    cex = 0.8
  )
  
  # Add a legend with category names
  legend("topleft", legend = names(proportions_list[[prop_name]]), fill = pastel_palette, cex = 0.5)
  
  # Reset graphical parameters
  par(mfrow=c(1,1))
}


library(ggplot2)

#For Age
ggplot(combined_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(combined_data$Age), max(combined_data$Age), by = 5)) +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Frequency")


# For BenefitAmountEndOfYear
ggplot(combined_data, aes(x = BenefitAmountEndOfYear)) +
  geom_histogram(binwidth = 100000, fill = "green", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(combined_data$BenefitAmountEndOfYear), max(combined_data$BenefitAmountEndOfYear), by = 100000)) +
  labs(title = "Distribution of Benefit Amount at End of Year",
       x = "Benefit Amount at End of Year",
       y = "Frequency")



# Create a logical condition for in-force data
condition2011 <- is.na(data2011$DateOfClaim) & is.na(data2011$DateOfNotification) & is.na(data2011$DateOfAdmission) & is.na(data2011$DateOfSettlement) & data2011$CICauseOfClaim == "NA"
condition2012 <- is.na(data2012$DateOfClaim) & is.na(data2012$DateOfNotification) & is.na(data2012$DateOfAdmission) & is.na(data2012$DateOfSettlement) & data2012$CICauseOfClaim == "NA"
condition2013 <- is.na(data2013$DateOfClaim) & is.na(data2013$DateOfNotification) & is.na(data2013$DateOfAdmission) & is.na(data2013$DateOfSettlement) & data2013$CICauseOfClaim == "NA"
condition2014 <- is.na(data2014$DateOfClaim) & is.na(data2014$DateOfNotification) & is.na(data2014$DateOfAdmission) & is.na(data2014$DateOfSettlement) & data2014$CICauseOfClaim == "NA"
condition2015 <- is.na(data2015$DateOfClaim) & is.na(data2015$DateOfNotification) & is.na(data2015$DateOfAdmission) & is.na(data2015$DateOfSettlement) & data2015$CICauseOfClaim == "NA"
condition2016 <- is.na(data2016$DateOfClaim) & is.na(data2016$DateOfNotification) & is.na(data2016$DateOfAdmission) & is.na(data2016$DateOfSettlement) & data2016$CICauseOfClaim == "NA"
condition2017 <- is.na(data2017$DateOfClaim) & is.na(data2017$DateOfNotification) & is.na(data2017$DateOfAdmission) & is.na(data2017$DateOfSettlement) & data2017$CICauseOfClaim == "NA"
condition2018 <- is.na(data2018$DateOfClaim) & is.na(data2018$DateOfNotification) & is.na(data2018$DateOfAdmission) & is.na(data2018$DateOfSettlement) & data2018$CICauseOfClaim == "NA"


# Use the condition to subset the dataset
inforce2011 <- data2011[condition2011, ]
outforce2011 <- data2011[!condition2011, ]
inforce2012 <- data2012[condition2012, ]
outforce2012 <- data2012[!condition2012, ]
inforce2013 <- data2013[condition2013, ]
outforce2013 <- data2013[!condition2013, ]
inforce2014 <- data2014[condition2014, ]
outforce2014 <- data2014[!condition2014, ]
inforce2015 <- data2015[condition2015, ]
outforce2015 <- data2015[!condition2015, ]
inforce2016 <- data2016[condition2016, ]
outforce2016 <- data2016[!condition2016, ]
inforce2017 <- data2017[condition2017, ]
outforce2017 <- data2017[!condition2017, ]
inforce2018 <- data2018[condition2018, ]
outforce2018 <- data2018[!condition2018, ]

# Create nonmissing datasets for each year
nomissing2011 <- outforce2011[complete.cases(outforce2011), ]
nomissing2012 <- outforce2012[complete.cases(outforce2012), ]
nomissing2013 <- outforce2013[complete.cases(outforce2013), ]
nomissing2014 <- outforce2014[complete.cases(outforce2014), ]
nomissing2015 <- outforce2015[complete.cases(outforce2015), ]
nomissing2016 <- outforce2016[complete.cases(outforce2016), ]
nomissing2017 <- outforce2017[complete.cases(outforce2017), ]
nomissing2018 <- outforce2018[complete.cases(outforce2018), ]

dataset_years <- c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')

# Initialize an empty data frame to store the combined data
combined_nomissing <- data.frame()

# Loop through the datasets and combine them
for (year in dataset_years) {
  current_nomissing <- get(paste0("nomissing", year))
  combined_nomissing <- rbind(combined_nomissing, current_nomissing)
}

# Get unique entries and their counts
unique_entries <- table(combined_nomissing$Category)

# Calculate total number of observations
total_observations <- nrow(combined_nomissing)

# Calculate proportions
proportions <- prop.table(unique_entries)

# Print the proportions
print(proportions*100)
sum(proportions)

library(lubridate)

# Iterate through each dataset
for (dataset_name in dataset_names) {
  # Extract year from DateOfClaim
  assign(dataset_name, mutate(get(dataset_name), 
                              YearOfClaim = year(DateOfClaim),
                              YearOfBenefitCommencement = year(DateOfBenefitCommencement)))
}

# Create an empty list to store the counts
counts <- list()

# Iterate through each dataset
for (dataset_name in dataset_names) {
  # Count observations for each year
  counts[[dataset_name]] <- table(get(dataset_name)$YearOfClaim)
}

# Print the counts
for (i in seq_along(dataset_names)) {
  cat("Counts for", dataset_names[i], ":\n")
  print(counts[[i]])
  cat("\n")
}

# Create an empty list to store the proportions
proportions <- list()

# Iterate through each dataset
for (dataset_name in dataset_names) {
  # Count observations for each year
  counts <- table(get(dataset_name)$YearOfClaim)
  
  # Calculate proportions
  proportions[[dataset_name]] <- prop.table(counts) * 100  # Multiply by 100 to get percentage
}

# Print the proportions
for (i in seq_along(dataset_names)) {
  cat("Proportions for", dataset_names[i], ":\n")
  print(proportions[[i]])
  cat("\n")
}


# Calculate the total number of entries with DateOfClaim and Cause
total_entries_with_claim <- sum(!is.na(combined_data$DateOfClaim))
total_entries_with_cause <- sum(!is.na(combined_data$CICauseOfClaim))

# Calculate the proportion
proportion_cause <- total_entries_with_cause / total_entries_with_claim
proportion_cause*100

# Calculate the number of entries with both DateOfClaim and CICauseOfClaim
entries_with_both <- nrow(combined_data[!is.na(combined_data$DateOfClaim) & combined_data$CICauseOfClaim != 'Unknown', ])

# Calculate the proportion
proportion_cause1 <- entries_with_both / total_entries_with_claim
proportion_cause1*100

entries_with_cause_no_date <- subset(combined_data, !is.na(CICauseOfClaim) & is.na(DateOfClaim))

(entries_with_both / nrow(combined_data)) * 100

cause_proportions_subset <- prop.table(table(entries_with_cause_no_date$Category)) * 100

# Number of 'NA' values in each column for the subset
na_count_category_subset <- sum(is.na(entries_with_cause_no_date$Category))
na_count_date_of_settlement_subset <- sum(is.na(entries_with_cause_no_date$DateOfSettlement) & is.na(entries_with_cause_no_date$DateOfNotification))
na_count_date_of_notification_subset <- sum(is.na(entries_with_cause_no_date$DateOfNotification))
na_count_date_of_admission_subset <- sum(is.na(entries_with_cause_no_date$DateOfAdmission))
na_count_date_of_beencommencement_subset <- sum(is.na(entries_with_cause_no_date$DateOfBenefitCommencement))

entries_with_cause_no_date$DateOfBenefitCommencement <- as.Date(entries_with_cause_no_date$DateOfBenefitCommencement)

# Create a histogram with manual breaks for years
breaks <- seq(as.Date("2010-01-01"), as.Date("2019-01-01"), by = "year")
hist(entries_with_cause_no_date$DateOfSettlement, breaks = "months", col = "blue", xlab = "Date", ylab = "Frequency", main = "Time Series Distribution of Date Of Settlement", xaxt = 'n')
axis(1, at = breaks, labels = format(breaks, "%Y"))

hist(entries_with_cause_no_date$DateOfNotification, breaks = "months", col = "blue", xlab = "Date", ylab = "Frequency", main = "Time Series Distribution of Date Of Notification", xaxt = 'n')
axis(1, at = breaks, labels = format(breaks, "%Y"))

hist(entries_with_cause_no_date$DateOfAdmission, breaks = "months", col = "blue", xlab = "Date", ylab = "Frequency", main = "Time Series Distribution of Date Of Admission", xaxt = 'n')
axis(1, at = breaks, labels = format(breaks, "%Y"))

breaksyears <- seq(as.Date("1990-01-01"), as.Date("2019-01-01"), by = "year")
hist(entries_with_cause_no_date$DateOfBenefitCommencement, breaks = "years", col = "blue", xlab = "Date", ylab = "Frequency", main = "Time Series Distribution of Date Of Benefit Commencement", xaxt = 'n')
axis(1, at = breaksyears, labels = format(breaksyears, "%Y"))

# Define a function to generate plots for a given dataset
generate_plots <- function(dataset, year) {
  dataset$DateOfClaim <- as.Date(dataset$DateOfClaim)
  dataset$DateOfSettlement <- as.Date(dataset$DateOfSettlement)
  dataset$DateOfNotification <- as.Date(dataset$DateOfNotification)
  dataset$DateOfAdmission <- as.Date(dataset$DateOfAdmission)
  dataset$TimeDifferenceYears <- as.numeric(difftime(dataset$DateOfSettlement, dataset$DateOfClaim, units = "days")) / 30
  
  claim_plot <- ggplot(dataset, aes(x = DateOfClaim)) +
    geom_histogram(binwidth = 30, fill = "blue", color = "white") +
    labs(title = paste("Time Series Distribution for Dates of Claim -", year), x = "Date of Claim") +
    scale_x_date(limits = as.Date(c("2010-01-01", "2018-12-31")), date_labels = "%Y-%m-%d")
  
  notif_plot <- ggplot(dataset, aes(x = DateOfNotification)) +
    geom_histogram(binwidth = 30, fill = "blue", color = "white") +
    labs(title = paste("Time Series Distribution for Dates of Notification -", year), x = "Date of Notification") +
    scale_x_date(limits = as.Date(c("2010-01-01", "2018-12-31")), date_labels = "%Y-%m-%d")

  admission_plot <- ggplot(dataset, aes(x = DateOfAdmission)) +
    geom_histogram(binwidth = 30, fill = "blue", color = "white") +
    labs(title = paste("Time Series Distribution for Dates of Admission -", year), x = "Date of Admission") +
    scale_x_date(limits = as.Date(c("2010-01-01", "2018-12-31")), date_labels = "%Y-%m-%d")
  
  settlement_plot <- ggplot(dataset, aes(x = DateOfSettlement)) +
    geom_histogram(binwidth = 30, fill = "blue", color = "white") +
    labs(title = paste("Time Series Distribution for Dates of Settlement -", year), x = "Date of Settlement") +
    scale_x_date(limits = as.Date(c("2010-01-01", "2018-12-31")), date_labels = "%Y-%m-%d")
  
  
  delay_plot <- ggplot(dataset, aes(x = TimeDifferenceYears)) +
    geom_histogram(binwidth = 1, fill = "red", color = "white") +
    labs(title = paste("Time Series Distribution for Delay Between Claim and Settlement -", year), x = "Time Difference (months)") +
    scale_x_continuous(limits = c(0, 50)) 

  
  return(list(claim_plot, delay_plot, notif_plot, admission_plot, settlement_plot))
}

# Generate plots for each dataset
for (i in 2011:2018) {
  dataset_name <- paste0("data", i)
  plots <- generate_plots(get(dataset_name), dataset_name)
  print(plots[[1]])
  print(plots[[2]])
  print(plots[[3]])
  print(plots[[4]])
  print(plots[[5]])
}

chronological_obs <- combined_data %>%
  filter(DateOfClaim <= DateOfNotification &
           DateOfNotification <= DateOfAdmission &
           DateOfAdmission <= DateOfSettlement)

# Get the number of observations with chronological dates
num_chronological_obs <- nrow(chronological_obs)

# Count observations with non-NA DateOfClaim
num_non_na_DateOfClaim <- sum(!is.na(combined_data$DateOfClaim))

# Count observations with non-NA DateOfNotification
num_non_na_DateOfNotification <- sum(!is.na(combined_data$DateOfNotification))

# Count observations with non-NA DateOfAdmission
num_non_na_DateOfAdmission <- sum(!is.na(combined_data$DateOfAdmission))

# Count observations with non-NA DateOfSettlement
num_non_na_DateOfSettlement <- sum(!is.na(combined_data$DateOfSettlement))
