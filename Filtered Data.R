# Remove rows where year of DateOfClaim is before value in Year column
filtered_data2011 <- CMI2011data[CMI2011data$DateOfClaim < as.Date(paste(CMI2011data$Year, '-01-01', sep = '')), ]

# Filter rows where year of DateOfClaim matches value in Year column
filtered_data_with_year_match <- CMI2011data[format(CMI2011data$DateOfClaim, "%Y") == as.character(CMI2011data$Year), ]


filtered_data2011 <- subset(CMI2011data, 
                        format(DateOfClaim, "%Y") >= as.character(Year) | is.na(DateOfClaim))
filtered_data2012 <- subset(CMI2012data, 
                        format(DateOfClaim, "%Y") >= 2011 | is.na(DateOfClaim))
CMI2013data$DateOfClaim <- as.Date(CMI2013data$DateOfClaim)
CMI2014data$DateOfClaim <- as.Date(CMI2014data$DateOfClaim)
CMI2015data$DateOfClaim <- as.Date(CMI2015data$DateOfClaim)
CMI2016data$DateOfClaim <- as.Date(CMI2016data$DateOfClaim)
CMI2017data$DateOfClaim <- as.Date(CMI2017data$DateOfClaim)
CMI2018data$DateOfClaim <- as.Date(CMI2018data$DateOfClaim)
filtered_data2013 <- subset(CMI2013data, 
                        format(DateOfClaim, "%Y") >= 2011 | is.na(DateOfClaim))
filtered_data2014 <- subset(CMI2014data, 
                            format(DateOfClaim, "%Y") >= 2011 | is.na(DateOfClaim))
filtered_data2015 <- subset(CMI2015data, 
                            format(DateOfClaim, "%Y") >= 2011 | is.na(DateOfClaim))
filtered_data2016 <- subset(CMI2016data, 
                            format(DateOfClaim, "%Y") >= 2011 | is.na(DateOfClaim))
filtered_data2017 <- subset(CMI2017data, 
                            format(DateOfClaim, "%Y") >= 2011 | is.na(DateOfClaim))
filtered_data2018 <- subset(CMI2018data, 
                            format(DateOfClaim, "%Y") >= 2011 | is.na(DateOfClaim))


# List of filtered dataset names
filtered_dataset_names <- c("filtered_data2011", "filtered_data2012", "filtered_data2013", 
                            "filtered_data2014", "filtered_data2015", "filtered_data2016", 
                            "filtered_data2017", "filtered_data2018")

# Create an empty dataframe to store results
results <- data.frame(
  Dataset = character(),
  EntriesWithDateOfClaimAndMissingDateOfSettlement = integer(),
  EntriesWithDateOfSettlementandMissingDateOfClaim = integer(),
  TotalMatchingEntries = integer(),
  Proportion = numeric()
)

# Loop through each filtered dataset
for (dataset_name in filtered_dataset_names) {
  # Load filtered dataset
  filtered_data <- get(dataset_name)
  
  # Count entries where DateOfClaim is not missing and DateOfSettlement is missing
  entries_with_date_claim_missing_settlement <- sum(!is.na(filtered_data$DateOfClaim) & is.na(filtered_data$DateOfSettlement))
  
  # Count entries where Date of Settlement is not missing and DateOfClaim is missing
  entries_with_date_settlement_missing_claim <- sum(!is.na(filtered_data$DateOfSettlement) & is.na(filtered_data$DateOfClaim))
  
  # Total number of entries with both date of claim and date of settlement
  total_entries <- sum(!is.na(filtered_data$DateOfClaim) & !is.na(filtered_data$DateOfSettlement))
  
  
  # Append results to dataframe
  results <- rbind(results, data.frame(
    Dataset = dataset_name,
    EntriesWithDateOfClaimAndMissingDateOfSettlement = entries_with_date_claim_missing_settlement,
    EntriesWithDateOfSettlementAndMissingDateOfClaim = entries_with_date_settlement_missing_claim,
    TotalMatchingEntries = total_entries))
}

# Print the results
print(results)

# List of filtered datasets
datasets <- c("filtered_data2011", "filtered_data2012", "filtered_data2013", "filtered_data2014", "filtered_data2015", "filtered_data2016", "filtered_data2017", "filtered_data2018")

# Loop through each dataset and save as CSV
for (dataset in datasets) {
  # Get the dataset object
  data <- get(dataset)
  
  # Generate file name
  filename <- paste0(dataset, ".csv")
  
  # Save dataset as CSV
  write.csv(data, file = filename, row.names = FALSE)
}

# Create an empty list to store proportions tables
proportions_tables <- list()

# Loop through each dataset and calculate proportions
for (dataset in datasets) {
  # Get the dataset object
  data <- get(dataset)
  
  # Filter out rows where CICauseOfClaim is missing
  filtered_data <- data %>% filter(!is.na(CICauseOfClaim))
  
  # Calculate proportions of each entry in CICauseOfClaim
  proportions_table <- filtered_data %>%
    group_by(CICauseOfClaim) %>%
    summarise(proportion = n() / nrow(filtered_data))
  
  # Store the proportions table in the list
  proportions_tables[[dataset]] <- proportions_table
}

# Now you can access each proportions table by dataset name
# For example:
print(proportions_tables[["filtered_data2011"]])

# Filter out rows with missing dates
filtered_data2011bothdates <- filtered_data2011[complete.cases(filtered_data2011$DateOfClaim, filtered_data2011$DateOfSettlement), ]

# Calculate the proportion of entries where the year of claim and settlement match
proportion_same_year <- sum(format(filtered_data2011bothdates$DateOfClaim, "%Y") == format(filtered_data2011bothdates$DateOfSettlement, "%Y")) / nrow(filtered_data2011bothdates)

# List to store proportions
proportions_list <- list()

# Loop through filtered_data2012 to filtered_data2018
for (year in 2012:2018) {
  # Subset the data for the current year
  current_data <- get(paste0("filtered_data", year))
  
  # Filter out rows with missing dates
  current_data_both_dates <- current_data[complete.cases(current_data$DateOfClaim, current_data$DateOfSettlement), ]
  
  # Calculate the proportion of entries where the year of claim and settlement match
  proportion_same_year <- sum(format(current_data_both_dates$DateOfClaim, "%Y") == format(current_data_both_dates$DateOfSettlement, "%Y")) / nrow(current_data_both_dates)
  
  # Store the proportion in the list
  proportions_list[[paste0("filtered_data", year)]] <- proportion_same_year
}

# Display the proportions
proportions_list
