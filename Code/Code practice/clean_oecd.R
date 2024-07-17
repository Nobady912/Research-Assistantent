# This function downloads and cleans data from the OECD.
# This function was written by Tie for a data cleaning job for Professor Hashmat Khan.
# Tie, 2024/05/18


#################################################################################
#update history
#0.0.1 the construction of the function 
#0.0.2 remove the excel generate process and it will transfer in the time series
#0.0.2 add the section of downloding the new verson OECD package from github
#################################################################################

  #OECD_clean
  clean_oecd <- function(dataset, filter) {
    
    # Install and load necessary packages
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }
    if (!requireNamespace("OECD", quietly = TRUE)) {
      devtools::install_github("expersso/OECD")
    }
    
    require(tidyr)
    require(dplyr)
    require(tsbox)
    require(xts) 
    require(writexl)
    require(devtools)

    
    # Access the data from the OECD
    raw_data <- get_dataset(dataset, filter)
    
    # Select relevant columns
    selection_data <- select(raw_data, ObsValue, REF_AREA, TIME_PERIOD, UNIT_MEASURE, MEASURE)
    
    # transfer the character to numeric
    selection_data$ObsValue <- as.numeric(selection_data$ObsValue)
    
    # Ensure TIME_PERIOD is numeric in the selection_data
    # So its better for next step on create entire time line
    selection_data$TIME_PERIOD <- as.numeric(selection_data$TIME_PERIOD)
    
    # Define a complete range of years and countries
    # generate the all the sequence for the each country
    all_countries <- unique(selection_data$REF_AREA)
    
    #generate the sequece from the smallest to the largest
    all_years <- seq(min(selection_data$TIME_PERIOD, na.rm = TRUE), max(selection_data$TIME_PERIOD, na.rm = TRUE), by = 1)
    
    # Create a full grid of TIME_PERIOD, REF_AREA, MEASURE, and UNIT_MEASURE
    full_grid <- expand.grid(
      TIME_PERIOD = all_years,
      REF_AREA = all_countries,
      MEASURE = unique(selection_data$MEASURE),
      UNIT_MEASURE = unique(selection_data$UNIT_MEASURE)
    )
    
    # Convert TIME_PERIOD to numeric in full_grid to match selection_data
    full_grid$TIME_PERIOD <- as.numeric(full_grid$TIME_PERIOD)
    
    # Merge full grid with actual data, filling in NAs where no data exists
    selection_data <- full_grid %>%
      left_join(selection_data, by = c("TIME_PERIOD", "REF_AREA", "MEASURE", "UNIT_MEASURE"))
    
    # Pivot data to wider format
    wide_data <- selection_data %>%
      pivot_wider(
        names_from = c(MEASURE, UNIT_MEASURE),
        values_from = ObsValue,
        names_sep = "_"
      )
    
    # Return the cleaned and widened data
    return(wide_data)
}

