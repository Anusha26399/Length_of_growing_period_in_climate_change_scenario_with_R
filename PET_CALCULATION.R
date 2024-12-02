
----------------------------------------------
#Plotting PETv/s YEAR
  # Load required libraries
library(dplyr)
library(ggplot2)

# Load the data
results <- read.csv("C:/Users/lenovo/Downloads/NAVSARI_pet_multiple_years.csv") 

# Filter the results for the year 2030
pet_2020 <- results %>%
  filter(Year == 2100)

# Step 1: Summarize monthly PET values
monthly_pet <- pet_2020 %>%
  group_by(Month) %>%
  summarise(Monthly_PET = sum(Adjusted_PET, na.rm = TRUE))

# Step 2: Create a line chart for Monthly PET
ggplot(monthly_pet, aes(x = Month, y = Monthly_PET)) +
  geom_line(color = "blue", size = 1) +  # Line for monthly PET
  geom_point(color = "red", size = 2) +  # Points for each month
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  # Month abbreviations
  labs(
    title = "Monthly Potential Evapotranspiration (PET) for the Year 2100",
    x = "Month",
    y = "Adjusted PET (mm)",
    caption = "Source: Navsari Data"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Optional: Save the plot as a PNG file
ggsave("C:/Users/lenovo/Downloads/monthly_pet_2100.png", width = 8, height = 5)

-------------------------------------------------------------------------
  
#YEAR_WISE PLOT

library(ggplot2)
# Step 1: Filter the results for the selected years
selected_years <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
pet_selected_years <- results %>%
  filter(Year %in% selected_years)

# Step 2: Summarize monthly PET values for each selected year
monthly_pet <- pet_selected_years %>%
  group_by(Year, Month) %>%
  summarise(Monthly_PET = sum(Adjusted_PET, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Create a line chart for Monthly PET across selected years
ggplot(monthly_pet, aes(x = Month, y = Monthly_PET, color = as.factor(Year), group = Year)) +
  geom_line(size = 1) +  # Line for monthly PET
  geom_point(size = 2) +  # Points for each month
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  # Month abbreviations
  labs(
    title = "Year-wise Potential Evapotranspiration (PET) for Selected Years",
    x = "Month",
    y = "Adjusted PET (mm)",
    color = "Year",
    caption = "Source: Navsari Data"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set1")  # Use a color palette for better visualization

# Optional: Save the plot as a PNG file
ggsave("C:/Users/lenovo/Downloads/yearwise_pet_plot.png", width = 10, height = 6)
------------------------------------------------------------------------------------
  # Load necessary libraries
  library(dplyr)
library(lubridate)

# Step 1: Load the input data
data <- read.csv("C:/Users/lenovo/Downloads/Navsari.csv")

# Define the years of interest
years_of_interest <- seq(2020, 2100, by = 10)

# Initialize an empty data frame to store results
results <- data.frame()

# Loop through each year to calculate PET
for (year in years_of_interest) {
  
  # Step 2: Filter data for the specific year
  data_year <- data %>%
    filter(Year == year) %>%
    mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))
  
  # Step 3: Divide each month into 4 weeks and calculate weekly mean temperature & precipitation
  weekly_data <- data_year %>%
    group_by(Month, Week = ceiling(day(Date) / 7)) %>%
    summarise(
      Weekly_Mean_Temp = mean(Mean_Temp, na.rm = TRUE),
      Weekly_Precipitation = mean(Precipitation, na.rm = TRUE),
      Days_in_Week = n()
    ) %>%
    ungroup()
  
  # Step 4: Calculate Weekly Heat Index (i) and Annual Heat Index (I) divided by 4 for each year
  I <- sum((weekly_data$Weekly_Mean_Temp / 5) ^ 1.514) / 4  # Annual Heat Index for the year
  
  # Add the calculated I value to the weekly data
  weekly_data <- weekly_data %>%
    mutate(
      i = (Weekly_Mean_Temp / 5) ^ 1.514,  # Weekly heat index
      I = I  # Store the yearly I value for reference
    )
  
  # Step 5: Calculate Empirical Exponent 'a'
  a <- 0.000000657 * I^3 - 0.0000771 * I^2 + 0.01792 * I + 0.49239
  
  # Step 6: Calculate Unadjusted PET (e)
  weekly_data <- weekly_data %>%
    mutate(
      e = ifelse(I > 0, 1.6 * (10 * Weekly_Mean_Temp / I) ^ a, 0)  # Unadjusted PET
    )
  
  # Step 7: Define Monthly k-values
  k_values <- data.frame(
    Month = 1:12,
    k = c(0.91, 0.95, 1.02, 1.1, 1.18, 1.25, 1.25, 1.2, 1.1, 1.02, 0.95, 0.91)
  )
  
  # Step 8: Join k-values and calculate Adjusted PET
  weekly_data <- weekly_data %>%
    left_join(k_values, by = "Month") %>%
    mutate(
      Adjusted_PET = (k * e * 10) / Days_in_Week
    )
  
  # Step 9: Add the year column to results
  weekly_data <- weekly_data %>%
    mutate(Year = year)
  
  # Append the results for the current year
  results <- rbind(results, weekly_data)
}

# Step 10: Save the results to a CSV file
write.csv(results, "C:/Users/lenovo/Downloads/NAVSARI_pet_multiple_years_final.csv", row.names = FALSE)

# Step 11: View the first few rows of the result
print(head(results))

---------------------------------------------
# Load the data
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
data <- read.csv("C:/Users/lenovo/Downloads/NAVSARI_pet_multiple_years_final.csv")

# Ensure Week is treated as numeric for proper sorting
data$Week <- as.numeric(data$Week)

# Filter data for the specific years of interest
years_of_interest <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
data_filtered <- data %>% filter(Year %in% years_of_interest)

# Plot the weekly precipitation for each year in separate facets
ggplot(data_filtered, aes(x = Week, y = Weekly_Precipitation)) +
  geom_line(color = "blue", size = 1) +  # Line plot with blue color
  labs(
    title = "Weekly Precipitation Trends for Selected Years",
    x = "Week of the Year",
    y = "Weekly Precipitation (mm)"
  ) +
  theme_minimal() +  # Clean theme
  facet_wrap(~ Year, ncol = 3) +  # Create separate plots for each year (3 columns)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    strip.text = element_text(size = 12, face = "bold")  # Customize facet labels
  )

---------------------------------------------------------------------------------------
  # Load necessary libraries
  library(dplyr)
library(lubridate)

# Step 1: Load the input data
data <- read.csv("C:/Users/lenovo/Downloads/Navsari.csv")

# Define the years of interest
years_of_interest <- seq(2015, 2100)

# Initialize an empty data frame to store results
results <- data.frame()

# Loop through each year to calculate weekly precipitation
for (year in years_of_interest) {
  
  # Step 2: Filter data for the specific year
  data_year <- data %>%
    filter(Year == year) %>%
    mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))
  
  # Step 3: Create a week number based on the day of the month
  data_year <- data_year %>%
    mutate(Week = case_when(
      day(Date) <= 7 ~ 1,
      day(Date) <= 14 ~ 2,
      day(Date) <= 21 ~ 3,
      day(Date) <= 28 ~ 4,
      TRUE ~ 5  # This accounts for days 29-31
    ))
  
  # Step 4: Calculate weekly mean temperature & precipitation
  weekly_data <- data_year %>%
    group_by(Month, Week) %>%  # Group by Month and the new Week variable
    summarise(
      Weekly_Mean_Temp = mean(Mean_Temp, na.rm = TRUE),
      Weekly_Precipitation = sum(Precipitation, na.rm = TRUE),  # Sum precipitation for the week
      Days_in_Week = n()
    ) %>%
    ungroup()
  
  # Step 5: Calculate Annual Heat Index (I)
  I <- sum((weekly_data$Weekly_Mean_Temp / 5) ^ 1.514) / 4  # Annual Heat Index for the year
  
  # Add the calculated I value to the weekly data
  weekly_data <- weekly_data %>%
    mutate(
      i = (Weekly_Mean_Temp / 5) ^ 1.514,  # Weekly heat index
      I = I  # Store the yearly I value for reference
    )
  
  # Step 6: Calculate Empirical Exponent 'a'
  a <- 0.000000657 * I^3 - 0.0000771 * I^2 + 0.01792 * I + 0.49239
  
  # Step 7: Calculate Unadjusted PET (e)
  weekly_data <- weekly_data %>%
    mutate(
      e = ifelse(I > 0, 1.6 * (10 * Weekly_Mean_Temp / I) ^ a, 0)  # Unadjusted PET
    )
  
  # Step 8: Define Monthly k-values
  k_values <- data.frame(
    Month = 1:12,
    k = c(0.91, 0.95, 1.02, 1.1, 1.18, 1.25, 1.25, 1.2, 1.1, 1.02, 0.95, 0.91)
  )
  
  # Step 9: Join k-values and calculate Adjusted PET
  weekly_data <- weekly_data %>%
    left_join(k_values, by = "Month") %>%
    mutate(
      Adjusted_PET = (k * e * 10) / Days_in_Week,  
      
    )
  
  # Step 10: Add the year column to results
  weekly_data <- weekly_data %>%
    mutate(Year = year)
  
  # Append the results for the current year
  results <- rbind(results, weekly_data)
}

# Step 11: Save the results to a CSV file
write.csv(results, "C:/Users/lenovo/Downloads/Navsaridummy.csv", row.names = FALSE)

# Step 12: View the first few rows of the result
print(head(results))
----------------------------------------------------------------
  
  
# Load necessary libraries
library(dplyr)
library(lubridate)

# Step 1: Load the input data
data <- read.csv("C:/Users/lenovo/Downloads/Navsari.csv")

# Define the years of interest
years_of_interest <- seq(2015, 2100)

# Initialize an empty data frame to store results
results <- data.frame()

# Loop through each year to calculate weekly precipitation
for (year in years_of_interest) {
  
  # Step 2: Filter data for the specific year
  data_year <- data %>%
    filter(Year == year) %>%
    mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")))
  
  # Step 3: Create a week number based on the day of the month
  data_year <- data_year %>%
    mutate(Week = case_when(
      day(Date) <= 7 ~ 1,
      day(Date) <= 14 ~ 2,
      day(Date) <= 21 ~ 3,
      day(Date) <= 28 ~ 4,
      TRUE ~ 5  # This accounts for days 29-31
    ))
  
  # Step 4: Calculate weekly mean temperature & precipitation
  weekly_data <- data_year %>%
    group_by(Month, Week) %>%  # Group by Month and the new Week variable
    summarise(
      Weekly_Mean_Temp = mean(Mean_Temp, na.rm = TRUE),
      Weekly_Precipitation = sum(Precipitation, na.rm = TRUE),  # Sum precipitation for the week
      Days_in_Week = n()
    ) %>%
    ungroup()
  
  # Step 5: Calculate Annual Heat Index (I)
  I <- sum((weekly_data$Weekly_Mean_Temp / 5) ^ 1.514) / 54  # Annual Heat Index for the year
  
  # Add the calculated I value to the weekly data
  weekly_data <- weekly_data %>%
    mutate(
      i = (Weekly_Mean_Temp / 5) ^ 1.514,  # Weekly heat index
      I = I  # Store the yearly I value for reference
    )
  
  # Step 6: Calculate Empirical Exponent 'a'
  a <- 0.000000657 * (I^3) - (0.0000771 * (I^2)) + (0.01792 * I) + 0.49239
  
  # Step 7: Calculate Unadjusted PET (e) in cm/week
  weekly_data <- weekly_data %>%
    mutate(
      e = ifelse(I > 0, 1.6 * (10 * Weekly_Mean_Temp / I) ^ a, 0)  # Unadjusted PET in cm/week
    )
  
  # Step 8: Define Monthly k-values
  k_values <- data.frame(
    Month = 1:12,
    k = c(0.91, 0.95, 1.02, 1.1, 1.18, 1.25, 1.25, 1.2, 1.1, 1.02, 0.95, 0.91)
  )
  
  # Step 9: Join k-values and calculate Adjusted PET in mm/day
  weekly_data <- weekly_data %>%
    left_join(k_values, by = "Month") %>%
    mutate(
      # Convert e from cm/week to mm/day
      e_mm_per_week = e ,  # Convert from cm/week to mm/day (1 cm = 10 mm and 1 week = 7 days)
      Adjusted_PET = (k * e_mm_per_week)  # Calculate Adjusted PET in mm/day
    )
  
  # Step 10: Add the year column to results
  weekly_data <- weekly_data %>%
    mutate(Year = year)
  
  # Append the results for the current year
  results <- rbind(results, weekly_data)
}

write.csv(results, "C:/Users/lenovo/Downloads/Navsaridummy26.csv", row.names = FALSE)


# Step 12: View the first few rows of the result
print(head(results))



----------
  # Load required libraries
library(dplyr)
library(readxl)
library(scales)
# Sample dataset (replace with your actual file path or data loading mechanism)
# Assuming you have your data in a CSV file called 'data.csv'
data <- read.csv("C:/Users/lenovo/Downloads/WaterbalanceNavsaricc.csv")

data$Year <- as.numeric(data$Year)

# Filter the data for the year 2020
data_2020 <- data %>% filter(Year == 2020)

# Ensure the 'Year' and 'Month' columns are numeric
data$Year <- as.numeric(data$Year)
data$Month <- as.numeric(data$Month)

# Filter data for the year 2020
data_2020 <- data %>% filter(Year == 2020)

# Convert the 'Month' column to month names (e.g., 1 -> January)
data_2020$Month <- factor(
  data_2020$Month, 
  levels = 1:12, 
  labels = month.name
)

# Create a combined column for "Month-Week" to show detailed labeling on X-axis
data_2020 <- data_2020 %>%
  mutate(Month_Week = paste(Month, Week, sep = "-"))

# Plot: Month (with weeks) on X-axis and MAI on Y-axis
ggplot(data_2020, aes(x = Month_Week, y = MAI, group = Month)) +
  geom_point(size = 3, color = "blue") +  # Plot points for MAI values
  geom_line(aes(group = 1), color = "darkred", size = 1) +  # Connect points
  labs(title = "Monthly MAI (with Weeks) for 2020", 
       x = "Month - Week", 
       y = "MAI") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),  # Rotate X-axis labels
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  ) +
  scale_x_discrete(labels = function(x) sub("-.*", "", x))  # Simplify X-axis labels to just the month name






