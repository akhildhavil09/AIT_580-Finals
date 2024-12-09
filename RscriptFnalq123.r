# Load required libraries for data manipulation, visualization, and statistical analysis
library(tidyverse)
library(lubridate)
library(ggplot2)
library(stats)

# Load cleaned datasets for analysis
calls_df <- read_csv("calls_cleaned.csv")
arrests_df <- read_csv("arrests_cleaned.csv")

# 1. Analyze response times across boroughs to identify disparities and trends
response_time_analysis <- function(calls_df) {
  # Summarize response times by borough (mean, median, and standard deviation)
  borough_response_times <- calls_df %>%
    group_by(BORO_NM) %>%
    summarise(
      mean_response_time = mean(response_time, na.rm = TRUE),
      median_response_time = median(response_time, na.rm = TRUE),
      sd_response_time = sd(response_time, na.rm = TRUE)
    )
  
  # Perform ANOVA to test differences in response times between boroughs
  response_time_model <- aov(response_time ~ BORO_NM, data = calls_df)
  response_time_summary <- summary(response_time_model)
  
  # Create a bar chart to visualize average response times with error bars
  ggplot(borough_response_times, aes(x = BORO_NM, y = mean_response_time)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_errorbar(aes(ymin = mean_response_time - sd_response_time, 
                      ymax = mean_response_time + sd_response_time), 
                  width = 0.2) +
    labs(title = "Average Response Time by Borough",
         x = "Borough", 
         y = "Response Time (Minutes)") +
    theme_minimal()
  
  ggsave("response_time_borough.png", width = 10, height = 6) # Save the visualization
  
  # Return summary statistics and ANOVA results
  list(
    summary_stats = borough_response_times,
    anova_results = response_time_summary
  )
}

# 2. Explore arrest patterns by demographics and temporal trends
arrest_demographic_analysis <- function(arrests_df) {
  # Summarize arrest counts by borough, race, gender, and offense type
  demographic_summary <- arrests_df %>%
    group_by(ARREST_BORO, PERP_RACE, PERP_SEX, OFNS_DESC) %>%
    summarise(arrest_count = n()) %>%
    ungroup()
  
  # Extract hour from arrest dates for time-of-day analysis
  arrests_df$arrest_hour <- hour(arrests_df$ARREST_DATE)
  
  # Summarize hourly arrest patterns by borough and offense type
  hourly_arrest_summary <- arrests_df %>%
    group_by(arrest_hour, ARREST_BORO, OFNS_DESC) %>%
    summarise(arrest_count = n()) %>%
    ungroup()
  
  # Visualize arrests by hour, borough, and offense type
  ggplot(hourly_arrest_summary, aes(x = arrest_hour, y = arrest_count, fill = ARREST_BORO)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ OFNS_DESC, scales = "free_y") +
    labs(title = "Arrests by Hour, Borough, and Offense Type",
         x = "Hour of Day", 
         y = "Number of Arrests") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("arrests_by_hour_borough.png", width = 15, height = 10) # Save the visualization
  
  # Return demographic and hourly arrest summaries
  list(
    demographic_summary = demographic_summary,
    hourly_arrest_summary = hourly_arrest_summary
  )
}
incident_resolution_analysis <- function(calls_df) {
  
  # Convert INCIDENT_TIME and CLOSNG_TS to POSIXct if they are not already
  calls_df$INCIDENT_TIME <- as.POSIXct(calls_df$INCIDENT_TIME, format = "%Y-%m-%d %H:%M:%S")
  calls_df$CLOSNG_TS <- as.POSIXct(calls_df$CLOSNG_TS, format = "%Y-%m-%d %H:%M:%S")
  
  # Calculate the response time (closing time - incident time) in minutes
  calls_df$response_time <- as.numeric(difftime(calls_df$CLOSNG_TS, calls_df$INCIDENT_TIME, units = "mins"))
  
  # Remove rows with missing response times or empty incident types
  calls_df <- calls_df %>%
    filter(!is.na(response_time), !is.na(TYP_DESC), TYP_DESC != "")
  
  # Convert TYP_DESC to a factor
  calls_df$TYP_DESC <- as.factor(calls_df$TYP_DESC)
  
  # Remove unused levels (levels with no observations)
  calls_df$TYP_DESC <- droplevels(calls_df$TYP_DESC)
  
  # Check if TYP_DESC has more than one level
  if (length(levels(calls_df$TYP_DESC)) < 2) {
    stop("The 'TYP_DESC' variable must have at least two levels for regression analysis.")
  }
  
  # Check the number of rows per incident type to ensure no empty levels
  print(table(calls_df$TYP_DESC))  # To verify if there are sufficient data in each level
  
  # Perform linear regression to model response time based on incident type
  lm_model <- lm(response_time ~ TYP_DESC, data = calls_df)
  
  # Summary of the regression model
  lm_summary <- summary(lm_model)
  
  # Visualize the relationship between incident type and mean response time
  resolution_summary <- calls_df %>%
    group_by(TYP_DESC) %>%
    summarise(mean_response_time = mean(response_time, na.rm = TRUE)) %>%
    ungroup()
  
  ggplot(resolution_summary, aes(x = TYP_DESC, y = mean_response_time)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Average Response Time by Incident Type",
         x = "Incident Type", 
         y = "Mean Response Time (Minutes)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("response_time_by_incident_type.png", width = 15, height = 8) # Save the visualization
  
  # Return regression model summary
  list(
    regression_summary = lm_summary
  )
}



#1 Analyze response times across boroughs
response_time_results <- response_time_analysis(calls_df)

#2 Analyze demographic patterns in arrests
demographic_results <- arrest_demographic_analysis(arrests_df)

#3 Analyze incident resolution times
resolution_results <- incident_resolution_analysis(calls_df)


set.seed(42)  # Set seed for reproducibility
random_types <- sample(levels(calls_df$TYP_DESC), size = 5, replace = FALSE)
