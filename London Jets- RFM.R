
# Load necessary libraries
library(readxl) # For reading Excel files
library(dplyr)  # For data manipulation
library(rfm)    # For RFM analysis
library(lubridate)  # For working with dates
library(writexl)    # For writing Excel files
library(ggplot2)    # For plotting

# Read the data from the Excel file
data <- read_excel("London Jets, Spreadsheet Supplement.xls", sheet = "Customer")

# Select relevant columns from the dataset
data1 <- select(data, CustID, Num_Games, Tot_Sales, LastTransYear, LastTransMonth)

# Check for missing values
sum(is.na(data1))

# Summarize the data
summary(data1)

# Display the structure of the data
str(data1)

# Create a date variable from LastTransYear and LastTransMonth
data1$Trans_Date <- make_date(year = data1$LastTransYear, month = data1$LastTransMonth, day = "01")

# Remove unnecessary columns
data2 <- data1[, -c(4:5)]

# Define analysis date
analysis_date <- as.Date("2002-01-01")

# Perform RFM analysis
rfm_values <- rfm_table_customer_2(data = data2, customer_id = CustID, n_transactions = Num_Games,
                                   latest_visit_date = Trans_Date, total_revenue = Tot_Sales, analysis_date = analysis_date,
                                   recency_bins = 10, frequency_bins = 10, monetary_bins = 10) 

# Write RFM values to a CSV file
write.csv(rfm_values$rfm, file = "rfm.csv")

# Define RFM categories and corresponding score ranges
rfm_categories <- c("First Grade", "Loyal", "Likely to be Loyal", "New Ones", "Could be Promising", 
                    "Require Assistance", "Getting Less Frequent", "Almost Out", "Can't Lose Them",
                    "Don't Show Up at All") 

recency_lower <-    c(9, 7, 5, 6, 7, 1, 1, 1, 1, 1)
recency_higher <-   c(10, 10, 9, 10, 7, 6, 4, 4,6,3)
frequncy_lower <-   c(9, 7, 5, 1, 4, 2, 1, 1, 3, 1)
frequency_higher <- c(10, 10, 9, 10, 8, 6, 8, 5,9,3)
monetary_lower <-   c(9, 7, 5, 1, 4, 1, 2, 1, 6, 1)
monetary_higher <-  c(10, 10, 9, 8, 8, 6, 7, 6, 10,3)

# Segment customers based on RFM values
rfm_segment <- rfm_segment(data = rfm_values, segment_names = rfm_categories, recency_lower, recency_higher, frequncy_lower, frequency_higher,
                           monetary_lower, monetary_higher)

# Summarize segments
segment_summary <- rfm_segment %>% 
  count(segment) %>% 
  arrange(desc(n)) %>% 
  rename(Count = n) %>% 
  mutate(Percentage = Count / sum(Count) * 100) %>% 
  mutate(Percentage = round(Percentage, digits = 2))

# Summarize RFM values
mysummary <- rfm_segment %>% 
  group_by(segment) %>% 
  summarise(M_recency = median(recency_days), 
            M_frequency = median(transaction_count),
            M_money = median(amount))

# Merge segment summary and RFM value summary
overall_summary <- merge(segment_summary, mysummary, by = "segment")

# Arrange summary by count
overall_summary <- arrange(overall_summary, desc(Count))

# Plot median recency
rfm_plot_median_recency(rfm_segment)

# Plot median frequency
rfm_plot_median_frequency(rfm_segment)                                                                                                      

# Plot median monetary
rfm_plot_median_monetary(rfm_segment)

# Plot histograms
rfm_histograms(rfm_values)

# Plot order distribution
rfm_order_dist(rfm_values)

# Plot bar chart
rfm_bar_chart(rfm_values)

# Plot heatmap
rfm_heatmap(rfm_values)

# Group customers based on RFM scores
Grp <- rfmdata %>% 
  group_by(recency_score, frequency_score, monetary_score) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
