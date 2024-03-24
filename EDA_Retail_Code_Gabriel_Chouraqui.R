#' Author: Gabriel Chouraqui
#' Data: Jan, 2019 to Nov, 2023
#' Purpose: Iowa Liquor Sales
#' 

# Libs
library(radiant.data)
library(DataExplorer)
library (ggplot2)
library (ggthemes)
library (purrr)
library (readr)
library(png)
library (dplyr)
library (lubridate)
library (tidyr)
library (scales)
library(visdat)
library(knitr)
library(stringr)
library(VIM)

# Set WD
setwd("~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/Personal Files")

# Importing data

# 2019
Jan_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/1_Jan_2019_Iowa_liquor_sales.csv')
Feb_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/2_Feb_2019_Iowa_liquor_sales.csv')
Mar_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/3_Mar_2019_Iowa_liquor_sales.csv')
Apr_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/4_Apr_2019_Iowa_liquor_sales.csv')
May_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/5_May_2019_Iowa_liquor_sales.csv')
Jun_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/6_Jun_2019_Iowa_liquor_sales.csv')
Jul_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/7_Jul_2019_Iowa_liquor_sales.csv')
Aug_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/8_Aug_2019_Iowa_liquor_sales.csv')
Sep_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/9_Sep_2019_Iowa_liquor_sales.csv')
Oct_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/10_Oct_2019_Iowa_liquor_sales.csv')
Nov_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/11_Nov_2019_Iowa_liquor_sales.csv')
Dec_2019_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2019/12_Dec_2019_Iowa_liquor_sales.csv')

# 2020
Jan_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/1_Jan_2020_Iowa_liquor_sales.csv')
Feb_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/2_Feb_2020_Iowa_liquor_sales.csv')
Mar_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/3_Mar_2020_Iowa_liquor_sales.csv')
Apr_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/4_Apr_2020_Iowa_liquor_sales.csv')
May_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/5_May_2020_Iowa_liquor_sales.csv')
Jun_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/6_Jun_2020_Iowa_liquor_sales.csv')
Jul_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/7_Jul_2020_Iowa_liquor_sales.csv')
Aug_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/8_Aug_2020_Iowa_liquor_sales.csv')
Sep_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/9_Sep_2020_Iowa_liquor_sales.csv')
Oct_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/10_Oct_2020_Iowa_liquor_sales.csv')
Nov_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/11_Nov_2020_Iowa_liquor_sales.csv')
Dec_2020_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2020/12_Dec_2020_Iowa_liquor_sales.csv')

# 2021
Jan_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/1_Jan_2021_Iowa_liquor_sales.csv')
Feb_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/2_Feb_2021_Iowa_liquor_sales.csv')
Mar_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/3_Mar_2021_Iowa_liquor_sales.csv')
Apr_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/4_Apr_2021_Iowa_liquor_sales.csv')
May_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/5_May_2021_Iowa_liquor_sales.csv')
Jun_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/6_Jun_2021_Iowa_liquor_sales.csv')
Jul_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/7_Jul_2021_Iowa_liquor_sales.csv')
Aug_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/8_Aug_2021_Iowa_liquor_sales.csv')
Sep_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/9_Sep_2021_Iowa_liquor_sales.csv')
Oct_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/10_Oct_2021_Iowa_liquor_sales.csv')
Nov_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/11_Nov_2021_Iowa_liquor_sales.csv')
Dec_2021_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2021/12_Dec_2021_Iowa_liquor_sales.csv')

# 2022
Jan_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/1_Jan_2022_Iowa_liquor_sales.csv')
Feb_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/2_Feb_2022_Iowa_liquor_sales.csv')
Mar_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/3_Mar_2022_Iowa_liquor_sales.csv')
Apr_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/4_Apr_2022_Iowa_liquor_sales.csv')
May_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/5_May_2022_Iowa_liquor_sales.csv')
Jun_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/6_Jun_2022_Iowa_liquor_sales.csv')
Jul_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/7_Jul_2022_Iowa_liquor_sales.csv')
Aug_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/8_Aug_2022_Iowa_liquor_sales.csv')
Sep_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/9_Sep_2022_Iowa_liquor_sales.csv')
Oct_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/10_Oct_2022_Iowa_liquor_sales.csv')
Nov_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/11_Nov_2022_Iowa_liquor_sales.csv')
Dec_2022_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2022/12_Dec_2022_Iowa_liquor_sales.csv')

# 2023 (up to November)
Jan_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/1_Jan_2023_Iowa_liquor_sales.csv')
Feb_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/2_Feb_2023_Iowa_liquor_sales.csv')
Mar_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/3_Mar_2023_Iowa_liquor_sales.csv')
Apr_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/4_Apr_2023_Iowa_liquor_sales.csv')
May_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/5_May_2023_Iowa_liquor_sales.csv')
Jun_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/6_Jun_2023_Iowa_liquor_sales.csv')
Jul_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/7_Jul_2023_Iowa_liquor_sales.csv')
Aug_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/8_Aug_2023_Iowa_liquor_sales.csv')
Sep_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/9_Sep_2023_Iowa_liquor_sales.csv')
Oct_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/10_Oct_2023_Iowa_liquor_sales.csv')
Nov_2023_sales <- read_csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A1_Retail_EDA/data/2023/11_Nov_2023_Iowa_liquor_sales.csv')

# Putting all the data together
all_data <- bind_rows(
  Jan_2019_sales, Feb_2019_sales, Mar_2019_sales, Apr_2019_sales, May_2019_sales, Jun_2019_sales,
  Jul_2019_sales, Aug_2019_sales, Sep_2019_sales, Oct_2019_sales, Nov_2019_sales, Dec_2019_sales,
  Jan_2020_sales, Feb_2020_sales, Mar_2020_sales, Apr_2020_sales, May_2020_sales, Jun_2020_sales,
  Jul_2020_sales, Aug_2020_sales, Sep_2020_sales, Oct_2020_sales, Nov_2020_sales, Dec_2020_sales,
  Jan_2021_sales, Feb_2021_sales, Mar_2021_sales, Apr_2021_sales, May_2021_sales, Jun_2021_sales,
  Jul_2021_sales, Aug_2021_sales, Sep_2021_sales, Oct_2021_sales, Nov_2021_sales, Dec_2021_sales,
  Jan_2022_sales, Feb_2022_sales, Mar_2022_sales, Apr_2022_sales, May_2022_sales, Jun_2022_sales,
  Jul_2022_sales, Aug_2022_sales, Sep_2022_sales, Oct_2022_sales, Nov_2022_sales, Dec_2022_sales,
  Jan_2023_sales, Feb_2023_sales, Mar_2023_sales, Apr_2023_sales, May_2023_sales, Jun_2023_sales,
  Jul_2023_sales, Aug_2023_sales, Sep_2023_sales, Oct_2023_sales, Nov_2023_sales
)


# What's the overall structure & dimensions of the data?
str(all_data)
head(all_data$`Vendor Name`)


# Count missing values for each column
missing_values_count <- colSums(is.na(all_data))
missing_values_count

# Calculate the percentage of missing values for each column
missing_percentage <- colMeans(is.na(all_data)) * 100
missing_percentage


# Remove rows with missing values
df_all <- all_data[complete.cases(all_data),]

# Count missing values for each column in the cleaned data
cleaned_missing_values_count <- colSums(is.na(df_all))
cleaned_missing_values_count


# Convert 'Date' column to Date format
df_all$Date <- as.Date(df_all$Date, format = "%Y-%m-%d")
str(df_all)


# Data set class
class(df_all)

# Classes for each column
sapply(df_all, class)

# Look at the top 6 rows
head(df_all)

# How many unique stores?
length(unique(df_all$`Store Number`))

# What are the column names?
names(df_all)

# Summary stats for each vector
summary(df_all)

---------------------------------------------------------------------------------

# Create a new dataframe with only DIAGEO AMERICAS
df_diageo <- df_all %>%
  filter(str_detect(tolower(`Vendor Name`), "diageo americas"))
head(df_diageo)


# Create a new dataframe without DIAGEO AMERICAS
df_not_diageo <- df_all %>%
  filter(!str_detect(tolower(`Vendor Name`), "diageo americas"))
head(df_not_diageo)

---------------------------------------------------------------------------------

# Sales trends across years

# Extract year from Date
  df_all$Year <- year(df_all$Date)

# Calculate total sales by year
sum_sales_by_year <- df_all %>%
  group_by(Year) %>%
  summarise(sum_sales = sum(`Sale (Dollars)`))

# Reorder the factor levels based on total sales
sum_sales_by_year$Year <- factor(sum_sales_by_year$Year, levels = sum_sales_by_year$Year[order(sum_sales_by_year$sum_sales, decreasing = FALSE)])

# Plot total sales by year
ggplot(sum_sales_by_year, aes(x = Year, y = sum_sales, fill = sum_sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Year",
       x = "Year",
       y = "Total Sales (Dollars)",
       fill = NULL) +
  theme_minimal() +
  scale_fill_gradient(low = "#EBDAA8", high = "#A18F63") +
  scale_y_continuous(labels = scales::label_comma()) +
  guides(fill = 'none') +
  theme(
    plot.background = element_rect(fill = "transparent"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    text = element_text(color = "white")
  )

# Export the plot as a PNG
ggsave("total_sales_plot.png", plot, bg = "transparent")


# Sales trends across months
# Create a vector of month names
month_names <- month.abb

# Calculate average sales by month
average_sales_by_month <- df_diageo %>%
  group_by(Month) %>%
  summarise(avg_sales = mean(`Sale (Dollars)`))

# Plot average sales by month
ggplot(average_sales_by_month, aes(x = factor(Month, labels = month_names), y = avg_sales, fill = avg_sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sales by Month",
       x = "Month",
       y = "Average Sales (Dollars)") +
  theme_minimal() + 
  scale_x_discrete(labels = month_names) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  scale_y_continuous(labels = scales::label_comma()) +
  guides(fill = 'none')


# Sales trends across seasons
# Define a function to map months to seasons
get_season <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

# Apply the function to create a new variable "Season"
df_diageo$Season <- sapply(df_diageo$Month, get_season)

# Calculate average sales by season
average_sales_by_season <- df_diageo %>%
  group_by(Season) %>%
  summarise(avg_sales = mean(`Sale (Dollars)`))

# Reorder the factor levels based on average sales
average_sales_by_season$Season <- factor(average_sales_by_season$Season, levels = c("Winter", "Spring", "Summer", "Fall")[order(average_sales_by_season$avg_sales, decreasing = TRUE)])

# Plot average sales by season
ggplot(average_sales_by_season, aes(x = Season, y = avg_sales, fill = avg_sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sales by Season",
       x = "Season",
       y = "Average Sales (Dollars)") +
  theme_minimal() +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  scale_y_continuous(labels = label_comma()) +
  guides(fill = 'none')


# Sales trends across weekdays VS weekends

# Create a new variable to represent weekdays and weekends
df_diageo$DayType <- ifelse(weekdays(df_diageo$Date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Calculate average sales by day type
average_sales_by_day_type <- df_diageo %>%
  group_by(DayType) %>%
  summarise(avg_sales = mean(`Sale (Dollars)`))

# Reorder the factor levels based on average sales
average_sales_by_day_type$DayType <- factor(average_sales_by_day_type$DayType, levels = c("Weekday", "Weekend")[order(average_sales_by_day_type$avg_sales, decreasing = TRUE)])

# Plot average sales by day type
ggplot(average_sales_by_day_type, aes(x = DayType, y = avg_sales, fill = DayType)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sales by Day Type",
       x = "Day Type",
       y = "Average Sales (Dollars)") +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "darkgreen")) +
  scale_y_continuous(labels = scales::label_comma()) +
  guides(fill = 'none')


# Dominant store locations
top_stores <- df_diageo %>%
  group_by(`Store Number`) %>%
  summarise(Total_Sales = sum(`Sale (Dollars)`)) %>%
  arrange(desc(Total_Sales)) %>%
  top_n(10)

# Plot top store performance
ggplot(top_stores, aes(x = factor(`Store Number`, levels = factor(`Store Number`, levels = top_stores$`Store Number`[order(-top_stores$Total_Sales)])), y = Total_Sales, fill = `Store Number`)) +
  geom_bar(stat = "identity", color = "white", show.legend = FALSE) + 
  labs(title = "Top 10 Store Performances", x = "Store Name", y = "Total Sales") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),  
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", color = "white"),  
    axis.title = element_text(size = 14, color = "white"),  
    axis.text.y = element_text(size = 12, color = "white"),  
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    legend.position = "none"  
  ) +
  scale_fill_gradient(low = "#EBDAA8", high = "#A18F63") +
  scale_y_continuous(labels = scales::dollar_format())

# Export the plot as a PNG 
ggsave("top_plot.png", plot, bg = "transparent")




# Filter data for store number 2633
store_2633_data <- all_data %>%
  filter(`Store Number` == 2633) %>%
  distinct(`Store Name`, Address, City, `Zip Code`, County)
store_2633_data


# Plot scatter plot
ggplot(store_2633_data, aes(x = Date, y = `Sale (Dollars)`)) +
  geom_point() +
  labs(title = "Sales of Store 2633 Over Time",
       x = "Date",
       y = "Sale (Dollars)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar_format())


# Filter data for store number 4829
store_4829_data <- all_data %>%
  filter(`Store Number` == 4829) %>%
  distinct(`Store Name`, Address, City, `Zip Code`, County)
store_4829_data

# Plot scatter plot
ggplot(store_4829_data, aes(x = Date, y = `Sale (Dollars)`)) +
  geom_point() +
  labs(title = "Sales of Store 4829 Over Time",
       x = "Date",
       y = "Sale (Dollars)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar_format())


# Prominent liquor categories and their sales composition
# Group data by Category Name and calculate total sales for each category
liquor_category_sales <- df_all %>%
  group_by(`Category Name`) %>%
  summarise(Total_Sales = sum(`Sale (Dollars)`)) %>%
  arrange(desc(Total_Sales))

# Calculate cumulative percentage of sales
liquor_category_sales <- liquor_category_sales %>%
  mutate(Cumulative_Percentage = cumsum(Total_Sales) / sum(Total_Sales))

# Filter categories representing 80% of sales
top_categories <- liquor_category_sales %>%
  filter(Cumulative_Percentage <= 0.8)

# Visual
ggplot(top_categories, aes(x = reorder(`Category Name`, Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", aes(fill = Total_Sales), color = "black") +
  scale_fill_gradient(low = "#EBDAA8", high = "#A18F63") +
  labs(title = "Top Liquor Categories Representing 80% of Sales",
       x = "Liquor Category",
       y = "Total Sales") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),  # Set x-axis text color
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold", color = "black"),  # Set title text color
    axis.title = element_text(size = 10, color = "black")) +  # Set axis title text color
  scale_y_continuous(labels = scales::dollar_format()) +
  guides(fill = 'none')

# Export the plot as a PNG 
ggsave("top_categories_plot.png", plot_categories, bg = "transparent", width = 8, height = 6, units = "in", dpi = 300, type = "cairo")




# CONSUMER PREFERENCES IN BOTTLE VOLUME

# Create custom bins
bins <- c(0, 500, 1000, Inf)

# Create a new variable indicating the bin for each observation
df_diageo$Volume_Bin <- cut(df_diageo$`Bottle Volume (ml)`, breaks = bins, labels = c("0-500", "500-1000", "More than 1000"), include.lowest = TRUE)

# Bar plot for custom bins
ggplot(df_diageo, aes(x = Volume_Bin, fill = Volume_Bin)) +
  geom_bar() +
  scale_fill_manual(values = c("0-500" = "blue", "500-1000" = "darkgreen", "More than 1000" = "lightgreen")) +
  labs(title="Bottle Volume Distribution", x="Volume Range", y="Frequency") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = scales::comma_format())+
  guides(fill = 'none')


# Vendor performance and their contribution to total sales and liters

# Total Sales by Vendor
total_sales_by_vendor <- df_all %>%
  group_by(`Vendor Name`) %>%
  summarise(Total_Sales = sum(`Sale (Dollars)`)) %>%
  arrange(desc(Total_Sales))

# Calculate cumulative percentage of total sales
total_sales_by_vendor$Cumulative_Percentage <- cumsum(total_sales_by_vendor$Total_Sales) / sum(total_sales_by_vendor$Total_Sales)

# Identify the top vendors that make up 80% of the sales
top_vendors <- total_sales_by_vendor %>%
  filter(Cumulative_Percentage <= 0.8)

# Save the plot with 
png("top_vendors_plot.png", width = 1200, height = 900, bg = "transparent", res = 300)

# Bar plot for Top Vendors
ggplot(top_vendors, aes(x = reorder(`Vendor Name`, -Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#EBDAA8", high = "#A18F63") + 
  labs(title="Top Vendors (80% of Total Sales)", x="Vendor Name", y="Total Sales (Dollars)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent"),  
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"), 
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", color = "white"),  
    axis.title = element_text(size = 14, color = "white"), 
    axis.text.y = element_text(size = 12, color = "white"),  
    panel.grid.major.y = element_line(color = "transparent", linetype = "dashed"),
    legend.position = "none" 
  ) +
  scale_y_continuous(labels = scales::dollar_format())

# Close the PNG device
dev.off()



# Total Liters Sold by Vendor
total_liters_by_vendor <- df_diageo %>%
  group_by(`Vendor Name`) %>%
  summarise(Total_Liters = sum(`Bottle Volume (ml)`)/1000)

# Sort in descending order of total liters
total_liters_by_vendor <- total_liters_by_vendor[order(-total_liters_by_vendor$Total_Liters), ]

# Calculate cumulative percentage of total liters
total_liters_by_vendor$Cumulative_Percentage <- cumsum(total_liters_by_vendor$Total_Liters) / sum(total_liters_by_vendor$Total_Liters)

# Identify the top vendors that make up 80% of the liters
top_vendors_liters <- total_liters_by_vendor %>%
  filter(Cumulative_Percentage <= 0.8)

# Bar plot for Top Vendors in terms of Liters
ggplot(top_vendors_liters, aes(x = reorder(`Vendor Name`, -Total_Liters), y = Total_Liters, fill = Total_Liters)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title="Top Vendors (80% of Total Liters Sold)", x="Vendor Name", y="Total Liters (in Thousands)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma_format()) +
  guides(fill = 'none')



# GEOGRAPHIC

# Sales by ZIP Code
sales_by_zip <- df_diageo %>%
  group_by(`Zip Code`) %>%
  summarise(Total_Sales = sum(`Sale (Dollars)`))

# Top N ZIP Codes
top_zip_codes <- sales_by_zip %>%
  top_n(10, wt = Total_Sales)

# Bar plot for Sales by ZIP Code
ggplot(top_zip_codes, aes(x = reorder(`Zip Code`, -Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title="Sales by ZIP Code", x="ZIP Code", y="Total Sales (Dollars)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  guides(fill = 'none')

# Sales by City
sales_by_city <- df_all %>%
  group_by(City) %>%
  summarise(Total_Sales = sum(`Sale (Dollars)`))

# Sort in descending order of total sales
sales_by_city <- sales_by_city[order(-sales_by_city$Total_Sales), ]

# Calculate cumulative percentage of total sales
sales_by_city$Cumulative_Percentage <- cumsum(sales_by_city$Total_Sales) / sum(sales_by_city$Total_Sales)

# Filter top cities representing 80% of sales
top_cities <- sales_by_city %>%
  filter(Cumulative_Percentage <= 0.8)

# Select top 10 cities

ggplot(top_10_cities, aes(x = reorder(City, -Total_Sales), y = Total_Sales, fill = Total_Sales)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#EBDAA8", high = "#A18F63") +
  labs(title="Top 10 Cities by Total Sales", x="City", y="Total Sales (Dollars)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        plot.title = element_text(size = 12, hjust = 0.5, face = "bold", color = "black"),
        axis.title = element_text(size = 10, color = "black")) +
  scale_y_continuous(labels = scales::dollar_format()) +
  guides(fill = 'none')

# END