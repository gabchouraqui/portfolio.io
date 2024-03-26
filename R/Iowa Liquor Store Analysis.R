library(dplyr)
library(readr)
library(ggplot2)


df <- read.csv("C:/Users/gabch/Documents/archive/liquor.csv")


# Aggregate data to find total sales for each seller
seller_sales <- aggregate(total ~ vendor, data = df, FUN = sum)

# Sort the aggregated data by total sales in descending order
seller_sales <- seller_sales[order(-seller_sales$total), ]

# Select top 5 sellers
top_5_sellers <- head(seller_sales, 5)

# Create a bar graph to visualize the top 5 sellers and their total sales
ggplot(top_5_sellers, aes(x = reorder(vendor, -total), y = total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 5 Sellers by Total Sales",
       x = "Seller",
       y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## LAST 5

# Select last 5 sellers
last_5_sellers <- tail(seller_sales, 5)

# Create a bar graph to visualize the last 5 sellers and their total sales
ggplot(last_5_sellers, aes(x = reorder(vendor, total), y = total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Last 5 Sellers by Total Sales",
       x = "Seller",
       y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
