install.packages("tidyverse")
install.packages("data.table")
install.packages("lubridate")
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)

data <- fread("C:/Users/96653/Downloads/retail_sales(1).csv")

view(data)
str(data)
summary(data)
head(data)

# check missing values
colSums(is.na(data))
sum(data$Category == "")
sum(data$Region == "")

# Checking for inconsistencies in product categories
inconsistencies_categories <- data[, list(UniqueCategories = uniqueN(Category)), by = ProductID]
products_w_inconsistent <- inconsistencies_categories[UniqueCategories > 1]
products_w_inconsistent

# ensure rows are unique
nrow(unique(data))

# replace missing values, date format and create new feature
retail_data<- data[,
  {
    median_q <- median(Quantity, na.rm = TRUE)
    median_p <- median(Price, na.rm = TRUE)
    Quantity <- ifelse(is.na(Quantity), median_q, Quantity)
    Price <- ifelse(is.na(Price), median_p, Price)
    
    list(
      CustomerID, ProductID, Quantity, Price, 
      Category = ifelse(is.na(Category) | Category == "", "Unknown", Category),
      PurchaseDate = ymd(PurchaseDate), 
      Region = ifelse(is.na(Region) | Region == "", "Unknown", Region)
        )
  }
]
#function to extract the most frequently 
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))] 
}
#Correcting the categories based on the most frequent use of each product.
product_category <- retail_data[, list(CorrectCategory = get_mode(Category)), by = ProductID]
retail_data[product_category, Category := i.CorrectCategory, on = "ProductID"]
#checking after the correcting
inconsistencies_categories2 <- retail_data[, list(UniqueCategories = uniqueN(Category)), by = ProductID]
products_w_inconsistent2 <- inconsistencies_categories2[UniqueCategories > 1]
products_w_inconsistent2

view(retail_data)
str(retail_data)
head(retail_data)
summary(retail_data)
nrow(retail_data)
colSums(is.na(retail_data))
sum(c(retail_data$Category,retail_data$Region) == "")

retail_data[,TotalSales:=Quantity*Price]
colnames(retail_data)

top_products <- retail_data[
  , list(Total_Sales = sum(TotalSales)), by = ProductID 
][
  order(desc(Total_Sales)) 
][
  1:10 
]

top_products

ggplot(top_products, 
  aes(x = reorder(ProductID, Total_Sales), y = Total_Sales,fill = Total_Sales)) +
  geom_col() + 
  scale_fill_gradient(low = "#9ecae1", high = "#151B54", name = "Sales")+ 
  coord_flip() + 
  ggtitle("Top 10 Products by total Sales") +  
  xlab("Product ID") +                        
  ylab("Total Sales") + 
  theme_bw()



Category_colors <- c( "Beauty" = "#F46C5B","Books" = "#C18E0C","Clothing" = "#7A9C2F","Electronics" = "#00B8D9"
                      ,"Furniture" = "#C68642","Groceries" = "#6CBF47","Home Appliances" = "#5A6F9E","Stationery" = "#C77CFF","Toys" = "#E754C7","Unknown"="#7f7f7f")

sales_by_category <- retail_data[ ,list(Total_Sales = sum(TotalSales)), by = Category ]

ggplot(sales_by_category,
  aes(x = reorder(Category, Total_Sales), y = Total_Sales,fill = Category)) +
  geom_col() +
  scale_fill_manual(values = Category_colors)+
  coord_flip() + 
  ggtitle("Total Sales by Product Category")+
  xlab("Product Category")+
  ylab("Total Sales") +
  theme_bw()


regional_sales_category <- retail_data[,list(Total_Sales = sum(TotalSales)), by = list(Region, Category)]

ggplot(regional_sales_category, 
  aes(x = reorder(Region, Total_Sales), y = Total_Sales,fill = Category)) +
  geom_col() +
  scale_fill_manual(values = Category_colors)+
  ggtitle("Regional Sales Distribution by Product Category")+
  xlab("Region")+
  ylab("Total Sales")+
  theme_bw()

region_distribution <- retail_data[, list(Total_Sales = sum(TotalSales)), by = Region]
region_distribution[, Percentage := round((Total_Sales / sum(Total_Sales)) * 100, 1)]

ggplot(region_distribution, 
  aes(x = "", y = Total_Sales, fill = Region)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Region, "\n", Percentage, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5) +
  ggtitle("Sales Distribution by Region")+
  theme_void()

monthly_sales <- retail_data[,list(Total_Sales = sum(TotalSales)), by = list(YearMonth = floor_date(PurchaseDate, "month")) ][
  order(YearMonth)
  ]

ggplot(monthly_sales,
  aes(x = YearMonth, y = Total_Sales)) +
  geom_line(color = "blue") +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  ggtitle("Monthly Sales Trend")+
  xlab("Date")+
  ylab("Total Sales")+
  theme_bw()