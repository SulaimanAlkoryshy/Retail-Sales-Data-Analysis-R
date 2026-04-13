# Retail-Sales-Data-Analysis-R

This project focuses on analyzing retail sales data to extract actionable insights. It covers the entire data science workflow, from raw data ingestion and cleaning to advanced exploratory data analysis.

## Data Cleaning Process
* **Missing Values:** Imputed missing Quantity and Price using the median.
* **Inconsistency Handling:** Used a custom `get_mode` function to correct inconsistent product categories.
* **Feature Engineering:** Calculated `TotalSales` and extracted time-based features using `lubridate`.

## Key Visualizations
* **Monthly Sales Trends:** Identifying seasonal patterns and growth.
* **Regional Distribution:** Pie charts showing the market share of different regions.
* **Category Performance:** Bar charts highlighting top-selling categories like Electronics and Beauty.

### **Language:** R
### **Libraries:** `data.table` (Fast data manipulation), `ggplot2` (Visualization), `lubridate` (Date handling).
