cso_data <- read.csv("/cloud/project/MEC02.20250930T180900.csv")
View(cso_data)

library(tidyverse)
library(scales)  

# Filter relevant categories
cso_clean <- cso_data %>%
  filter(`Electricity.Consumption` %in% c("Data centres", 
                                          "Customers other than data centres", 
                                          "All metered electricity consumption")) %>%
  select(Quarter, `Electricity.Consumption`, VALUE)

# Pivot for comparison
cso_wide <- cso_clean %>%
  pivot_wider(names_from = `Electricity.Consumption`, values_from = VALUE)

head(cso_wide)

cso_wide <- cso_wide %>%
  mutate(
    data_centre_share = `Data centres` / `All metered electricity consumption`,
    other_share = `Customers other than data centres` / `All metered electricity consumption`
  )

ggplot(cso_clean, aes(x = Quarter, 
                         y = VALUE, 
                         color = `Electricity.Consumption`, 
                         group = `Electricity.Consumption`)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Electricity Consumption in Ireland",
       subtitle = "Data Centres vs. Other Customers",
       y = "Consumption (GWh)", x = "Quarter") +
  theme_minimal()

library(zoo)  # for yearqtr

library(scales)

ggplot(cso_clean, aes(x = Quarter, 
                         y = VALUE, 
                         color = `Electricity.Consumption`, 
                         group = `Electricity.Consumption`)) +
  geom_line(linewidth = 1.2) +
  scale_x_date(date_labels = "%YQ%q", date_breaks = "1 year") +
  labs(title = "Electricity Consumption in Ireland",
       subtitle = "Data Centres vs. Other Customers",
       y = "Consumption (GWh)", x = "Quarter") +
  theme_minimal()

-----
library(stringr)

cso_clean <- cso_clean %>%
  mutate(Year = str_sub(Quarter, 1, 4) |> as.numeric())


typeof(cso_clean$Quarter)
ggplot(cso_clean, aes(x = Quarter, 
                         y = VALUE, 
                         color = `Electricity.Consumption`, 
                         group = `Electricity.Consumption`)) +
  geom_line(linewidth = 1.1) +
  labs(title = "Electricity Consumption in Ireland",
       subtitle = "Data Centres vs. Other Customers",
       y = "Consumption (GWh)", x = "Quarter") +
  theme_minimal()

#histo of data centre share
ggplot(cso_wide, aes(x = data_centre_share)) +
  geom_histogram(binwidth = 0.02, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Data Centre Share of Electricity Consumption",
       x = "Data Centre Share of Total", y = "Count") +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_minimal()


#density of data centre share
cso_wide %>%
  select(data_centre_share, other_share) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "Share") %>%
  ggplot(aes(x = Share, fill = Type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Data Centre vs. Other Customers Share",
       x = "Share of Total Consumption", y = "Density") +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_minimal()

##boxplot
cso_wide %>%
  select(data_centre_share, other_share) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "Share") %>%
  ggplot(aes(x = Type, y = Share, fill = Type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot of Electricity Consumption Shares",
       x = "", y = "Share of Total Consumption") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

typeof(cso_clean$Quarter)

consumption_summary <- cso_clean %>%
  group_by(Quarter, `Electricity.Consumption`) %>%
  summarise(total_GWh = sum(VALUE, na.rm = TRUE)) %>%
  arrange(Quarter, `Electricity.Consumption`)

consumption_summary

knitr::kable(consumption_summary, align = "lc")

annual_consumption <- cso_clean %>%
  group_by(Year, `Electricity.Consumption`) %>%
  summarise(total_GWh = sum(VALUE, na.rm = TRUE), .groups = "drop")

# Wide format table
annual_consumption_wide <- annual_consumption %>%
  tidyr::pivot_wider(names_from = `Electricity.Consumption`, values_from = total_GWh)

print(annual_consumption_wide)

annual_consumption_data_centre <- annual_consumption_wide %>%
  select(-`All metered electricity consumption`,-`Customers other than data centres`)

ggplot(annual_consumption_data_centre, aes(x = factor(Year), 
                                y = `Data centres`, 
                                fill = `Data centres`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Electricity Consumption by Data centres",
       x = "Year", y = "Total Consumption (GWh)", fill = "Total Consumption (GWh)") +
  theme_classic() 

annual_consumption_2cat <- annual_consumption_wide %>%
  select(-`All metered electricity consumption`)

annual_consumption_2cat_wide <- annual_consumption_2cat %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Consumption")

ggplot(annual_consumption_2cat_wide, aes(x = factor(Year), y = Consumption, fill = Category)) +
  geom_col(position = "dodge") +
  labs(title = "Electricity Consumption by Category",
       subtitle = "Data centres vs Other customers",
       x = "Year", y = "Consumption (GWh)", fill = "Category") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Data centres" = "#1f77b4", 
                               "Customers other than data centres" = "#aec7e8"))

ggplot(annual_consumption_2cat_wide, aes(x = Year, y = Consumption, color = Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Trends in Electricity Consumption",
       x = "Year", y = "Consumption (GWh)", color = "Category") +
  theme_minimal()

ggplot(consumption_summary, aes(x = factor(Quarter), 
                               y = total_GWh, 
                               fill = `Electricity.Consumption`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Quarter wise Electricity Consumption by Category",
       x = "Year", y = "Total Consumption (GWh)", fill = "Category") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(dplyr)

# Step 1: get unique quarters sorted
quarters_levels <- consumption_summary %>%
  distinct(Quarter) %>%
  arrange(Quarter) %>%
  pull(Quarter)

# Step 2: collapse only to two categories
consumption_two <- consumption_summary %>%
  filter(`Electricity.Consumption` %in% c("Data centres", "Customers other than data centres")) %>%
  mutate(Quarter = factor(Quarter, levels = quarters_levels))

# Step 3: keep totals separately
total_by_quarter <- consumption_summary %>%
  filter(`Electricity.Consumption` == "All metered electricity consumption") %>%
  select(Quarter, total_GWh_total = total_GWh) %>%
  mutate(Quarter = factor(Quarter, levels = quarters_levels))

ggplot(consumption_two, aes(x = Quarter, y = total_GWh, fill = `Electricity.Consumption`)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Quarter-wise Electricity Consumption (GWh)",
       subtitle = "Data centres vs Customers other than data centres",
       x = "Quarter", y = "Total Consumption (GWh)", fill = "Category") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
consumption_two

data_centre_share <- data.frame(
  Year = 2016:2024,
  DataCentreShare = c(0.058, 0.068, 0.081, 0.094, 0.112, 0.141, 0.177, 0.207, 0.219)
)


ict_gva_share <- data.frame(
  Year = 2016:2024,
  ICT_GVA_Share = c(0.154, 0.167, 0.188, 0.196, 0.224, 0.257, 0.242, 0.266, 0.283)
)

ratio_df <- left_join(data_centre_share, ict_gva_share, by = "Year")


ggplot# Make sure Year is integer
ratio_df <- ratio_df %>% mutate(Year = as.integer(Year))

# If your shares are proportions (0.05 = 5%), multiply by 100 for percent; 
# if they're already in percent (5 or 5.0), skip the *100
ggplot(ratio_df, aes(x = Year)) +
  geom_line(aes(y = DataCentreShare * 100, color = "Data Centres - Electricity Demand"),
            linewidth = 1.2) +
  geom_line(aes(y = ICT_GVA_Share * 100, color = "ICT Sector - GDP Share"),
            linewidth = 1.2, linetype = "dashed") +
  scale_x_continuous(
    breaks = seq(min(ratio_df$Year), max(ratio_df$Year), by = 1),   # one tick per year
    labels = seq(min(ratio_df$Year), max(ratio_df$Year), by = 1)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = c("Data Centres - Electricity Demand" = "#1f77b4",
                                "ICT Sector - GDP Share" = "#66b2ff")) +
  labs(title = "Economic vs Energy Footprint of Dublin’s ICT Sector",
       subtitle = "% of GDP vs % of Electricity Demand",
       x = "Year", y = "Share (%)", color = "Indicator") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create the data frame
data <- data.frame(
  Year = c(2021, 2022, 2023, 2024),
  EPR = c(30.13915212, 41.61088977, 42.07559975, 45.85665088)
)

# Create the line chart
ggplot(data, aes(x = Year, y = EPR)) +
  geom_line(color = "#1f77b4", size = 1.5) +  # Line color and thickness
  geom_point(color = "#ff7f0e", size = 3) +   # Highlight points
  geom_text(aes(label = round(EPR, 2)), vjust = -1, size = 4) + # Add labels
  scale_x_continuous(breaks = data$Year) +   # Ensure all years appear
  labs(
    title = "EPR Trend Over Years",
    x = "Year",
    y = "EPR"
  ) +
  theme_minimal() +            # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text()
  )
