
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("corrplot")
# install.packages("scales")
# install.packages("ggcorrplot")


library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(scales)
library(ggcorrplot)

cat("All R Analytics libraries loaded!\n")



deliveries <- read.csv("deliveries.csv",  stringsAsFactors = FALSE)
orders     <- read.csv("orders.csv",      stringsAsFactors = FALSE)
drivers    <- read.csv("drivers.csv",     stringsAsFactors = FALSE)
customers  <- read.csv("customers.csv",   stringsAsFactors = FALSE)
hubs       <- read.csv("hubs.csv",        stringsAsFactors = FALSE)
complaints <- read.csv("complaints.csv",  stringsAsFactors = FALSE)
incidents  <- read.csv("incidents.csv",   stringsAsFactors = FALSE)
vehicles   <- read.csv("vehicles.csv",    stringsAsFactors = FALSE)
app_events <- read.csv("app_events.csv",  stringsAsFactors = FALSE)

cat("All datasets loaded!\n")
cat("Deliveries:", nrow(deliveries), "rows\n")
cat("Customers :", nrow(customers),  "rows\n")
cat("Drivers   :", nrow(drivers),    "rows\n")



# --- Check missing values ---
cat("\nMissing values per column in deliveries:\n")
print(colSums(is.na(deliveries)))

cat("\nMissing values per column in customers:\n")
print(colSums(is.na(customers)))

# --- Standardise delivery_status column (trim whitespace) ---
deliveries$delivery_status <- trimws(deliveries$delivery_status)
drivers$base_zone          <- trimws(drivers$base_zone)
customers$home_zone        <- trimws(customers$home_zone)

# --- Convert date columns to proper Date/POSIXct format ---
deliveries$dispatch_time <- as.POSIXct(deliveries$dispatch_time,
                                        format = "%Y-%m-%d %H:%M:%S")
deliveries$delivery_completed_at <- as.POSIXct(deliveries$delivery_completed_at,
                                                 format = "%Y-%m-%d %H:%M:%S")

# --- Calculate actual delivery duration in hours ---
deliveries$delivery_hours <- as.numeric(
  difftime(deliveries$delivery_completed_at,
           deliveries$dispatch_time,
           units = "hours")
)

cat("\nDelivery duration (hours) preview:\n")
print(summary(deliveries$delivery_hours))

# --- Create binary flag: 1 = failed or delayed, 0 = on time ---
deliveries$problem_flag <- ifelse(
  deliveries$delivery_status %in% c("Failed", "Delayed"), 1, 0
)

# --- Create high-override flag ---
deliveries$high_override <- ifelse(
  deliveries$manual_route_override_count >= 2, "High Override", "Normal"
)

cat("\nData cleaning complete!\n")


# --- Full summary of key numeric columns ---
cat("\n--- Summary: Deliveries (key numeric columns) ---\n")
print(summary(deliveries[, c(
  "route_distance_km",
  "fuel_or_charge_cost",
  "customer_rating_post_delivery",
  "manual_route_override_count"
)]))

# --- Standard deviations ---
cat("\n--- Standard Deviations ---\n")
sd_results <- sapply(
  deliveries[, c("route_distance_km",
                 "fuel_or_charge_cost",
                 "customer_rating_post_delivery")],
  sd, na.rm = TRUE
)
print(round(sd_results, 4))

# --- Group statistics: delivery status ---
cat("\n--- Group Statistics by Delivery Status ---\n")
group_stats <- deliveries %>%
  group_by(delivery_status) %>%
  summarise(
    count         = n(),
    mean_rating   = round(mean(customer_rating_post_delivery, na.rm = TRUE), 2),
    sd_rating     = round(sd(customer_rating_post_delivery,   na.rm = TRUE), 2),
    median_rating = round(median(customer_rating_post_delivery, na.rm = TRUE), 2),
    mean_cost     = round(mean(fuel_or_charge_cost,           na.rm = TRUE), 2),
    mean_distance = round(mean(route_distance_km,             na.rm = TRUE), 2),
    mean_overrides = round(mean(manual_route_override_count,  na.rm = TRUE), 2),
    .groups = "drop"
  )
print(group_stats)

# --- Driver statistics ---
cat("\n--- Summary: Driver Ratings ---\n")
print(summary(drivers$driver_rating))

cat("\n--- Summary: Driver Experience (years) ---\n")
print(summary(drivers$years_experience))

# --- Complaint resolution time ---
cat("\n--- Summary: Complaint Resolution Days ---\n")
print(summary(complaints$resolution_days))

cat("\n--- Average Compensation by Complaint Type ---\n")
comp_summary <- complaints %>%
  group_by(complaint_type) %>%
  summarise(
    count       = n(),
    avg_days    = round(mean(resolution_days, na.rm = TRUE), 1),
    avg_comp    = round(mean(compensation_amount, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(count))
print(comp_summary)


cat("\n============================================================\n")
cat("SECTION 3: Statistical Tests\n")
cat("============================================================\n")


cat("\n--- T-Test: Customer Rating (OnTime vs Failed) ---\n")
ontime_ratings <- deliveries$customer_rating_post_delivery[
  deliveries$delivery_status == "OnTime"
]
failed_ratings <- deliveries$customer_rating_post_delivery[
  deliveries$delivery_status == "Failed"
]

t_result <- t.test(ontime_ratings, failed_ratings)
print(t_result)

cat("\n--- T-Test Interpretation ---\n")
if (t_result$p.value < 0.05) {
  cat("p-value =", round(t_result$p.value, 6), "-> SIGNIFICANT difference!\n")
  cat("OnTime mean rating:", round(mean(ontime_ratings, na.rm=TRUE), 3), "\n")
  cat("Failed mean rating:", round(mean(failed_ratings, na.rm=TRUE), 3), "\n")
} else {
  cat("p-value =", round(t_result$p.value, 6), "-> No significant difference\n")
}

cat("\n--- ANOVA: Rating differences across OnTime, Delayed, Failed ---\n")
anova_model <- aov(customer_rating_post_delivery ~ delivery_status,
                   data = deliveries)
print(summary(anova_model))


cat("\n--- Correlation: Driver Rating vs Customer Rating ---\n")
merged_df <- deliveries %>%
  left_join(drivers, by = "driver_id") %>%
  filter(!is.na(driver_rating),
         !is.na(customer_rating_post_delivery))

cor_val <- cor(merged_df$driver_rating,
               merged_df$customer_rating_post_delivery,
               use = "complete.obs")
cat("Pearson correlation (driver_rating vs customer_rating):",
    round(cor_val, 4), "\n")


cat("\n--- Linear Regression: Predicting Customer Rating ---\n")
reg_model <- lm(
  customer_rating_post_delivery ~
    driver_rating +
    years_experience +
    training_score +
    manual_route_override_count,
  data = merged_df
)
print(summary(reg_model))




# Prepare numeric columns for correlation
cor_data <- merged_df %>%
  mutate(is_failed = ifelse(delivery_status == "Failed", 1, 0)) %>%
  select(
    driver_rating,
    years_experience,
    training_score,
    manual_route_override_count,
    customer_rating_post_delivery,
    fuel_or_charge_cost,
    route_distance_km,
    is_failed
  ) %>%
  na.omit()

cor_matrix <- cor(cor_data)
cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 3))

# --- Plot correlation matrix ---
png("correlation_matrix.png", width = 900, height = 800, res = 120)
ggcorrplot(
  cor_matrix,
  method    = "circle",
  type      = "lower",
  lab       = TRUE,
  lab_size  = 3,
  colors    = c("#E74C3C", "white", "#2E86C1"),
  title     = "NorthStar: Correlation Matrix",
  ggtheme   = theme_minimal()
)
dev.off()
cat("Correlation matrix chart saved: correlation_matrix.png\n")




# --- CHART 1: Delivery Status Bar Chart ---
cat("Creating Chart 1: Delivery Status Distribution...\n")

status_df <- deliveries %>%
  group_by(delivery_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

chart1 <- ggplot(status_df,
                 aes(x = reorder(delivery_status, -count),
                     y = count,
                     fill = delivery_status)) +
  geom_bar(stat = "identity", width = 0.55) +
  geom_text(aes(label = paste0(count, "\n(", percentage, "%)")),
            vjust = -0.3, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "OnTime"  = "#2E86C1",
    "Delayed" = "#F39C12",
    "Failed"  = "#E74C3C"
  )) +
  labs(
    title    = "NorthStar: Delivery Status Distribution",
    subtitle = paste("Total deliveries:", nrow(deliveries)),
    x        = "Delivery Status",
    y        = "Number of Deliveries",
    caption  = "Source: NorthStar Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("chart1_delivery_status.png", chart1, width = 7, height = 5, dpi = 150)
print(chart1)
cat("Chart 1 saved!\n")


# --- CHART 2: Boxplot - Customer Rating by Delivery Status ---
cat("Creating Chart 2: Customer Rating Boxplot...\n")

chart2 <- ggplot(deliveries,
                 aes(x    = delivery_status,
                     y    = customer_rating_post_delivery,
                     fill = delivery_status)) +
  geom_boxplot(alpha = 0.75,
               outlier.colour = "red",
               outlier.size   = 2,
               outlier.shape  = 16) +
  scale_fill_manual(values = c(
    "OnTime"  = "#2E86C1",
    "Delayed" = "#F39C12",
    "Failed"  = "#E74C3C"
  )) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "black") +
  labs(
    title    = "Customer Rating Distribution by Delivery Status",
    subtitle = "Diamond = Mean | Box = IQR | Line = Median",
    x        = "Delivery Status",
    y        = "Customer Rating (1 to 5)",
    caption  = "Source: NorthStar Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("chart2_rating_boxplot.png", chart2, width = 7, height = 5, dpi = 150)
print(chart2)
cat("Chart 2 saved!\n")


# --- CHART 3: Manual Route Overrides vs Delivery Outcome ---
cat("Creating Chart 3: Route Override Analysis...\n")

override_df <- deliveries %>%
  group_by(delivery_status, manual_route_override_count) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(manual_route_override_count <= 4)

chart3 <- ggplot(override_df,
                 aes(x    = factor(manual_route_override_count),
                     y    = count,
                     fill = delivery_status)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.65) +
  scale_fill_manual(values = c(
    "OnTime"  = "#2E86C1",
    "Delayed" = "#F39C12",
    "Failed"  = "#E74C3C"
  )) +
  labs(
    title    = "Manual Route Overrides vs Delivery Outcome",
    subtitle = "Higher overrides are linked to more failures",
    x        = "Number of Manual Route Overrides",
    y        = "Number of Deliveries",
    fill     = "Status",
    caption  = "Source: NorthStar Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("chart3_route_overrides.png", chart3, width = 9, height = 5, dpi = 150)
print(chart3)
cat("Chart 3 saved!\n")


# --- CHART 4: Driver Rating vs Customer Rating (Scatter Plot) ---
cat("Creating Chart 4: Driver Rating vs Customer Rating Scatter...\n")

chart4 <- ggplot(merged_df,
                 aes(x      = driver_rating,
                     y      = customer_rating_post_delivery,
                     colour = delivery_status)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE,
              colour = "black", linewidth = 1) +
  scale_colour_manual(values = c(
    "OnTime"  = "#2E86C1",
    "Delayed" = "#F39C12",
    "Failed"  = "#E74C3C"
  )) +
  labs(
    title    = "Driver Rating vs Customer Rating",
    subtitle = paste("Pearson r =", round(cor_val, 3)),
    x        = "Driver Rating",
    y        = "Customer Rating (post-delivery)",
    colour   = "Status",
    caption  = "Source: NorthStar Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("chart4_driver_vs_customer_rating.png", chart4,
       width = 8, height = 5, dpi = 150)
print(chart4)
cat("Chart 4 saved!\n")


# --- CHART 5: Complaint Type Frequency Bar Chart ---
cat("Creating Chart 5: Complaint Types...\n")

complaint_df <- complaints %>%
  group_by(complaint_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

chart5 <- ggplot(complaint_df,
                 aes(x    = reorder(complaint_type, count),
                     y    = count,
                     fill = count)) +
  geom_bar(stat = "identity", width = 0.65) +
  geom_text(aes(label = count), hjust = -0.3, size = 4.2) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276") +
  coord_flip() +
  labs(
    title    = "NorthStar: Complaint Type Frequency",
    subtitle = paste("Total complaints:", nrow(complaints)),
    x        = "Complaint Type",
    y        = "Number of Complaints",
    caption  = "Source: NorthStar Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("chart5_complaint_types.png", chart5, width = 8, height = 5, dpi = 150)
print(chart5)
cat("Chart 5 saved!\n")


# --- CHART 6: Average Cost by Hub (Bar Chart) ---
cat("Creating Chart 6: Average Cost by Hub...\n")

hub_cost <- deliveries %>%
  left_join(hubs, by = "hub_id") %>%
  group_by(hub_id, hub_name) %>%
  summarise(avg_cost = round(mean(fuel_or_charge_cost, na.rm = TRUE), 2),
            total    = n(),
            .groups  = "drop") %>%
  arrange(desc(avg_cost))

chart6 <- ggplot(hub_cost,
                 aes(x    = reorder(hub_name, avg_cost),
                     y    = avg_cost,
                     fill = avg_cost)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0("£", avg_cost)),
            hjust = -0.2, size = 4) +
  scale_fill_gradient(low = "#A9DFBF", high = "#1E8449") +
  coord_flip() +
  labs(
    title    = "Average Fuel / Charge Cost per Hub",
    subtitle = "Higher cost hubs may indicate operational inefficiency",
    x        = "Hub Name",
    y        = "Average Cost (£)",
    caption  = "Source: NorthStar Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("chart6_hub_cost.png", chart6, width = 8, height = 5, dpi = 150)
print(chart6)
cat("Chart 6 saved!\n")


# --- CHART 7: Incident Type Frequency ---
cat("Creating Chart 7: Incident Types...\n")

incident_df <- incidents %>%
  group_by(incident_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

chart7 <- ggplot(incident_df,
                 aes(x    = reorder(incident_type, count),
                     y    = count,
                     fill = incident_type)) +
  geom_bar(stat = "identity", width = 0.65) +
  geom_text(aes(label = count), hjust = -0.3, size = 4.2) +
  coord_flip() +
  labs(
    title    = "NorthStar: Incident Type Frequency",
    subtitle = paste("Total incidents:", nrow(incidents)),
    x        = "Incident Type",
    y        = "Number of Incidents",
    caption  = "Source: NorthStar Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("chart7_incident_types.png", chart7, width = 8, height = 5, dpi = 150)
print(chart7)
cat("Chart 7 saved!\n")


# --- CHART 8: Histogram of Customer Ratings ---
cat("Creating Chart 8: Customer Rating Histogram...\n")

chart8 <- ggplot(deliveries,
                 aes(x = customer_rating_post_delivery,
                     fill = delivery_status)) +
  geom_histogram(binwidth = 0.25, colour = "white", alpha = 0.85) +
  scale_fill_manual(values = c(
    "OnTime"  = "#2E86C1",
    "Delayed" = "#F39C12",
    "Failed"  = "#E74C3C"
  )) +
  geom_vline(xintercept = mean(deliveries$customer_rating_post_delivery,
                                na.rm = TRUE),
             colour = "black", linetype = "dashed", linewidth = 1) +
  labs(
    title    = "Distribution of Customer Ratings",
    subtitle = paste("Dashed line = overall mean:",
                     round(mean(deliveries$customer_rating_post_delivery,
                                na.rm = TRUE), 2)),
    x        = "Customer Rating",
    y        = "Count",
    fill     = "Status",
    caption  = "Source: NorthStar Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("chart8_rating_histogram.png", chart8, width = 8, height = 5, dpi = 150)
print(chart8)
cat("Chart 8 saved!\n")



# --- Top 10 drivers with most failures ---
cat("\n--- Top 10 Drivers: Most Delivery Failures ---\n")
driver_failures <- deliveries %>%
  filter(delivery_status == "Failed") %>%
  group_by(driver_id) %>%
  summarise(failures = n(), .groups = "drop") %>%
  left_join(drivers %>% select(driver_id, base_zone, driver_rating,
                               years_experience, employment_type),
            by = "driver_id") %>%
  arrange(desc(failures)) %>%
  head(10)
print(driver_failures)

# --- Vehicles with most incidents ---
cat("\n--- Vehicles Linked to Most Incidents ---\n")
vehicle_incidents <- incidents %>%
  left_join(deliveries %>% select(delivery_id, vehicle_id), by = "delivery_id") %>%
  group_by(vehicle_id) %>%
  summarise(incident_count = n(), .groups = "drop") %>%
  left_join(vehicles %>% select(vehicle_id, vehicle_type, maintenance_status),
            by = "vehicle_id") %>%
  arrange(desc(incident_count)) %>%
  head(10)
print(vehicle_incidents)

# --- Zone level performance summary ---
cat("\n--- Zone-Level Performance (Driver Base Zone) ---\n")
zone_perf <- deliveries %>%
  left_join(drivers %>% select(driver_id, base_zone), by = "driver_id") %>%
  group_by(base_zone) %>%
  summarise(
    total         = n(),
    on_time       = sum(delivery_status == "OnTime"),
    failures      = sum(delivery_status == "Failed"),
    failure_rate  = round(sum(delivery_status == "Failed") / n() * 100, 1),
    avg_rating    = round(mean(customer_rating_post_delivery, na.rm = TRUE), 2),
    .groups       = "drop"
  ) %>%
  arrange(desc(failure_rate))
print(zone_perf)


cat("\n============================================================\n")
cat("R ANALYTICS COMPLETE - Key Findings\n")
cat("============================================================\n")
cat("Total deliveries          :", nrow(deliveries), "\n")
cat("On Time                   :", sum(deliveries$delivery_status == "OnTime"), "\n")
cat("Delayed                   :", sum(deliveries$delivery_status == "Delayed"), "\n")
cat("Failed                    :", sum(deliveries$delivery_status == "Failed"), "\n")
cat("------------------------------------------------------------\n")
cat("Avg rating (OnTime)       :", round(mean(deliveries$customer_rating_post_delivery[deliveries$delivery_status=="OnTime"],  na.rm=TRUE), 3), "\n")
cat("Avg rating (Delayed)      :", round(mean(deliveries$customer_rating_post_delivery[deliveries$delivery_status=="Delayed"], na.rm=TRUE), 3), "\n")
cat("Avg rating (Failed)       :", round(mean(deliveries$customer_rating_post_delivery[deliveries$delivery_status=="Failed"],  na.rm=TRUE), 3), "\n")
cat("------------------------------------------------------------\n")
cat("Driver rating mean        :", round(mean(drivers$driver_rating, na.rm=TRUE), 3), "\n")
cat("Driver rating SD          :", round(sd(drivers$driver_rating, na.rm=TRUE), 3), "\n")
cat("Total complaints          :", nrow(complaints), "\n")
cat("Total incidents           :", nrow(incidents), "\n")
cat("Correlation (driver vs cust rating):", round(cor_val, 4), "\n")
cat("T-test p-value (OnTime vs Failed)  :", round(t_result$p.value, 6), "\n")
cat("------------------------------------------------------------\n")
cat("Charts saved (8 PNG files in working directory)\n")
cat("============================================================\n")
