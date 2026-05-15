
# install.packages("sqldf")
# install.packages("RSQLite")
# install.packages("DBI")
# install.packages("dplyr")
# install.packages("ggplot2")



library(sqldf)
library(RSQLite)
library(DBI)
library(dplyr)
library(ggplot2)

cat("All libraries loaded successfully!\n")


# WINDOWS example:  setwd("C:/Users/YourName/NorthStar")
# MAC example:      setwd("/Users/YourName/NorthStar")

# OR use RStudio menu: Session -> Set Working Directory -> To Source File Location
# That will automatically set the folder where this script is saved

cat("Working directory:", getwd(), "\n")



deliveries <- read.csv("deliveries.csv",  stringsAsFactors = FALSE)
orders     <- read.csv("orders.csv",      stringsAsFactors = FALSE)
drivers    <- read.csv("drivers.csv",     stringsAsFactors = FALSE)
customers  <- read.csv("customers.csv",   stringsAsFactors = FALSE)
hubs       <- read.csv("hubs.csv",        stringsAsFactors = FALSE)
complaints <- read.csv("complaints.csv",  stringsAsFactors = FALSE)
incidents  <- read.csv("incidents.csv",   stringsAsFactors = FALSE)
vehicles   <- read.csv("vehicles.csv",    stringsAsFactors = FALSE)

cat("\n--- Data Loaded Successfully ---\n")
cat("deliveries : ", nrow(deliveries),  "rows\n")
cat("orders     : ", nrow(orders),      "rows\n")
cat("drivers    : ", nrow(drivers),     "rows\n")
cat("customers  : ", nrow(customers),   "rows\n")
cat("hubs       : ", nrow(hubs),        "rows\n")
cat("complaints : ", nrow(complaints),  "rows\n")
cat("incidents  : ", nrow(incidents),   "rows\n")
cat("vehicles   : ", nrow(vehicles),    "rows\n")

# Quick preview of deliveries
cat("\n--- Preview: deliveries ---\n")
print(head(deliveries, 3))

cat("\n--- Column Names: deliveries ---\n")
print(names(deliveries))


cat("\n============================================================\n")
cat("QUERY 1: Delivery Status Distribution\n")
cat("============================================================\n")

query1 <- sqldf("
  SELECT
    delivery_status,
    COUNT(*) AS total_count,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM deliveries), 1) AS percentage
  FROM deliveries
  GROUP BY delivery_status
  ORDER BY total_count DESC
")

print(query1)

# Interpretation:
# OnTime  = 616  (64.8%) -> good
# Delayed = 202  (21.3%) -> needs attention
# Failed  = 132  (13.9%) -> serious problem



query2 <- sqldf("
  SELECT
    d.hub_id,
    h.hub_name,
    h.zone,
    COUNT(*)                              AS failed_count,
    ROUND(AVG(d.fuel_or_charge_cost), 2)  AS avg_cost,
    ROUND(AVG(d.route_distance_km), 2)    AS avg_distance_km,
    ROUND(AVG(d.customer_rating_post_delivery), 2) AS avg_rating
  FROM deliveries d
  JOIN hubs h ON d.hub_id = h.hub_id
  WHERE d.delivery_status = 'Failed'
  GROUP BY d.hub_id, h.hub_name, h.zone
  ORDER BY failed_count DESC
")

print(query2)



query3 <- sqldf("
  SELECT
    dr.driver_id,
    dr.base_zone,
    dr.employment_type,
    ROUND(dr.driver_rating, 2)             AS driver_rating,
    dr.years_experience,
    COUNT(d.delivery_id)                   AS total_deliveries,
    SUM(CASE WHEN d.delivery_status = 'Failed'  THEN 1 ELSE 0 END) AS failures,
    SUM(CASE WHEN d.delivery_status = 'Delayed' THEN 1 ELSE 0 END) AS delays,
    ROUND(AVG(d.customer_rating_post_delivery), 2) AS avg_customer_rating
  FROM drivers dr
  LEFT JOIN deliveries d ON dr.driver_id = d.driver_id
  GROUP BY dr.driver_id, dr.base_zone, dr.employment_type,
           dr.driver_rating, dr.years_experience
  HAVING total_deliveries > 0
  ORDER BY failures DESC
  LIMIT 15
")

print(query3)


query4 <- sqldf("
  SELECT
    delivery_status,
    COUNT(*)                                    AS total,
    ROUND(AVG(fuel_or_charge_cost), 2)          AS avg_cost,
    ROUND(AVG(route_distance_km), 2)            AS avg_distance_km,
    ROUND(AVG(customer_rating_post_delivery), 2) AS avg_rating,
    SUM(manual_route_override_count)            AS total_overrides
  FROM deliveries
  GROUP BY delivery_status
  ORDER BY avg_cost DESC
")

print(query4)


query5 <- sqldf("
  SELECT
    c.complaint_type,
    c.severity,
    c.channel,
    COUNT(*)                            AS complaint_count,
    ROUND(AVG(c.resolution_days), 1)    AS avg_resolution_days,
    ROUND(AVG(c.compensation_amount), 2) AS avg_compensation
  FROM complaints c
  GROUP BY c.complaint_type, c.severity, c.channel
  ORDER BY complaint_count DESC
  LIMIT 15
")

print(query5)


query6 <- sqldf("
  SELECT
    v.maintenance_status,
    v.vehicle_type,
    COUNT(DISTINCT v.vehicle_id)        AS vehicle_count,
    COUNT(i.incident_id)                AS total_incidents,
    ROUND(AVG(i.resolved_hours), 1)     AS avg_resolve_hours
  FROM vehicles v
  LEFT JOIN deliveries d  ON v.vehicle_id = d.vehicle_id
  LEFT JOIN incidents  i  ON d.delivery_id = i.delivery_id
  GROUP BY v.maintenance_status, v.vehicle_type
  ORDER BY total_incidents DESC
")

print(query6)


# --- Create in-memory SQLite database ---
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# --- Load tables ---
dbWriteTable(con, "deliveries", deliveries)
dbWriteTable(con, "hubs",       hubs)
dbWriteTable(con, "drivers",    drivers)
dbWriteTable(con, "orders",     orders)
dbWriteTable(con, "complaints", complaints)

cat("Tables loaded into SQLite:\n")
print(dbListTables(con))


# --- CHECK query plan WITHOUT index ---
cat("\n--- Query Plan WITHOUT Index ---\n")
plan_before <- dbGetQuery(con, "
  EXPLAIN QUERY PLAN
  SELECT hub_id, COUNT(*) AS cnt
  FROM deliveries
  WHERE delivery_status = 'Failed'
  GROUP BY hub_id
")
print(plan_before)
# You should see: SCAN deliveries  (meaning it reads ALL rows - slow!)


# --- CREATE indexes ---
dbExecute(con, "CREATE INDEX idx_delivery_status ON deliveries(delivery_status)")
dbExecute(con, "CREATE INDEX idx_hub_id          ON deliveries(hub_id)")
dbExecute(con, "CREATE INDEX idx_driver_id       ON deliveries(driver_id)")

cat("\nIndexes created successfully!\n")


# --- CHECK query plan WITH index ---
cat("\n--- Query Plan WITH Index ---\n")
plan_after <- dbGetQuery(con, "
  EXPLAIN QUERY PLAN
  SELECT hub_id, COUNT(*) AS cnt
  FROM deliveries
  WHERE delivery_status = 'Failed'
  GROUP BY hub_id
")
print(plan_after)
# You should now see: SEARCH deliveries USING INDEX idx_delivery_status  (much faster!)


# --- Run optimised query ---
cat("\n--- Optimised Query Result ---\n")
opt_result <- dbGetQuery(con, "
  SELECT
    d.hub_id,
    h.hub_name,
    h.zone,
    COUNT(*)                              AS total_failures,
    ROUND(AVG(d.fuel_or_charge_cost), 2)  AS avg_cost,
    ROUND(AVG(d.route_distance_km), 2)    AS avg_distance
  FROM deliveries d
  JOIN hubs h ON d.hub_id = h.hub_id
  WHERE d.delivery_status = 'Failed'
  GROUP BY d.hub_id, h.hub_name, h.zone
  ORDER BY total_failures DESC
")
print(opt_result)


# --- Close connection ---
dbDisconnect(con)
cat("\nSQLite connection closed.\n")



status_df <- deliveries %>%
  group_by(delivery_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

ggplot(status_df, aes(x = reorder(delivery_status, -count),
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
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey40"))

# Save the chart
ggsave("delivery_status_chart.png", width = 7, height = 5, dpi = 150)
cat("Chart saved as: delivery_status_chart.png\n")



cat("\n============================================================\n")
cat("ANALYSIS COMPLETE - Key Findings\n")
cat("============================================================\n")
cat("Total deliveries      :", nrow(deliveries), "\n")
cat("On Time               :", sum(deliveries$delivery_status == "OnTime"),
    paste0("(", round(mean(deliveries$delivery_status == "OnTime")*100, 1), "%)"), "\n")
cat("Delayed               :", sum(deliveries$delivery_status == "Delayed"),
    paste0("(", round(mean(deliveries$delivery_status == "Delayed")*100, 1), "%)"), "\n")
cat("Failed                :", sum(deliveries$delivery_status == "Failed"),
    paste0("(", round(mean(deliveries$delivery_status == "Failed")*100, 1), "%)"), "\n")
cat("Avg rating (OnTime)   :", round(mean(deliveries$customer_rating_post_delivery[deliveries$delivery_status=="OnTime"],  na.rm=TRUE), 2), "\n")
cat("Avg rating (Failed)   :", round(mean(deliveries$customer_rating_post_delivery[deliveries$delivery_status=="Failed"],  na.rm=TRUE), 2), "\n")
cat("Total complaints      :", nrow(complaints), "\n")
cat("Total incidents       :", nrow(incidents),  "\n")
cat("============================================================\n")
