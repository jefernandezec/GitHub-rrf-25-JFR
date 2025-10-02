# Reproducible Research Fundamentals 
# 03. Data Analysis


# Load data -----
#household level data
hh_data   <- read_dta(file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# secondary data 
# secondary_data <- read_dta(file.path(data_path, "Final/TZA_amenity_analysis.dta")) %>%
#     mutate(district = as_factor(district))
# 
# # Exercise 1 and 2: Create graph of area by district -----

# Bar graph by treatment for all districts
# Ensure treatment is a factor for proper labeling
hh_data_plot <- hh_data %>%
    mutate(treatment = factor(treatment, labels = c("Control", "Treatment")), 
           district = as_factor(district))

# Create the bar plot
# Create the bar plot
ggplot(hh_data_plot, aes(x = district, y = area_acre_w, fill = treatment)) +
  geom_bar(stat = "summary", fun = "mean", 
           position = position_dodge(width = 0.9), color = "black") +
  stat_summary(fun = "mean", geom = "text",
               aes(label = sprintf("%.1f", ..y..)),
               position = position_dodge(width = 0.9), 
               vjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Set2", name = "Treatment Group") +  # formal color palette
  labs(
    title = "Average Area (in Acres) by District and Treatment",
    x = "District",
    y = "Average Area (Acres)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
       ggsave(file.path("Outputs", "fig1.png"), width = 10, height = 6)
       
       
# Exercise 3: Create a density plot of non-food consumption -----
       
# Calculate mean non-food consumption for female and male-headed households
mean_female <- hh_data %>% 
   filter(female_head == 1) %>% 
   summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
   pull(mean)

mean_male <- hh_data %>% 
   filter(female_head == 0) %>% 
   summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
   pull(mean)

# # Create the density plot
# ggplot(hh_data, 
#       aes(......)) +
#    geom_density(......) +  # Density plot
#    geom_vline(xintercept = ......, color = "purple", linetype = "dashed", size = 1) +  # Vertical line for female mean
#    geom_vline(xintercept = ......, color = "grey", linetype = "dashed", size = 1) +  # Vertical line for male mean
#    labs(title = "Distribution of Non-Food Consumption",
#         x = "Non-food consumption value (USD)", 
#         y = "Density",
#         color = "Household Head:") +  # Custom labels
#    theme_minimal() +
#    ...... # Add other customization if needed
# 
# ggsave(file.path("Outputs", "fig2.png"), width = 10, height = 6)
#        

# Exercise 4: Summary statistics ----

# Create summary statistics by district and export to CSV
library(dplyr)

# Example: summarize 5 numeric variables by district
summary_tbl <- hh_data_plot %>%
  group_by(district) %>%
  summarise(
    mean_area   = mean(area_acre_w, na.rm = TRUE),
    sd_area     = sd(area_acre_w, na.rm = TRUE),
    mean_sick   = mean(days_sick, na.rm = TRUE),
    sd_sick     = sd(days_sick, na.rm = TRUE),
    mean_food   = mean(food_cons_usd, na.rm = TRUE),
    sd_food     = sd(food_cons_usd, na.rm = TRUE),
    mean_nfood   = mean(nonfood_cons_usd, na.rm = TRUE),
    sd_nfood     = sd(nonfood_cons_usd, na.rm = TRUE),
    mean_duration   = mean(duration, na.rm = TRUE),
    sd_duration     = sd(duration, na.rm = TRUE)
  )

#alternative
vars <- c("area_acre_w", "days_sick", "food_cons_usd", 
          "nonfood_cons_usd", "duration")

# Mean
mean_tbl <- aggregate(hh_data_plot[vars],
                      by = list(District = hh_data_plot$district),
                      FUN = function(x) mean(x, na.rm = TRUE))

# SD
sd_tbl <- aggregate(hh_data_plot[vars],
                    by = list(District = hh_data_plot$district),
                    FUN = function(x) sd(x, na.rm = TRUE))

# Merge mean & sd results
summary_tbl <- merge(mean_tbl, sd_tbl,
                     by = "District",
                     suffixes = c("_mean", "_sd"))


# Save to CSV
write.csv(summary_tbl, 
      file.path("Outputs","household_summary_by_district.csv"), 
      row.names = FALSE)


# Exercise 5: Balance table ----
# Mean
bal_tbl <- t(aggregate(hh_data_plot[vars],
                      by = list(treatment = hh_data_plot$treatment),
                      FUN = function(x) mean(x, na.rm = TRUE)))

vars <- c("area_acre_w", "days_sick", "food_cons_usd", 
          "nonfood_cons_usd", "duration")

bal_tbl = hh_data_plot %>%
  select(all_of(vars), treatment) %>%
  tbl_summary(
    by = treatment,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p() %>%
  as_gt() # nicely formatted table

# Save to CSV
write.csv(bal_tbl, 
          file.path("Outputs","balance_table.csv"), 
          row.names = FALSE)

# Exercise 6: Regressions ----

# Model 1: Food consumption regressed on treatment
model1 <- lm(food_cons_usd_w~treatment, data = hh_data)

# Model 2: Add controls (crop_damage, drought_flood)
model2 <- lm(food_cons_usd_w~treatment+crop_damage+
               drought_flood, data = hh_data)

# Model 3: Add FE by district
# hh_data$district=as_factor(hh_data$district)
# model3 <- lm(food_cons_usd_w~treatment+crop_damage+
#                drought_flood+as.factor(district), data = hh_data)

# Create regression table using stargazer
stargazer(
    model1, model2,
    title = "Food Consumption Effects",
    keep = c("treatment", "crop_damage", "drought_flood"),
    covariate.labels = c("Treatment",
                         "Crop Damage",
                         "Drought/Flood"),
    dep.var.labels = c("Food Consumption (USD)"),
    dep.var.caption = "",
    add.lines = list(c("District Fixed Effects", "No", "No", "Yes")),
    header = FALSE,
    keep.stat = c("n", "adj.rsq"),
    notes = "Standard errors in parentheses",
    out = file.path("Outputs","regression_table.tex")
)

# Exercise 7: Combining two plots ----

# long_data <- secondary_data %>%
#     ungroup() %>% 
#     select(-c(n_hospital, n_clinic)) %>% 
#     pivot_longer(cols = c(n_school, n_medical), names_to = "amenity", values_to = "count") %>%
#     mutate(amenity = recode(amenity, n_school = "Number of Schools", n_medical = "Number of Medical Facilities"),
#            in_sample = if_else(district %in% c("Kibaha", "Chamwino", "Bagamoyo"), "In Sample", "Not in Sample"))
# 
# # Create the facet-wrapped bar plot
# ggplot(long_data,
#        aes(......)) +
#     geom_bar(......) +
#     coord_flip() +
#     facet_wrap(......) +  # Create facets for schools and medical facilities
#     labs(title = "Access to Amenities: By Districts",
#          x = "District", y = NULL, fill = "Districts:") +
#     scale_fill_brewer(palette="PuRd") +
#     theme_minimal() +
#     ...... # Add other customization if needed
# 
# ggsave(file.path("Outputs", "fig3.png"), width = 10, height = 6)
