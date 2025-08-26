setwd("/Users/default/Documents/Semester 4/Data Management/PrblemSets/")
myFilename<-file.choose()
Comrade_data = read.csv(myFilename, stringsAsFactors=FALSE)
View(Comrade_data)

summary(Comrade_data)
str(Comrade_data)

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(broom)
install.packages("stargazer")
library(stargazer)

# Explore the data structure
glimpse(Comrade_data)

# Data description:
# The dataset contains results from the Comrades Marathon, an ultramarathon race.
# It includes variables like runner ID, name, country, year, finishing time, 
# overall place (OA.Place), gender place, and other performance metrics.

# Transform time variable to seconds for calculations
Comrade_data <- Comrade_data %>%
  mutate(time_seconds = period_to_seconds(hms(Time)))

# Calculate aggregated statistics by country and year
aggregated_data <- Comrade_data %>%
  group_by(Nation, Year) %>%
  summarise(
    avg_time = mean(time_seconds, na.rm = TRUE),
    min_time = min(time_seconds, na.rm = TRUE),
    max_time = max(time_seconds, na.rm = TRUE),
    best_place = min(OA.Place, na.rm = TRUE),
    gold_medals = sum(OA.Place == 1, na.rm = TRUE),
    silver_medals = sum(OA.Place == 2, na.rm = TRUE),
    bronze_medals = sum(OA.Place == 3, na.rm = TRUE),
    participants = n()
  ) %>%
  ungroup()

# Convert average time back to period format for readability
aggregated_data <- aggregated_data %>%
  mutate(avg_time_period = seconds_to_period(avg_time))


####################### Exercise2 ############################
#a
library(dplyr)
install.packages("xtable")
library(xtable)

# Step 1: Create summary statistics
summary_stats <- aggregated_data %>%
  select(avg_time, min_time, max_time, best_place, gold_medals, participants) %>%
  summarise_all(list(
    Mean = ~mean(. , na.rm = TRUE),
    SD = ~sd(. , na.rm = TRUE),
    Min = ~min(. , na.rm = TRUE),
    Max = ~max(. , na.rm = TRUE)
  ))

# Step 2: Transpose to make it readable
summary_table <- as.data.frame(t(summary_stats))
colnames(summary_table) <- c("Value")
summary_table$Statistic <- rownames(summary_table)
summary_table <- summary_table[, c("Statistic", "Value")]

# Step 3: Create LaTeX table with xtable
latex_table <- xtable(summary_table,
                      caption = "Summary Statistics of Aggregated Variables",
                      label = "tab:summary_stats")

# Step 4: Save LaTeX table to .tex file
print(latex_table,
      file = "summary_stats.tex",
      include.rownames = FALSE,
      include.colnames = TRUE,
      caption.placement = "top",
      hline.after = c(-1, 0, nrow(summary_table)))


#b
# Focus on top 10 countries by participation
top_countries <- aggregated_data %>%
  group_by(Nation) %>%
  summarise(total_participants = sum(participants)) %>%
  arrange(desc(total_participants)) %>%
  head(10) %>%
  pull(Nation)

# Filter data for regression
regression_data <- aggregated_data %>%
  filter(Nation %in% top_countries)

# Regression 1: Average finishing time by country and year
model_time <- lm(avg_time ~ Nation + as.factor(Year), data = regression_data)

# Regression 2: Best place by country and year
model_place <- lm(best_place ~ Nation + as.factor(Year), data = regression_data)

# Regression 3: Gold medals by country and year
model_gold <- lm(gold_medals ~ Nation + as.factor(Year), data = regression_data)

# Save regression results for LaTeX
stargazer(model_time, model_place, model_gold, type = "text", out = "regression_results.tex")


#c
# Scatterplot of average time by country and year
highlight_country <- "USA" # Choose a country to highlight

time_plot <- ggplot(aggregated_data, aes(x = Year, y = avg_time/3600, group = Nation)) +
  geom_point(aes(color = Nation == highlight_country, size = Nation == highlight_country)) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray")) +
  scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0.5)) +
  labs(title = "Average Finishing Time by Country Over Years",
       x = "Year",
       y = "Average Time (hours)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white")    # Set overall plot background to white
  )

ggsave("time_plot.png", time_plot, width = 8, height = 6)

#bonus Point
# Additional figure: Participation trends by top countries
participation_plot <- aggregated_data %>%
  filter(Nation %in% top_countries) %>%
  ggplot(aes(x = Year, y = participants, color = Nation)) +
  geom_line() +
  geom_point() +
  labs(title = "Participation Trends by Top Countries",
       x = "Year",
       y = "Number of Participants") +
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave("participation_plot.png", participation_plot, width = 8, height = 6)
