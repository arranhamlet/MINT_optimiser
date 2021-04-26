options(scipen = 999)

library(ROI.plugin.glpk)
library(dplyr)
library(tidyverse)
library(ompr)
library(data.table)
library(ggpubr)
library(gridExtra)
library(grid)
# install packages from here https://r-opt.r-universe.dev
library(ROIoptimzer)
library(rmpk)

invisible(sapply(list.files("R/functions", full.names = T, recursive = T), function(x) source(x)))

data <- read.csv("data/mock_data.csv", stringsAsFactors = FALSE)
data2 <- data
data2$zone <- data2$zone + 6
data <- rbind(data, data2)

# create the input parameters for the model
# hacked together :)
n_zones <- n_distinct(data$zone)
n_interventions <- n_distinct(data$intervention)
budget <- sum(data$total_costs) / 4 #some random budget


time1 <- Sys.time()
intervention_optimised <- intervention_optimiser(data = data,
                                                 n_zones = n_zones,
                                                 n_interventions = n_interventions,
                                                 budget = 1000000,
                                                 budget_variation = seq(1, 0.8, by = -0.05))
time2 <- Sys.time()

time2 - time1

intervention_optimised_agg <- aggregate(x = list(total_costs = intervention_optimised$total_costs,
                                                 total_cases_averted = intervention_optimised$total_cases_averted),
                                        by = list(budget_variation = intervention_optimised$budget_variation),
                                        FUN = sum)

heatmap_intervention <- ggplot(data = intervention_optimised, aes(x = factor(zone), y = factor(budget_variation), fill = intervention)) +
  geom_tile(color = "black") +
  theme_minimal() +
  labs(y = "Proportion of original budget spent", x = "Zone", fill = "Intervention") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

barplot_averted <- ggplot(data = intervention_optimised_agg) +
  geom_bar(aes(y = factor(budget_variation), x = total_cases_averted, fill = total_costs), stat = "identity") +
  theme_minimal() +
  labs(x = "Total cases averted", y = "Proportion of original budget spent", fill = "Total costs") +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = max(intervention_optimised_agg$total_cases_averted), linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.key.width= unit(.95, 'cm')) +
  scale_fill_continuous(labels = scales::comma)


combo_plot <- ggarrange(heatmap_intervention, barplot_averted, widths = c(3, 2))

ggsave("figs/intervention_combinations_plot.png", combo_plot, height = 5, width = 12)







