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
# data2 <- data
# data2$zone <- data2$zone + 6
# data <- rbind(data, data2)

# create the input parameters for the model
# hacked together :)
time1 <- Sys.time()
intervention_optimised <- intervention_optimiser(data = data,
                                                 budget = sum(data$total_costs)/5,
                                                 budget_variation = seq(1, 0.8, by = -0.05))
time2 <- Sys.time()

intervention_optimised_agg <- aggregate(x = list(total_costs = intervention_optimised$total_costs,
                                                 total_cases_averted = intervention_optimised$total_cases_averted),
                                        by = list(budget_variation = intervention_optimised$budget_variation,
                                                  intervention_removed = intervention_optimised$intervention_removed),
                                        FUN = sum)
time2 - time1

#Now with all combinations
heatmap_intervention <- ggplot(data = subset(intervention_optimised, !is.na(total_cases_averted)), 
                               aes(x = factor(zone), y = factor(budget_variation), fill = intervention)) +
  geom_tile(color = "black") +
  theme_minimal() +
  labs(y = "Proportion of original budget spent", x = "Zone", fill = "Intervention") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2)) +
  facet_wrap(~intervention_removed, ncol = 1)

barplot_averted <- ggplot(data = subset(intervention_optimised_agg, !is.na(total_cases_averted))) +
  geom_bar(aes(y = factor(budget_variation), x = total_cases_averted, fill = total_costs), stat = "identity") +
  theme_minimal() +
  labs(x = "Total cases averted", y = "", fill = "Total costs ($)") +
  scale_x_continuous(labels = scales::comma) +
  geom_vline(xintercept = max(intervention_optimised_agg$total_cases_averted), linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.45, 'cm')) +
  scale_fill_continuous(labels = scales::comma) +
  facet_wrap(~intervention_removed, ncol = 1)

combo_plot <- ggarrange(heatmap_intervention, barplot_averted, widths = c(3, 2.1))

combo_plot

ggsave("figs/intervention_combinations_plot_all_considerations.png", combo_plot, height = 10, width = 12)




