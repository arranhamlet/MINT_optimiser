intervention_optimiser <- function(data, #This is the dataframe provided into MINT
                                   n_zones, #The number of zones to optimise over
                                   n_interventions, #The number of interventions to optimise over
                                   budget, #The total budget
                                   budget_variation = 1 #This is a vector of proporional changes to the budget in order to add some variation and see other combinations, i.e. c(1, 0.9, 0.8)
){
  
  rbindlist(sapply(budget_variation, function(a){
    
    cost_df <- distinct(data, zone, intervention, total_costs) %>%
      arrange(intervention, zone) %>%
      group_by(zone) %>% mutate(j = row_number()) %>%
      group_by(intervention) %>% mutate(i = row_number()) %>%
      ungroup()
    
    cases_av_df <- distinct(data, zone, intervention, total_cases_averted) %>%
      arrange(intervention, zone) %>%
      group_by(zone) %>% mutate(j = row_number()) %>%
      group_by(intervention) %>% mutate(i = row_number()) %>%
      ungroup()
    
    cost <- function(zone, intervention) {
      filter(cost_df, i == !!zone, j == !!intervention)$total_costs
    }
    cases_av <- function(zone, intervention) {
      filter(cases_av_df, i == !!zone, j == !!intervention)$total_cases_averted
    }
    
    # There are different ways to formulate that
    # The idea is to have a binary decision variable that models
    # if zone i has intervention j. I.e. it is 1 if zone i has intervention j otherwise 0
    
    # create a model with a GLPK as the optimizer
    model <- optimization_model(
      ROI_optimizer("glpk", control = list(verbose = TRUE)))
    
    # our decision variable, integer with lower bound 0 and upper bound 1
    y <- model$add_variable("y", i = 1:n_zones, j = 1:n_interventions, type = "integer", lb = 0, ub = 1)
    
    # the objective is to maximize cases averted
    model$set_objective(sum_expr(y[i, j] * cases_av(i, j), i = 1:n_zones, j = 1:n_interventions), sense = "max")
    
    # subject to a budget constraint
    model$add_constraint(sum_expr(y[i, j] * cost(i, j), i = 1:n_zones, j = 1:n_interventions) <= budget * a)
    
    # Also make sure that each zone gets exactly one intervention
    model$add_constraint(sum_expr(y[i, j], j = 1:n_interventions) == 1, i = 1:n_zones)
    model$optimize()
    
    res <- model$get_variable_value(y[i, j]) %>%
      filter(value == 1) %>%
      left_join(cost_df) %>%
      left_join(select(cases_av_df, total_cases_averted, i, j), by = c("i", "j"))
  
    res$budget_variation <- a
    
    res
    
  }, simplify = FALSE))
  
}