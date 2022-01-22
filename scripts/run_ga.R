
results_frame <- map_dfr(1:nrow(parameter_frame), find_optimal_ga_parameters)

beep()