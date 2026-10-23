sim <- simulate_nowcast_data(n_years = 16, seed = 11)
sim_v <- vintage(sim, "2015-08-20")
sim_target <- "2015Q3"
