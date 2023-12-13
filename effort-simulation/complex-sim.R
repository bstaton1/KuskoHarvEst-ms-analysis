
# set up session
source(file.path(this.path::this.proj(), "session-setup.R"))

# load the simulation functions
source(file.path(proj_dir, "effort-simulation/complex-sim-fns.R"))

# set the number of iterations per scenario
nsim = 100

##### SCENARIOS WITH UNBIASED INTERVIEW SAMPLE TIMING #####
# start a timer
starttime = Sys.time()

# set arguments common to all scenarios
common_args = list(
  nsim = nsim,
  N = 400,
  trip_start_range = c(5,14.5),
  trip_duration_range = c(1,6),
  p_interview = 0.4,
  flight_starts = c(A = 7, B = 10.5, C = 14),
  flight_ends = c(A = 8.5, B = 12, C = 15.5),
  interview_start = 5, interview_end = 19
)

# run all iterations of all flight scenarios where participation curve is uniform throughout day
out_unif = do.call(sim_scenarios, append(common_args, list(beta_trip_start = c(1,1))))

# run all iterations of all flight scenarios where participation curve is skewed earlier in the day
out_early = do.call(sim_scenarios, append(common_args, list(beta_trip_start = c(1,3))))

# run all iterations of all flight scenarios where participation curve is skewed later in the day
out_late = do.call(sim_scenarios, append(common_args, list(beta_trip_start = c(3,1))))

# end the timer
stoptime = Sys.time()
stoptime - starttime

# set ylimits
ylim_box = c(-0.6,0.6)  # for boxplots
ylim_pN = c(0, 0.6)     # for p_active

# open the plotting device
png("figures/complex-sim-1.png", h = 6 * ppi, w = 3.45 * ppi, res = ppi)

# set the layout and graphical parameters
layout(matrix(1:6, ncol = 2, byrow = TRUE), widths = c(1,1))
par(yaxs = "i", mgp = c(2,0.2,0), tcl = -0.15, mar = c(2.5,0,1,0), oma = c(0.5,3.25,1,4.75),
    cex.axis = 0.9, lend = "square", ljoin = "mitre")

# make the plots summarizing the scenarios
one_row(out_unif, panel_label = '(a) Effort Evenly Distributed over Opener', ylim_pN = ylim_pN, ylim_box = ylim_box, draw_interviews = FALSE)
one_row(out_early, panel_label = '(b) Effort Skewed Early in the Opener', ylim_pN = ylim_pN, ylim_box = ylim_box, draw_interviews = FALSE)
one_row(out_late, panel_label = '(c) Effort Skewed Late in the Opener', ylim_pN = ylim_pN, ylim_box = ylim_box, draw_interviews = FALSE)

# axis labels
mtext(side = 2, outer = TRUE, line = 2, "% of Trips Active", cex = 0.9)
mtext(side = 4, outer = TRUE, line = 2.5, "% Error in Estimated Trips", cex = 0.9)

# close the device
dev.off(); #file.show("figures/complex-sim-1.png")

##### SCENARIOS WITH UNBIASED INTERVIEW SAMPLE TIMING #####
# start a timer
starttime = Sys.time()

# set arguments common to all scenarios
common_args = list(
  nsim = nsim,
  N = 400,
  trip_start_range = c(5,14.5),
  trip_duration_range = c(1,6),
  p_interview = 0.4,
  flight_starts = c(A = 7, B = 10.5, C = 14),
  flight_ends = c(A = 8.5, B = 12, C = 15.5),
  interview_start = 5, interview_end = 19
)

# run all iterations of all flight scenarios where interviewed timing curve is biased toward center
out_unif = do.call(sim_scenarios, append(common_args, list(interview_beta = c(3,3))))

# run all iterations of all flight scenarios where interviewed timing curve is biased toward earlier trips
out_early = do.call(sim_scenarios, append(common_args, list(interview_beta = c(1,2.5))))

# run all iterations of all flight scenarios where interviewed timing curve is biased toward later trips
out_late = do.call(sim_scenarios, append(common_args, list(interview_beta = c(2.5,1))))

# end the timer
stoptime = Sys.time()
stoptime - starttime

# set ylimits
ylim_box = c(-0.5,2.25)  # for boxplots
ylim_pN = c(0, 0.6)      # for p_active

# open the plotting device
png("figures/complex-sim-2.png", h = 6 * ppi, w = 3.45 * ppi, res = ppi)

# set the layout and graphical parameters
layout(matrix(1:6, ncol = 2, byrow = TRUE), widths = c(1,1))
par(yaxs = "i", mgp = c(2,0.2,0), tcl = -0.15, mar = c(2.5,0,1,0), oma = c(0.5,4.25,1,3.75),
    cex.axis = 0.9, lend = "square", ljoin = "mitre")

# make the plots summarizing the scenarios
one_row(out_unif, panel_label = '(d) Interview Sampling Bias for Mid-Opener Trips', ylim_pN = ylim_pN, ylim_box = ylim_box)
one_row(out_early, panel_label = '(e) Interview Sampling Bias for Early-Opener Trips', ylim_pN = ylim_pN, ylim_box = ylim_box)
one_row(out_late, panel_label = '(f) Interview Sampling Bias for Late-Opener Trips', ylim_pN = ylim_pN, ylim_box = ylim_box)

# axis labels
mtext(side = 2, outer = TRUE, line = 2, "% of Trips Active", cex = 0.9)
mtext(side = 4, outer = TRUE, line = 2.5, "% Error in Estimated Trips", cex = 0.9)

# close the device
dev.off(); #file.show("figures/complex-sim-2.png")

##### COMBINE THE TWO FIGURES INTO ONE FILE #####

# informed by this thread:
# https://stackoverflow.com/questions/31732359/combine-png-files-in-current-folder-to-a-single-png-file-in-r

# open a new PNG device
png("figures/complex-sim.png", width = 7 * ppi, height = 6 * ppi, res = ppi)

# load the two plot image rasters
plots = lapply(c("figures/complex-sim-1.png", "figures/complex-sim-2.png"), function(x){
  img = as.raster(png::readPNG(x))
  grid::rasterGrob(img, interpolate = TRUE)
})

# dump them in the new file
junk = print(gridExtra::marrangeGrob(grobs = plots, nrow = 1, ncol = 2, top = NULL))

# close the graphics device
dev.off()

# delete the individual figure files
unlink("figures/complex-sim-1.png")
unlink("figures/complex-sim-2.png")
