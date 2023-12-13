
# convert a decimal time of day to a datetime object
to_datetime = function(x, the_date) {
  time = paste0(floor(x), ":", stringr::str_pad(round((x - floor(x)) * 60), width = 2, side = "left", pad = "0"))
  KuskoHarvUtils::combine_datetime(rep(the_date, length(time)), time)
}

# convert a datetime object to decimal time of day
from_datetime = function(x) {
  lubridate::hour(x) + lubridate::minute(x)/60
}

# set simulation arguments
set_sim_args = function(
    # number of trips
    N = 500,
    
    # hours of the day a trip CAN start
    trip_start_range = c(5, 21),
    
    # number of hours a trip CAN last
    trip_duration_range = c(2, 10),
    
    # shape parameters of beta dist controlling start time of trip within range
    beta_trip_start = c(1,1),
    
    # shape parameters of beta dist controlling duration of trip within range
    beta_trip_duration = c(4,10),
    
    # time of day interviews can start/end
    interview_start = 0, interview_end = 24,
    
    # shape parameters of beta dist controlling whether trips are interviewed
    interview_beta = c(1,1),
    
    # start times of each flight
    flight_start = c(9, 14, 19),
    
    # end times of each flight
    flight_end = c(11, 16, 21),
    
    # expected proportion of trips returning during interviewed times that are interviewed
    p_interview = 0.25
) {
  
  list(
    N = N,
    trip_start_range = trip_start_range,
    trip_duration_range = trip_duration_range,
    beta_trip_start = beta_trip_start,
    beta_trip_duration = beta_trip_duration,
    interview_start = interview_start,
    interview_end = interview_end,
    interview_beta = interview_beta, 
    flight_start = flight_start,
    flight_end = flight_end,
    p_interview = p_interview
  )
}

# function to simulate trips
trips_sim_rng = function(
    N = set_sim_args()$N,
    trip_start_range = set_sim_args()$trip_start_range,
    trip_duration_range = set_sim_args()$trip_duration_range,
    beta_trip_start = set_sim_args()$beta_trip_start,
    beta_trip_duration = set_sim_args()$beta_trip_duration
){
  
  # create the date for datetime variables
  # the date choice is meaningless for simulation
  # KuskoHarvEst requires datetime class, not just times
  # and all data must be from the same date
  mm = "06"; dd = "12"; yyyy = "2023"
  the_date = paste(mm, dd, yyyy, sep = "/")
  
  # create the times for each trip: sample start time and duration from uniform distributions
  trip_start = round(runif(N, trip_start_range[1], trip_start_range[2]), 2)
  trip_duration = round(runif(N, trip_duration_range[1], trip_duration_range[2]), 2)
  
  # resample from these these distributions by making them non-uniform
  trip_start = sample(
    sort(trip_start), size = N, replace = TRUE, 
    prob = dbeta(seq(0.01, 0.99, length = N), beta_trip_start[1], beta_trip_start[2]))
  trip_duration = sample(
    sort(trip_duration), size = N, replace = TRUE,
    prob = dbeta(seq(0.01, 0.99, length = N), beta_trip_duration[1], beta_trip_duration[2]))
  trip_end = trip_start + trip_duration
  
  # ensure no trips have an end time later than 24
  trip_end[trip_end > 23.75] = 23.75
  trip_start[trip_end > 23.75] = 23.75
  
  # contains start/stop times for every trip
  # last two columns are needed for KuskoHarvEst but don't come into play here
  data.frame(
    trip_start = to_datetime(trip_start, the_date),
    trip_end = to_datetime(trip_end, the_date),
    gear = "drift",
    suit_effort = TRUE
  )
}

# function simulate flight counts
flight_sim = function(
    trips, 
    flight_start = set_sim_args()$flight_start,
    flight_end = set_sim_args()$flight_end
) {
  # make up the spatial distribution of the effort counts
  # also doesn't matter: effort estimator ignores spatial structure
  # but KuskoHarvEst is designed based on having the data collected with this structure
  spatial_dist = c("A_drift" = 0.3, "B_drift" = 0.2, "C_drift" = 0.4, "D1_drift" = 0.1)
  
  # extract the date; doesn't matter
  the_date = unique(KuskoHarvUtils::basic_date(lubridate::date(trips$trip_start)))
  
  # create the flight data frame: placeholders for counts for now
  flight_data = data.frame(
    flight = 1:length(flight_start),
    start_time = to_datetime(flight_start, the_date),
    end_time = to_datetime(flight_end, the_date),
    A_drift = NA,
    B_drift = NA,
    C_drift = NA,
    D1_drift = NA
  )
  
  # determine if each trip (that truly occurred) was active during the time of each flight
  x = KuskoHarvEst:::was_counted(trips[,c("trip_start","trip_end")], flight_data[,c("start_time", "end_time")])
  
  # count these up for only the non-joint counts
  # this is the count on each flight
  if (ncol(x) > 1) {
    flight_counts = colSums(x[,!stringr::str_detect(colnames(x), "&")])
  } else {
    flight_counts = sum(x)
  }
  flight_data[,names(spatial_dist)] = t(sapply(flight_counts, function(x) KuskoHarvUtils:::smart_round(x * spatial_dist)))
  
  flight_data
}

# function to simulate whether each trip was interviewed
interview_sim = function(
    trips,
    interview_start = set_sim_args()$interview_start,
    interview_end = set_sim_args()$interview_end,
    interview_beta = set_sim_args()$interview_beta,
    p_interview = set_sim_args()$p_interview
) {
  
  # which trips are possible to be interviewed
  trips_possible = trips[from_datetime(trips$trip_end) >= interview_start & from_datetime(trips$trip_end) <= interview_end,]
  end = from_datetime(trips_possible$trip_end)
  
  # sample trips according to beta distribution 
  interview_length = interview_end - interview_start
  time_q = interview_start + seq(0, 1, 0.01) * interview_length
  time_p = seq(0, 1, 0.01)
  at_x = sapply(end, function(e) time_p[which.min(e > time_q)])
  i = sample(1:nrow(trips_possible), size = rbinom(1, nrow(trips_possible), p_interview), replace = FALSE, prob = dbeta(at_x, interview_beta[1], interview_beta[2]))
  
  # return only trips that were interviewed
  interview_data = trips_possible[i,]
  interview_data[order(interview_data$trip_start),]
}

# performs one iteration of simulation/estimation
sim_est = function(i, do_p_active = FALSE, ...) {
  
  if (i == 1) cat("\n")
  cat("\rIteration:", i)
  
  # set the arguments for all simulation functions
  args = set_sim_args(...)
  
  # obtain the names of all arguments to each of these functions
  trips_sim_args = formalArgs(trips_sim_rng)
  interview_sim_args = formalArgs(interview_sim)
  flight_sim_args = formalArgs(flight_sim)
  
  # simulate the trip outcomes: truth
  args = append(args, list(trips = do.call(trips_sim_rng, args[trips_sim_args])))
  
  # sample truth to obtain the interview data set
  args = append(args, list(interview_data = do.call(interview_sim, args[interview_sim_args])))
  
  # sample truth to obtain the flight data set
  args = append(args, list(flight_data = do.call(flight_sim, args[flight_sim_args])))
  
  # produce and return the effort estimate and associated information
  effort_data = with(args, KuskoHarvEst:::tally_effort_data(interview_data, flight_data))
  est_out = c(iter = i, KuskoHarvEst:::N_estimator(effort_data))
  est_out_empty = c(iter = NA, pi1_hat = NA, pi2_hat = NA, pi3_hat = NA, psi_hat = NA, N_hat = NA)
  est_out_empty[names(est_out)] = est_out
  est_out = est_out_empty
  
  # calculate p_active if instructed, otherwise return NULL
  if (do_p_active) {
    # calculate p_active for all trips: true participation curve
    p_active_trip = p_active(args$trips$trip_start, args$trips$trip_end)
    colnames(p_active_trip)[2] = "all_trips"
    
    # calculate p_active for interviewed trips only
    p_active_interview = p_active(args$interview_data$trip_start, args$interview_data$trip_end)
    colnames(p_active_interview)[2] = "interviews"
    
    # combine the two p_active curves and the iteration number
    p_active_out = merge(p_active_trip, p_active_interview, by = "time")
    p_active_out = cbind(iter = i, p_active_out)
  } else {
    p_active_out = NULL
  }
  
  # build the output from this simulation-estimation iteration
  out = list(
    # iteration ID
    i = i,
    
    # p_active curves
    p_active = p_active_out,
    
    # the arguments supplied for simulation
    args = args,
    
    # the output of estimation
    est_out = est_out
  )
  
  # return this list
  return(out)
}

# performs nsim iterations of simulation/estimation under identical settings
replicate_sim_est = function(nsim = 10, do_p_active = FALSE, seed = 1234, ...) {
  args = set_sim_args(...)
  set.seed(seed)
  out = lapply(1:nsim, function(i) do.call(sim_est, append(args, list(i = i, do_p_active = do_p_active))))
  
  list(
    ests = do.call(rbind, lapply(out, function(i) i$est_out)),
    p_active = do.call(rbind, lapply(out, function(i) i$p_active))
  )
}

# performs all combinations of flight scenarios, where there are three possible flights for the day
sim_scenarios = function(nsim, 
                         flight_starts = c(A = 8, B = 12, C = 16),
                         flight_ends = flight_starts + 1.5, ...) {
  
  # set the arguments that will apply to all simulations
  # unless (a) expressly changed in this list or (b) removed and new value appended
  common_args = set_sim_args(...)
  common_args = append(common_args, list(nsim = nsim))
  
  # one flight scenarios: A, B, C represent had we selected different flight to use for the day
  cat("\nRunning One-Flight Scenarios")
  remove = which(names(common_args) %in% c("flight_start", "flight_end"))
  out_1 = list(
    A = do.call(replicate_sim_est, append(common_args[-remove], list(flight_start = flight_starts["A"], flight_end = flight_ends["A"]))),
    B = do.call(replicate_sim_est, append(common_args[-remove], list(flight_start = flight_starts["B"], flight_end = flight_ends["B"]))),
    C = do.call(replicate_sim_est, append(common_args[-remove], list(flight_start = flight_starts["C"], flight_end = flight_ends["C"])))
  )
  
  # two flight scenarios: first two possible flights
  cat("\nRunning Two-Flight Scenarios (AB)")
  common_args$flight_start = flight_starts[c("A", "B")]
  common_args$flight_end = flight_ends[c("A", "B")]
  out_2ab = do.call(replicate_sim_est, common_args)
  
  # two flight scenarios: first and last two possible flights
  cat("\nRunning Two-Flight Scenarios (AC)")
  common_args$flight_start = flight_starts[c("A", "C")]
  common_args$flight_end = flight_ends[c("A", "C")]
  out_2ac = do.call(replicate_sim_est, common_args)
  
  # two flight scenarios: second and third possible flights
  cat("\nRunning Two-Flight Scenarios (BC)")
  common_args$flight_start = flight_starts[c("B", "C")]
  common_args$flight_end = flight_ends[c("B", "C")]
  out_2bc = do.call(replicate_sim_est, common_args)
  
  # three flight scenarios
  cat("\nRunning Three-Flight Scenarios")
  common_args$flight_start = flight_starts[c("A", "B", "C")]
  common_args$flight_end = flight_ends[c("A", "B", "C")]
  out_3 = do.call(replicate_sim_est, append(common_args, list(do_p_active = TRUE)))
  
  # summarize the "p_active" output
  p_active_out = out_3$p_active
  out_3 = out_3[-which(names(out_3) == "p_active")]
  p_active_out = merge(
    aggregate(all_trips ~ time, data = p_active_out, FUN = mean),
    aggregate(interviews ~ time, data = p_active_out, FUN = mean),
    by = "time"
  )
  
  cat("\n")
  
  # build a list with all output
  out = c(out_1, list(AB = out_2ab, AC = out_2ac, BC = out_2bc, ABC = out_3))
  list(
    N_hat = do.call(cbind, lapply(out, function(x) x$ests[,"N_hat"])),
    pi1_hat = do.call(cbind, lapply(out, function(x) x$ests[,"pi1_hat"])),
    pi2_hat = do.call(cbind, lapply(out, function(x) x$ests[,"pi2_hat"])),
    pi3_hat = do.call(cbind, lapply(out, function(x) x$ests[,"pi3_hat"])),
    psi_hat = do.call(cbind, lapply(out, function(x) x$ests[,"psi_hat"])),
    p_active_out = p_active_out,
    args = common_args
  )
}

# given a set of start and end times for a set of trips,
# calculate fraction active at every 15 minute interval of the day
p_active = function(trip_start, trip_end)  {
  
  # make sure all records have data present
  times = data.frame(trip_start = trip_start, trip_end = trip_end)
  times = times[!is.na(times$trip_start) & !is.na(times$trip_end),]
  
  # extract the date
  the_date = unique(KuskoHarvUtils::basic_date(lubridate::date(times$trip_start)))
  
  # build the times to test the proportion of all fishers that were active at
  time_test = seq(to_datetime(0, the_date), to_datetime(23, the_date), by = "hour")
  time_test = sort(unique(c(time_test, time_test + 15 * 60, time_test + 30 * 60, time_test + 45 * 60)))
  
  # function to determine if each fisher was fishing at a given time
  fishing_at_time = function(trip_start, trip_end) {
    sapply(1:length(time_test), function(i) dplyr::between(time_test[i], trip_start, trip_end))
  }
  
  # apply this to all trips and times and convert to a proportion
  active = sapply(1:nrow(times), function(i) fishing_at_time(times$trip_start[i], times$trip_end[i]))
  p_active = rowMeans(active)
  
  # create and return the output
  data.frame(time = time_test, p_active = p_active)
}

# function to plot the p_active distribution
p_active_plot = function(p_active_out, flight_start = NULL, flight_end = NULL, ylim = c(0,1), draw_interviews = TRUE) {
  
  # get the date used: arbitrary
  the_date = unique(KuskoHarvUtils::basic_date(lubridate::date(p_active_out$time)))
  
  # empty plot with correct dimensions
  plot(p_active_out$all_trips ~ p_active_out$time, type = "n", xaxt = "n", yaxt = "n",
       ylim = ylim, xlim = c(to_datetime(4, the_date), to_datetime(20, the_date)),
       xlab = "", ylab = "")
  
  # draw the p_active distribution
  polygon(x = c(p_active_out$time, rev(p_active_out$time)),
          y = c(rep(0, length(p_active_out$time)), rev(p_active_out$all_trips)),
          col = "grey85", border = "black")
  
  # draw the stripes for the flight times
  if (!is.null(flight_start) & !is.null(flight_end)) {
    if (is.numeric(flight_start)) flight_start = to_datetime(flight_start, the_date)
    if (is.numeric(flight_end)) flight_end = to_datetime(flight_end, the_date)
    
    start_i = which(p_active_out$time %in% lubridate::round_date(flight_start, "15 minutes"))
    end_i = which(p_active_out$time %in% lubridate::round_date(flight_end, "15 minutes"))
    
    sapply(1:length(start_i), function(i) {
      polygon(x = c(p_active_out$time[start_i[i]:end_i[i]], rev(p_active_out$time[start_i[i]:end_i[i]])),
              y = c(rep(0, length(start_i[i]:end_i[i])), rev(p_active_out$all_trips[start_i[i]:end_i[i]])),
              col = scales::alpha("grey20", 0.2), border = "black")
    })
    
    # label the flights
    usr = par("usr"); ydiff = diff(usr[3:4])
    text(x = colMeans(rbind(flight_start, flight_end)), cex = 0.9,
         y = usr[3] + ydiff * 0.1, labels = LETTERS[1:length(flight_start)])
    
  }
  
  # draw the interviewed p_active curve if desired
  if (draw_interviews) {
    lines(p_active_out$interviews ~ p_active_out$time, col = "black", lty = 2)
  }
  
  # draw y-axis
  at_y = axisTicks(par("usr")[3:4], log = FALSE)
  axis(side = 2, at = at_y, labels = paste0(at_y * 100, "%"), las = 1)
  
  # draw x-axis
  at_x = to_datetime(seq(4, 20, 4), the_date)
  lab_x = c("4am", "8am", "12pm", "4pm", "8pm")
  axis(side = 1, at = at_x, labels = lab_x, las = 2)
}

# function to plot the p_active distribution and error boxplots for one block of scenarios
one_row = function(out, ylim_box = c(-0.5,2), ylim_pN = c(0,0.5), panel_label, draw_interviews = TRUE) {
  
  # draw the p_active plot
  p_active_plot(out$p_active_out, flight_start = out$args$flight_start, flight_end = out$args$flight_end, ylim = ylim_pN, draw_interviews = draw_interviews)  
  mtext(side = 3, adj = 0, panel_label, line = 0.1, cex = 0.65)
  
  # calculate effort estimate errors made
  errors = (out$N_hat - out$args$N)/out$args$N
  
  # draw the errors boxplots
  at_x = c(1,2,3, 4.5,5.5,6.5, 8)
  bp = boxplot(errors, at = at_x, plot = FALSE)
  bp$stats = apply(errors, 2, function(x) quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE))
  bxp(bp, at = at_x, las = 3, outline = FALSE, yaxt = "n",
      ylim = ylim_box, boxfill = "grey85", boxcol = "grey85", staplewex = 0, whisklty = 1)
  
  # draw reference lines
  abline(h = 0, lwd = 1, lty = 2, col = "black")
  abline(v = 3.75, col = "grey")
  abline(v = 7.25, col = "grey")
  
  # draw points for mean errors
  points(x = at_x, y = colMeans(errors, na.rm = TRUE), col = "white", pch = 21, bg = "grey50")
  
  # draw y-axis for errors
  at_y = axisTicks(par("usr")[3:4], log = FALSE)
  axis(side = 4, at = at_y, labels = paste0(at_y * 100, "%"), las = 1)
  box()
}
