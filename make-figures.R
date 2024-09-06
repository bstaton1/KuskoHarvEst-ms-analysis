
# set up session
source(file.path(this.path::this.proj(), "session-setup.R"))

# set the figure type to create if doesn't already exist
if (!exists("file_type")) file_type = "pdf"

##### MAKE FIGURE: p-active-obs #####

# cat("\nMaking Figure: p-active-obs\n")

# load the functions from the complex effort simulation
# loads p_active() function
source(file.path(proj_dir, "effort-simulation/complex-sim-fns.R"))

# calculate the proportion of interviewed trips that were active at 15min intervals by opener
# if file doesn't exist already, perform calculation and export file
# otherwise, read in file -- this process takes a minute or two, faster reproducibility
if (!file.exists(file.path(figure_dir, "p-active-obs.csv"))) {
  cat("\nCalculating p_active\n")
  p_active_all = do.call(rbind, lapply(date(M_info$start), function(d) {
    cat("\r", KuskoHarvUtils::basic_date(d))
    I_data_sub = subset(I_data, date(trip_start) == d)
    p_active(I_data_sub$trip_start, I_data_sub$trip_end)
  }))
  write.csv(p_active_all, file.path(figure_dir, "p-active-obs.csv"), row.names = FALSE)
} else {
  p_active_all = read.csv(file.path(figure_dir, "p-active-obs.csv"))
  p_active_all$time = as_datetime(p_active_all$time, tz = "US/Alaska")
}

# function to create plot for one day
f = function(d, x_axis_ticks = TRUE, y_axis_ticks = TRUE) {
  
  # subset the meta data, interview data, and flight data for this day
  mdat = subset(M_info, date(start) == d)
  idat = subset(I_data, date(trip_start) == d)
  fdat = subset(F_data, date(start_time) == d)
  
  # subset the p_active data for this day
  out = p_active_all[date(p_active_all$time) == d,]
  
  # get pi estimates
  edat = KuskoHarvEst:::tally_effort_data(idat, fdat)
  effort_ests = KuskoHarvEst:::N_estimator(edat)
  pi_ests = effort_ests[str_detect(names(effort_ests), "pi")]
  
  # empty plot with correct dimensions
  plot(p_active ~ time, data = out, type = "n", ylim = c(0,1.2),
       xlim = c(min(time[hour(time) == 0]), max(time)),
       xaxt = "n", yaxt = "n", xlab = "")
  
  # draw the p_active distribution
  with(out, polygon(x = c(time, rev(time)),
                    y = c(p_active, rep(0, length(p_active))),
                    col = "grey85", border = "black"))
  
  # draw xaxis
  at_x = to_datetime(seq(4, 20, 4), KuskoHarvUtils::basic_date(d))
  # at_x = to_datetime(seq(3, 21, 3), KuskoHarvUtils::basic_date(d))
  # at_x = to_datetime(seq(4, 20, 8), KuskoHarvUtils::basic_date(d))
  if (x_axis_ticks) {
    lab_x = c("4am", "8am", "12pm", "4pm", "8pm")
    # lab_x = c("3am", "6am", "9am", "12pm", "3pm", "6pm", "9pm")
    # lab_x = c("4am", "12pm", "8pm")
    axis(side = 1, at = at_x, labels = lab_x, las = 2)
  } else {
    axis(side = 1, at = at_x, labels = FALSE, las = 2)
  }
  
  # draw yaxis
  at_y = seq(0, 1, by = 0.25)
  if (y_axis_ticks) {
    axis(side = 2, at = at_y, labels = KuskoHarvUtils::percentize(at_y), las = 1)
  } else {
    axis(side = 2, at = at_y, labels = FALSE)
  }
  
  # draw stripes for each flight
  start_i = which(out$time %in% round_date(fdat$start_time, "15 minutes"))
  end_i = which(out$time %in% round_date(fdat$end_time, "15 minutes"))
  sapply(1:length(start_i), function(i) {
    polygon(x = c(out$time[start_i[i]:end_i[i]], rev(out$time[start_i[i]:end_i[i]])),
            y = c(rep(0, length(start_i[i]:end_i[i])), rev(out$p_active[start_i[i]:end_i[i]])),
            col = scales::alpha("grey20", 0.2), border = "black")
    points(x = mean(out$time[start_i[i]:end_i[i]]), y = pi_ests[i], cex = 1.2, pch = 21, col = "black", bg = "black")
  })
  
  # draw the start and end times of the opener
  segments(mdat$start, 0, mdat$start, 1, lty = 2)
  segments(mdat$end, 0, mdat$end, 1, lty = 2)
  
  # draw a date label
  print_date = paste0(" ", year(d), " ", month(d, label = TRUE), " ", day(d))
  mtext(side = 3, line = -1.15, adj = 0, print_date, cex = 0.7)
}

# open the graphics device
dev.on(base = "p-active-obs", ext = file_type, height = 8, width = 7.2)

# graphical parameters
par(yaxs = "i", mar = c(0.6,0.3,0,0.3), oma = c(2.25,4,0.5,0.5), cex.axis = 1, tcl = -0.25, mgp = c(2,0.3,0))
par(mfrow = c(8,5))

# is each panel in the first column or last row?
is_first_column = rep(c(T,F,F,F,F), 8)
is_last_row = c(rep(F, 35), rep(T, 5))

# loop through days, making the plot for each
ds = 1:nrow(M_info)
sapply(ds, function(i) {
  f(d = date(M_info$start)[i],
    x_axis_ticks = is_last_row[which(ds == i)],
    y_axis_ticks = is_first_column[which(ds == i)]
  )
})

# draw y-axis label
mtext(side = 2, outer = TRUE, line = 2.25, "% of Trips Active", cex = 1.2)

# close the device
dev.off()

##### MAKE FIGURE: harvest-compare #####

# cat("\nMaking Figure: harvest-compare\n")

# perform the validation analysis calculations
source(file.path(proj_dir, "validation/validation-analysis.R"))

# function to make a 1:1 scatterplot between ismp vs. psmp estimates
scatter_f = function(species, strat, legend_loc) {
  
  # extract the information for this species, stratum
  x = values %>%
    filter(stat == "est" & stratum == strat & spp == species) %>%
    mutate(value = value/1000) %>%
    dcast(year + spp ~ prgm, value.var = "value")
  lim = c(0, max(x$ISMP, x$PSMP) * 1.3)
  
  # create the scatterplot with no points but correct dimensions
  plot(ISMP ~ PSMP, data = x, xlim = lim, ylim = lim, las = 1, type = "n")
  
  # dimensional objects that depend on if a subplot or main plot
  off = ifelse(strat == "total", 0.0425, 0.0675)
  pt_cex = ifelse(strat == "total", 1.8, 1.3)
  text_cex = ifelse(strat == "total", 1.3, 0.9)
  
  # get plot dimensions
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  
  # draw the ISMP vs. PSMP estimates
  points(ISMP ~ PSMP, data = x, pch = 21, bg = scales::alpha("black", 0.5), cex = pt_cex * 1.6, lwd = 0.0, col = "white")
  
  # label the years
  text(ISMP ~ PSMP, data = x, labels = substr(year, 3, 4), cex = text_cex * 0.7, col = "white", font = 2)
  
  # label the stratum
  text(x = usr[1] - xdiff * ifelse(strat == "total", 0, 0.025),
       y = usr[4] - ydiff * ifelse(strat == "total", 0.08, 0.115),
       labels = paste0("", KuskoHarvUtils::capitalize(strat), ""), pos = 4, cex = text_cex * 1.2)
  
  # summarize percent errors and correlation in estimates
  errors = KuskoHarvUtils::get_errors(yhat = x$ISMP, yobs = x$PSMP, FUN = mean)
  errors = c(KuskoHarvUtils::percentize(errors$summary[c("MPE", "MAPE")]), round(errors$summary["RHO"], 2))
  
  # include error type labels
  error_text = paste0(names(errors), ": ", errors)
  
  # draw the error summaries: some nasty fine-tuning here to place the legend in the correct corner
  # so as to not vastly overlap the data for a given panel
  x_use = ifelse(stringr::str_detect(legend_loc, "left"), 1, 2)
  y_use = ifelse(stringr::str_detect(legend_loc, "bottom"), 3, 4)
  x_sign = ifelse(x_use == 1, -1, 0.2)
  y_sign = ifelse(y_use == 3, -0.3, -1)
  x_loc = usr[x_use] + x_sign * xdiff * ifelse(strat == "total", 0.05, 0.1)
  y_loc = usr[y_use] + y_sign * ydiff * ifelse(strat == "total", 0.08, 0.1)
  legend(x = x_loc, y = y_loc, legend = error_text, cex = text_cex * 0.8, bty = "n", 
         xjust = ifelse(x_use == 1, 0, 1), yjust = ifelse(y_use == 3, 0, 1))
  
  # draw a 1:1 line
  abline(0,1, lty = 2)
}

# open a graphics device
dev.on(base = "harvest-compare", ext = file_type, width = 7.5, height = 4.75)

# set the layout
m1 = matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE)
m2 = matrix(c(6,6,7,8,9,10), 3, 2, byrow = TRUE)
m3 = matrix(c(11,11,12,13,14,15), 3, 2, byrow = TRUE)
layout(cbind(m1, m2, m3), heights = c(1, 0.5, 0.5))

# set graphical parameters
par(mar = c(1.25,1.25,0.25,0.25), tcl = -0.15, mgp = c(2,0.25,0), cex.axis = 0.9, oma = c(2.5,2,1,0), ljoin = "mitre", lend = "square")

# all Chinook plots
scatter_f("Chinook", "total", legend_loc = "topleft"); mtext(side = 3, adj = 0, "(a) Chinook")
scatter_f("Chinook", "A", legend_loc = "bottomright")
scatter_f("Chinook", "B", legend_loc = "bottomright")
scatter_f("Chinook", "C", legend_loc = "topleft")
scatter_f("Chinook", "D1", legend_loc = "bottomright")

# all Chum plots
scatter_f("Chum", "total", legend_loc = "topleft"); mtext(side = 3, adj = 0, "(b) Chum")
scatter_f("Chum", "A", legend_loc = "bottomright")
scatter_f("Chum", "B", legend_loc = "bottomright")
scatter_f("Chum", "C", legend_loc = "topleft")
scatter_f("Chum", "D1", legend_loc = "bottomright")

# all Sockeye plots
scatter_f("Sockeye", "total", legend_loc = "topleft"); mtext(side = 3, adj = 0, "(c) Sockeye")
scatter_f("Sockeye", "A", legend_loc = "topleft")
scatter_f("Sockeye", "B", legend_loc = "bottomright")
scatter_f("Sockeye", "C", legend_loc = "topleft")
scatter_f("Sockeye", "D1", legend_loc = "topleft")

# axis labels
mtext(side = 1, outer = TRUE, line = 0.3, "Post-season Estimate (1,000s)")
mtext(side = 1, outer = TRUE, line = 1.5, "(Adjusted for Time, Gear, and Area)", cex = 0.75)
mtext(side = 2, outer = TRUE, line = 0.5, "In-season Estimate (1,000s)")

# close the device
dev.off()

##### MAKE FIGURE: p-timing #####

# cat("\nMaking Figure: p-timing\n")

# prepare the calendar information
source(file.path(proj_dir, "validation/prepare-calendar-data.R"))

# load opener information and harvest estimates: reload to get all openers (including the 3 unmonitored)
data(openers_all, package = "KuskoHarvData")
data(harv_est_all, package = "KuskoHarvData")

# set the colors
cols = c("chinook" = "black", "chum" = "royalblue", "sockeye" = "tomato")
tcols = scales::alpha(cols, c(0.35, 0.5, 0.5)); names(tcols) = names(cols)

# function create a cumulative harvest timing plot
calendar_plot = function(y, legend = FALSE, xaxis = TRUE) {
  
  # extract the area-aggregate calendar proportions for the supplied year
  dat = calendars[calendars$group == "aggrt" & year(calendars$date) == y,]
  
  # drop duplicated dates if found
  if (any(duplicated(dat$date))) dat = dat[!duplicated(dat$date),]
  
  # get in-season harvest estimates by opener for this year
  ests = subset(harv_est_all, year(date) == y & stratum == "total" & quantity == "mean" & species != "total")
  ests = dcast(ests, date ~ species, value.var = "estimate")
  ests[is.na(ests)] = 0
  
  # calculate the cumulative proportion of in-season harvest estimates
  p_ests = apply(ests[,c("chinook", "chum", "sockeye")], 2, function(x) cumsum(x)/sum(x, na.rm = TRUE))
  p_ests = data.frame(date = ests$date, p_ests, missed = rowSums(ests[,2:4]) == 0)
  
  # using calendar data through 7/15:
  with(dat[dat$date <= as_date(paste0(y, "-07-15")),], {
    # create a blank plot with the correct dimensions
    plot(p_sockeye ~ date, type = "n", ylim = c(0,1), las = 1, xaxt = ifelse(xaxis, "s", "n"))
    
    # draw the calendar data for each species
    lines(p_chinook ~ date, col = cols["chinook"], lwd = 2)
    lines(p_sockeye ~ date, col = cols["sockeye"], lwd = 2)
    lines(p_chum ~ date, col = cols["chum"], lwd = 2)
    
    # add lines at the dates of the openers
    abline(v = date(openers_all$start[year(openers_all$start) == y]), col = "grey50", lty = 2)
    
    # get user coordinates of plotting region
    usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
    
    # draw year label
    text(x = usr[1] - xdiff * 0.02, y = usr[4] - ydiff * 0.05, labels = y, font = 2, pos = 4, cex = 1.1)
    
    # draw in-season estimates cumulative proportions
    points(sockeye ~ date, data = p_ests, col = cols["sockeye"], bg = tcols["sockeye"], pch = ifelse(missed, 1, 21), cex = 1.4, xpd = TRUE)
    points(chum ~ date, data = p_ests, col = cols["chum"], bg = tcols["chum"], pch = ifelse(missed, 1, 21), cex = 1.4, xpd = TRUE)
    points(chinook ~ date, data = p_ests, col = cols["chinook"], bg = tcols["chinook"], pch = ifelse(missed, 1, 21), cex = 1.4, xpd = TRUE)
    
  })
  box()
}

# open a PNG graphics device
dev.on(base = "p-timing", ext = file_type, width = 7.5, height = 6.25)

# set graphical parameters
par(mfrow = c(3,3), mar = c(1,1.5,1,0.75), yaxs = "i", oma = c(1.5,1.75,0,0), mgp = c(2,0.25,0), tcl = -0.15, cex.axis = 0.95, lend = "square", ljoin = "mitre")

# loop through years, drawing the plot for each
junk = lapply(unique(year(calendars$date)), function(y) calendar_plot(y, legend = ifelse(y == 2016, TRUE, FALSE), xaxis = ifelse(y == 2019, TRUE, TRUE)))

# create a legend on the next empty panel
par(mar = c(2,2,2,2))
plot(1, 1, xlim = c(0,1), ylim = c(0,1), type = "n", ann = FALSE, axes = FALSE)
usr = par("usr")
legend(x = 0.5, y = 0.95, xjust = 0.5, title = "Species",
       legend = KuskoHarvUtils::capitalize(names(cols)),
       col = cols, bty = "n", cex = 1.1, title.font = 2, seg.len = 1.5,
       pch = 22, pt.lwd = 1, pt.bg = tcols, pt.cex = 2, lwd = 2)
legend(x = 0.5, y = 0.5, xjust = 0.5, title = "Data Source",
       legend = c("ISMP Monitored", "ISMP Unmontiored", "ADF&G Calendars"),
       col = "grey50", bty = "n", cex = 1.1, title.font = 2, seg.len = 1.5,
       pch = c(1,16,NA), pt.lwd = c(1,1,NA), pt.cex = c(1.5,1.5,NA), lwd = c(NA,NA,2))
box()

# axes labels
mtext(side = 1, outer = TRUE, line = 0.5, "Date")
mtext(side = 2, outer = TRUE, line = 0.5, "Cumulative Harvest Proportion")

# close the PNG device
dev.off()

# remove these versions of meta and harvest_estimate_master
rm(openers_all); rm(harv_est_all)

##### MAKE FIGURE: p-covered #####

# cat("\nMaking Figure: p-covered\n")

# perform the validation analysis calculations
source(file.path(proj_dir, "validation/validation-analysis.R"))

# get the total harvest by villages in the ISMP, but estimated by the PSMP
# and prior to any corrections
uncorrected = truth_pre_correct |> 
  group_by(spp, year) |> 
  summarize(uncorrected = sum(harvest))

# get the fraction that was left after corrections were applied
# and format for barplotting
p_covered = values |> 
  filter(stat == "est" & prgm == "PSMP" & stratum == "total") |> 
  select(-stat, -prgm, -stratum) |> 
  rename(corrected = value) |> 
  left_join(uncorrected, by = c("year", "spp")) |> 
  mutate(p_covered = corrected/uncorrected) |> 
  select(-corrected, -uncorrected) |> 
  dcast(spp ~ year, value.var = "p_covered") |> 
  select(-spp) |> 
  as.matrix()
colnames(p_covered) = NULL

# bar colors by species
greys = c("grey20", "grey50", "grey75")

# open a PNG graphics device
dev.on(base = "p-covered", ext = file_type, width = 3.45, height = 3)

# set graphical parameters
par(mar = c(1.25,2.75,1,0.25), mgp = c(2,0.2,0), tcl = -0.15, cex.axis = 0.7, lend = "square", ljoin = "mitre")

# create the barplot
mp = barplot(p_covered * 100, beside = TRUE, border = "white", las = 2, ylim = c(0,1.05) * 100, yaxt = "n", col = greys)

# add the species legend
legend("topleft", horiz = FALSE, legend = c("Chinook", "Chum", "Sockeye"), col = greys, pch = 15, pt.cex = 1.25, bty = "n", cex = 0.6)

# draw yaxis
usr = par("usr")
axis(side = 2, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"), las = 2)

# draw xaxis
par(mgp = c(2,0,0))
segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
axis(side = 1, at = colMeans(mp), labels = paste0("'", substr(unique(values$year), 3, 4)))

# y axis label
mtext(side = 2, line = 1.75, text = "% of Regional Harvest Covered by ISMP", cex = 0.9)

# close the PNG device
dev.off()

##### MAKE FIGURE: simple-sim.png #####

# REPRODUCE FIGURE BY EXECUTING SCRIPT: "effort-simulation/simple-sim.R"

##### MAKE FIGURE:  complex-sim.png #####

# REPRODUCE FIGURE BY EXECUTING SCRIPT: "effort-simulation/complex-sim.R"
