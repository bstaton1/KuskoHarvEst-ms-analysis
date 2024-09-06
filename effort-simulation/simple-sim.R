
# set up session
source(file.path(this.path::this.proj(), "session-setup.R"))

# function to simulate data from one opener with two flights
# and apply the effort estimator
one_sim = function(N, pi1, pi2, psi) {
  x1 = as.logical(rbinom(N, 1, pi1))
  x2 = as.logical(rbinom(N, 1, pi2))
  y = as.logical(rbinom(N, 1, psi))

  # performs the role of KuskoHarvEst:::tally_effort_data()
  # KuskoHarvEst:::tally_effort_data() accepts raw data in a different format
  effort_data = list(
    # tally the number of interviews, regardless of whether counted on a flight or not
    Y = sum(y),

    # tally the number of trips counted on each flight, regardless of whether interviewed
    X = c(X1 = sum(x1), X2 = sum(x2)),

    # tally the number of trips with various counting and interviewing joint outcomes
    XnY = c(
      "X1&Y" = sum(x1 & y),
      "X2&Y" = sum(x2 & y),
      "X1&X2&Y" = sum(x1 & x2 & y),
      "!Xany&Y" = sum(((!x1 & !x2) & y)),
      "Xall&Y" = sum(x1 & x2 & y)
    )
  )

  # perform the effort calculation
  KuskoHarvEst:::N_estimator(effort_data)
}

# build the combinations of parameters
params = expand.grid(N = 400, psi = c(0.1, 0.25, 0.5, 0.75, 0.9), pi1 = c(0.1, 0.5, 0.9), pi2 = c(0.1, 0.5, 0.9))

# selects a block with a constant pi2 equals something
params[with(params, which(pi2 == 0.1)),]

# number of parameters
nsim = 1e3

# set the random seed for reproducibility
# for some seeds, the mean error of pi1 = 0.1, pi2 = 0.1, psi = 0.1 scenario goes off plot (very large positive errors)
# point of scenario is to show that positive bias can occur in this case, which this seed does.
set.seed(3)

# each list element stores replicates of one set of parameters
all_sims = lapply(1:nrow(params), function(scenario) {
  cat("\r", scenario)
  scenario_sims = replicate(n = nsim, do.call(one_sim, as.list(params[scenario,])), simplify = FALSE)
  do.call(rbind, scenario_sims)
})

# plotting function
f = function(which_pi2, yaxt_inc = 0.25, labels) {
  
  # extract abundance estimates for scenarios with this pi2
  N_ests = do.call(cbind, lapply(all_sims[with(params, which(pi2 == which_pi2))], function(x) x[,"N_hat"]))
  
  # calculate errors
  errors = (N_ests - unique(params$N))/unique(params$N)
  
  # where on the plot do draw the boxes - makes nice groups
  at_x = c(1:5, 7:11, 13:17)
  
  # initialize a boxplot
  bp = boxplot(errors, at = at_x, outline = FALSE, plot = FALSE)
  
  # obtain the params for this boxplot
  these_params = params[with(params, which(pi2 == which_pi2)),]
  
  # calculate the summary of the errors
  bp$stats = apply(errors, 2, function(x) quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE))
  
  # set the ylimits
  ylim = 0.7 * c(-1,1)
  
  # create the boxplot
  bxp(bp, at = at_x, xaxt = "n", ylim = ylim, yaxt = "n", outline = FALSE, boxfill = "grey85", boxcol = "grey85", staplewex = 0, whisklty = 1)
  
  # draw reference lines
  abline(v = c(6, 12))
  abline(h = 0, lty = 2)
  
  # draw axes
  axis(side = 1, at = at_x, labels = these_params$psi, las = 2)
  axis(side = 2, at = seq(-2, 2, yaxt_inc), labels = paste0(round(seq(-2, 2, yaxt_inc) * 100), "%"), las = 2)
  
  # draw points showing mean errors 
  points(x = at_x, y = colMeans(errors, na.rm = TRUE), col = "white", pch = 21, bg = "grey50")
  
  # calculate plot coordinates
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])

  # where to draw the scenario labels
  at_lab = c(6, 12, usr[2])
  
  # set parameters for labels
  pi1_vals = sort(unique(params$pi1))
  pi2_vals = rep(which_pi2, 3)
  
  # add pi1 label text
  text(x = at_lab + xdiff * 0.015, y = usr[4] - ydiff * 0.05,
       labels = latex2exp::TeX(paste0("$\\pi_{1}\\,=\\,", pi1_vals, "$")), pos = 2)
  
  # add pi2 label text
  text(x = at_lab - xdiff * 0.15, y = usr[4] - ydiff * 0.06,
       labels = paste0("(", labels, ")"), pos = 2, cex = 1.2)
  
  # add letter label text
  text(x = at_lab + xdiff * 0.015, y = usr[4] - ydiff * 0.125,
       labels = latex2exp::TeX(paste0("$\\pi_{2}\\,=\\,", pi2_vals, "$")), pos = 2)
}

# open a device
# file_type = "png"
file_type = "pdf"
dev.on(base = "simple-sim", ext = file_type, height = 6, width = 3.45)

# set graphical parameters
par(mfrow = c(3,1), mgp = c(2,0.35,0), tcl = -0.15, mar = c(3,0.5,0,1),
    oma = c(0.5,4,1,0), lend = "square", ljoin = "mitre", yaxs = "i")

# all plots for pi2 = 0.1
f(0.1, yaxt_inc = 0.25, labels = c("a", "b", "c"))

# all plots for pi2 = 0.5
f(0.5, yaxt_inc = 0.25, labels = c("d", "e", "f"))

# all plots for pi2 = 0.9
f(0.9, yaxt_inc = 0.25, labels = c("h", "i", "j"))

# axis labels
mtext(side = 2, outer = TRUE, line = 2.25, "% Error in Estimated Trips (N)")
mtext(side = 1, outer = TRUE, line = -0.5, latex2exp::TeX("$\\psi$"), cex = 1.2)

# close the device
dev.off(); file.show("figures/simple-sim.pdf")
