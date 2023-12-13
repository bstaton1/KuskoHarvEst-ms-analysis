
# set up session
source(file.path(this.path::this.proj(), "session-setup.R"))

##### MAKE FILE: interviews-by-village-and-stratum.csv #####

# cat("\nMaking Table: interviews-by-village-and-stratum.csv\n")

# load the calendar data, this is where v_df is defined (villages included and their order)
source(file.path(proj_dir, "validation/prepare-calendar-data.R"))

# load the raw interview data: includes all interviews, even for the three openers with no flight data
data(interview_data_master, package = "KuskoHarvData")
dat = interview_data_master

# handle villages
dat$village[dat$village == "#VALUE!"] = NA
dat = dat[dat$village != "Tuluksak",]
dat = dat[dat$village != "Eek",]
dat = dat[!is.na(dat$stratum) & !is.na(dat$village),]

# count interviews by village
dat$village = factor(dat$village)
counts = table(dat$village, dat$stratum)

# reformat the counts
counts = as.data.frame(counts)
colnames(counts) = c("village", "stratum", "count")
stratum_counts = reshape2::dcast(counts, village ~ stratum, value.var = "count")

# extract the unique years and number of unique years each village was sampled
year_info = lapply(unique(dat$village), function(v) {
  # extract all interviews for this village
  dat_v = subset(dat, village == v & !is.na(trip_start))
  
  # extract the unique years sampled for this village
  unique_years = unique(year(dat_v$trip_start))
  
  # build the output: village, number of years, and the specific years
  data.frame(village = v, yr_counts = length(unique_years), yrs_sampled = paste(unique_years, collapse = "; "))
})
year_info = do.call(rbind, year_info)

# combine year info with stratum counts
stratum_counts = merge(year_info, stratum_counts, by = "village")

# order the data frame by village
stratum_counts$village = factor(stratum_counts$village, levels = levels(v_df$village))
stratum_counts = stratum_counts[order(stratum_counts$village),]

# save as a csv file
write.csv(stratum_counts, file.path(table_dir, "interviews-by-village-and-stratum.csv"), row.names = FALSE)

# remove these versions of the interview data
rm(interview_data_master); rm(dat)

##### MAKE FILE: obs-effort-ests.csv #####

# cat("\nMaking Table: obs-effort-ests.csv\n")

# function to process info for one date
f = function(keep_date) {
  # subset interview and flight data
  i = subset(I_data, date(trip_start) == keep_date)
  f = subset(F_data, date(start_time) == keep_date)
  
  # tally up & format effort data: e.g., how many interviews reported trips active during flights
  tallies_raw = KuskoHarvEst:::tally_effort_data(interview_data = i, flight_data = f)
  tallies = unlist(unname(tallies_raw))
  names(tallies)[1] = "Y"
  tallies_df = as.data.frame(as.list(tallies))
  colnames(tallies_df) = names(tallies)
  
  # apply the effort estimator
  out = as.list(KuskoHarvEst:::N_estimator(effort_data = tallies_raw))
  out_df = as.data.frame(out)
  names(out_df) = names(out)
  
  # create a placeholder object that has equal dimensions regardless of how many flights were conducted
  out_empty = data.frame("Y" = NA, "X1" = NA, "X2" = NA, "X3" = NA, "X1&Y" = NA, "X2&Y" = NA, "X3&Y" = NA, "X1&X2&Y" = NA, "X2&X3&Y" = NA, "!Xany&Y" = NA, "Xall&Y" = NA, pi1_hat = NA, pi2_hat = NA, pi3_hat = NA, psi_hat = NA, N_hat = NA)
  colnames(out_empty) = str_replace_all(colnames(out_empty), "\\.", "&")
  colnames(out_empty)[colnames(out_empty) == "X&Xany&Y"] = "!Xany&Y"
  out_empty[,colnames(out_df)] = out_df
  
  # fill in the placeholder object
  out_empty[,colnames(tallies_df)] = tallies_df
  
  # add the date variable
  cbind(date = keep_date, out_empty)
}

# loop through every date and apply the function to summarize effort data and estimates
out = lapply(report_dates, function(d) f(d))
out = do.call(rbind, out)

# handle dates
mn = as.character(month(out$date, label = TRUE))
dy = day(out$date)
yr = year(out$date)
out$date_nice = paste(dy, mn)
out$year = yr

# reorder/keep only useful columns
obs_effort_ests = out[,c("year", "date", "date_nice", "X1", "X2", "X3", "Y", "X1&Y", "X2&Y", "X3&Y", "X1&X2&Y", "X2&X3&Y", "!Xany&Y", "N_hat", "pi1_hat", "pi2_hat", "pi3_hat", "psi_hat")]

# round estimates
obs_effort_ests[,"N_hat"] = round(obs_effort_ests[,"N_hat"])

# save the file
write.csv(obs_effort_ests, file.path(table_dir, "obs-effort-ests.csv"), row.names = FALSE)

##### MAKE FILE: obs-harv-ests.csv #####

# cat("\nMaking Table: obs-harv-ests.csv\n")

# 1.) obtain effort estimates
trips_out = E_ests[E_ests$stratum == "total",c("date", "estimate")]
colnames(trips_out)[2] = "N_hat"

# 2.) format harvest estimates
total_harv = subset(H_ests, quantity == "mean" & stratum == "total")
mean_harv = reshape2::dcast(total_harv, date ~ species, value.var = "estimate")

# correct for tiny rounding issues
# (mean_harv$chinook + mean_harv$chum + mean_harv$sockeye) - mean_harv$total
mean_harv[,"total"] = rowSums(mean_harv[,c("chinook", "chum", "sockeye")])
colnames(mean_harv)[2:5] = paste0(colnames(mean_harv)[2:5], "_mean")
harv_out = mean_harv

# 3.) Get number of interviews
int_counts = with(I_data, tapply(trip_start, date(trip_start), length))
int_out = data.frame(date = as_date(names(int_counts)), n = unname(int_counts))

# 4.) get opener duration
duration_out = data.frame(
  date = report_dates,
  hours_open = ceiling(as.numeric(as.period(interval(M_info$start, M_info$end)), units = "hours"))
)

# 5.) get catch per trip
cpt_out = apply(mean_harv[,2:5], 2, function(x) round(x/trips_out$N_hat, digits = 1))
cpt_out = cbind(date = mean_harv$date, as.data.frame(cpt_out))
colnames(cpt_out) = str_replace(colnames(cpt_out), "mean", "cpt")

# 6.) combine
out = merge(int_out, trips_out, by = "date") |> 
  merge(duration_out, by = "date") |> 
  merge(harv_out, by = "date") |> 
  merge(cpt_out, by = "date")

# 7.) format date/year
mn = as.character(month(out$date, label = TRUE))
dy = day(out$date)
yr = year(out$date)
out = cbind(year = yr, date_nice = paste(dy, mn), out)

# 8.) get year totals
mean_harv_tot = data.frame(date = "Total",
                           chinook_mean = with(mean_harv, tapply(chinook_mean, year(date), sum)),
                           chum_mean = with(mean_harv, tapply(chum_mean, year(date), sum)),
                           sockeye_mean = with(mean_harv, tapply(sockeye_mean, year(date), sum)),
                           total_mean = with(mean_harv, tapply(total_mean, year(date), sum))
)

int_tot = data.frame(date = "Total", n = with(int_out, tapply(n, year(date), sum)))
duration_tot = data.frame(date = "Total", hours_open = with(duration_out, tapply(hours_open, year(date), sum)))
trips_tot = data.frame(date = "Total", N_hat = with(trips_out, tapply(N_hat, year(date), sum)))
cpt_tot = data.frame(date = "Total",
                     chinook_cpt = rep(NA, nrow(trips_tot)),
                     chum_cpt = rep(NA, nrow(trips_tot)),
                     sockeye_cpt = rep(NA, nrow(trips_tot)),
                     total_cpt = rep(NA, nrow(trips_tot))
)
out_tot = cbind(date_nice = "Total", n = int_tot[,-1], N_hat = round(trips_tot[,-1]), hours_open = duration_tot[,-1], mean_harv_tot[,-1], cpt_tot[,-1])
out_tot = cbind(year = as.numeric(rownames(out_tot)), date = NA, out_tot)
rownames(out_tot) = NULL

# loop through years and add each year's total to the bottom of each year-block
# then rbind each year block back together into the large data frame
obs_harv_ests = do.call(rbind, lapply(1:nrow(out_tot), function(y) rbind(subset(out, year == out_tot$year[y]), out_tot[y,])))

# save the file
write.csv(obs_harv_ests, file.path(table_dir, "obs-harv-ests.csv"), row.names = FALSE)
