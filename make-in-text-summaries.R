
# set up session
source(file.path(this.path::this.proj(), "session-setup.R"))

# run the validation analysis
source(file.path(proj_dir, "validation/validation-analysis.R"))

# number of communities sampled by ISMP
nv_ISMP = nrow(v_df)

# number of communities sampled by PSMP
nv_PSMP = length(unique(harv_all$village))

# function for reporting summaries: a single number
report_num = function(x, is_percent = FALSE, digits = 0) {
  if(is_percent) {
    out = KuskoHarvUtils::percentize(x, digits = digits)
  } else {
    out = prettyNum(round(x, digits = digits), big.mark = ",")
  }
  return(out)
}

# function for reporting summaries: the range of a vector
report_range = function(x, is_percent = FALSE, digits = 0) {
  paste(prettyNum(round(range(x, na.rm = TRUE), digits), big.mark = ","), collapse = "-")
}

##### IN-TEXT NUMBERS: SECTION 'METHODS > STUDY AREA, SPECIES, AND TIMEFRAME #####

# ANS ranges by species
chinook_ans = c(67200,109800)
chum_ans = c(41200,116400)
sockeye_ans = c(32200,58700)

# which years to calculate proportional summary statistics
p_yrs = 2010:2021

# calculate total harvest by year from PSMP
all_totals = harv_all %>%
  filter(year %in% p_yrs) %>%
  group_by(year, spp) %>%
  summarize(all_harvest = sum(harvest, na.rm = TRUE))

# calculate proportion taken by communities within ISMP study area
p = harv_all %>%
  filter(year %in% p_yrs & village %in% v_df$village) %>%
  group_by(year, spp) %>%
  summarize(partial_harvest = sum(harvest, na.rm = TRUE), .groups = "drop") %>%
  left_join(all_totals, by = c("year", "spp")) %>%
  mutate(p = partial_harvest/all_harvest) %>%
  group_by(spp) %>%
  summarize(mean_p = mean(p), .groups = "drop")

# average total harvest across these years
mean_harv = all_totals %>%
  group_by(spp) %>%
  summarize(mean_harv = round(mean(all_harvest), -2))

# format for printing in-text
p_print = KuskoHarvUtils::percentize(p$mean_p); names(p_print) = p$spp; names(p_print)[2:3] = tolower(names(p_print)[2:3])
h_print = prettyNum(mean_harv$mean_harv, big.mark = ",", scientific = FALSE); names(h_print) = mean_harv$spp

##### IN-TEXT NUMBERS: SECTION 'METHODS > DATA COLLECTION > ACCESS POINT COMPLETED TRIP INTERVIEWS #####

# calculate the proportion of all interviews came from each source
p_source = KuskoHarvUtils::percentize(table(I_data$source)/sum(table(I_data$source)))

# which year(s) did ADF&G sample?
uADFG_yrs = knitr::combine_words(unique(year(I_data$trip_start[I_data$source == "ADFG"])))

# the years in ISMP
yr_range = paste0(min(year(I_data$trip_start), na.rm = TRUE), " -- ", max(year(I_data$trip_start), na.rm = TRUE))

##### IN-TEXT NUMBERS: SECTION 'METHODS > ESTIMATORS > UNCERTAINTY ESTIMATION' #####

# get cv by opener, species and stratum
y = H_ests |> 
  filter(quantity %in% c("mean", "sd")) |> 
  dcast(date + species + stratum ~ quantity, value.var = "estimate") |> 
  mutate(year = year(date), cv = sd/mean)

y |> arrange(desc(cv))

# summarize across openers within years
z = y |> 
  group_by(year, species, stratum) |> 
  summarize(mean_cv = mean(cv, na.rm = TRUE),
                   min_cv = min(cv, na.rm = TRUE),
                   max_cv = max(cv, na.rm = TRUE))

# stratum totals
z |> 
  filter(species == "chinook" & stratum == "total")
z |> 
  filter(species == "chum" & stratum == "total")
z |> 
  filter(species == "sockeye" & stratum == "total")

##### IN-TEXT NUMBERS: SECTION 'RESULTS FIRST PARAGRAPH' #####

obs_harv_ests = read.csv(file.path(table_dir, "obs-harv-ests.csv"))
obs_harv_ests$date = as_date(obs_harv_ests$date)

n_openers = nrow(obs_harv_ests[obs_harv_ests$date_nice != "Total",])
p_12_hrs = nrow(obs_harv_ests[obs_harv_ests$hours_open == 12,])/n_openers
p_6_hrs = nrow(obs_harv_ests[obs_harv_ests$hours_open == 6,])/n_openers
p_9_hrs = nrow(obs_harv_ests[obs_harv_ests$hours_open == 9,])/n_openers
p_24_hrs = nrow(obs_harv_ests[obs_harv_ests$hours_open == 24,])/n_openers
mean_days_closed = mean(as.numeric(diff(obs_harv_ests$date) - 1), na.rm = TRUE)

# how long were trips
p_trips_longer_than_4hrs = sapply(unique(date(I_data$trip_start)), function(d) {
  I_data_sub = subset(I_data, date(trip_start) == d)
  I_data_sub$duration = as.numeric(I_data_sub$trip_duration, unit = "hours")
  mean(I_data_sub$duration > 4, na.rm = TRUE)
})
mean(p_trips_longer_than_4hrs > 0.5)

##### IN-TEXT NUMBERS: SECTION 'RESULTS > PATTERNS IN ISMP DATA COLLECTION' #####

# number of flights per opener
n_flights = table(apply(obs_effort_ests[,c("X1", "X2", "X3")], 1, function(x) sum(!is.na(x))))

# flight counts to use as an example for presenting intuition behind effort estimator 
ex_counts = unlist(obs_effort_ests[obs_effort_ests$date == "2019-06-12",c("X1", "X2", "X3", "Y", "X1&Y", "X2&Y", "X3&Y", "X1&X2&Y", "X2&X3&Y")])

# get the fraction of interviews that came from Bethel vs. non-bethel sources
UIDS = unique(I_data$UID[year(I_data$trip_start) != 2016])  # unique opener ids
f = function(uid) {
  # extract only BBH and CBM records for this opener
  idat_o = subset(I_data, UID == uid & source %in% c("BBH", "CBM"))
  counts = table(idat_o$source)
  (counts["BBH"])/sum(counts)
}
p = sapply(UIDS, f)

# summarize these for presentation in-text
mean_p_bethel = KuskoHarvUtils::percentize(mean(p))
range_p_bethel = paste(KuskoHarvUtils::percentize(range(p)), collapse = "-")
sd_p_bethel = round(sd(p) * 100)

# compare soak times and catch rates to Bethel and non-bethel fishers
I_data$is_bethel = I_data$source == "BBH"
I_data$catch = with(I_data, chinook + chum + sockeye)
soaks = tapply(as.numeric(I_data$soak_duration, unit = "hours"), I_data$is_bethel, mean, na.rm = TRUE)
catches = tapply(I_data$catch, I_data$is_bethel, mean, na.rm = TRUE)
non_bethel_soak_ratio = soaks[1]/soaks[2]
non_bethel_catch_ratio = catches[1]/catches[2]

##### IN-TEXT NUMBERS: SECTION 'RESULTS > PATTERNS IN ISMP ESTIMATES' #####

# calculate june 12 catch per trip
jun12_chinook_cpt = obs_harv_ests[obs_harv_ests$date_nice == "12 Jun","chinook_cpt"]
jun12_chum_cpt = obs_harv_ests[obs_harv_ests$date_nice == "12 Jun","chum_cpt"]
jun12_sockeye_cpt = obs_harv_ests[obs_harv_ests$date_nice == "12 Jun","sockeye_cpt"]

# calculate percent of harvest that was chinook by opener
obs_harv_ests$p_chinook = with(obs_harv_ests, chinook_mean/total_mean)

# break harvest estimates into pre and post june 23; averages and ranges presented in text are summaries of these
pre_jun23 = obs_harv_ests[obs_harv_ests$date <= as_date(paste0(obs_harv_ests$year, "-", "06-23")) & !is.na(obs_harv_ests$date),]
post_jun23 = obs_harv_ests[obs_harv_ests$date > as_date(paste0(obs_harv_ests$year, "-", "06-23")) & !is.na(obs_harv_ests$date),]

# calculate total harvest across years
year_totals = with(obs_harv_ests[!is.na(obs_harv_ests$date),], tapply(total_mean, year, sum))
min_H_total = min(year_totals); min_Y_total = names(year_totals)[which.min(year_totals)]
max_H_total = max(year_totals); max_Y_total = names(year_totals)[which.max(year_totals)]
chinook_totals = with(obs_harv_ests[!is.na(obs_harv_ests$date),], tapply(chinook_mean, year, sum))

# how many openers were there per year
openers_per_year = with(obs_harv_ests[!is.na(obs_harv_ests$date),], tapply(total_mean, year, length))

# how many hours open per year
hours_open_per_year = with(obs_harv_ests[!is.na(obs_harv_ests$date),], tapply(hours_open, year, sum))

# total harvest per hour by season
harvest_per_hour = year_totals/hours_open_per_year

# calculate harvest per hour by opener
obs_harv_ests$harvest_per_hour = obs_harv_ests$total_mean/obs_harv_ests$hours_open

##### IN-TEXT NUMBERS: SECTION 'RESULTS > VALIDATION OF ISMP HARVEST ESTIMATES' #####

spp = c("chinook", "chum", "sockeye")
errors = lapply(KuskoHarvUtils::capitalize(spp), function(s) {
  # extract the ISMP and PSMP annual totals for comparison
  sub = subset(values, spp == s & stratum == "total" & stat == "est")
  ismp = sub$value[sub$prgm == "ISMP"]
  psmp = sub$value[sub$prgm == "PSMP"]
  
  # summarize the errors
  errors = KuskoHarvUtils::get_errors(yhat = ismp, yobs = psmp)
  names(errors$error) = sort(unique(sub$year))
  names(errors$p_error) = sort(unique(sub$year))
  errors
})
names(errors) = spp

# extract and format the correlation and mean percent error
RHOS = sapply(errors, function(s) KuskoHarvUtils::percentize(s$summary["RHO"]) |> unname())
MPES = sapply(errors, function(s) KuskoHarvUtils::percentize(s$summary["MPE"]) |> unname())

# get mean percent error for chinook by spatial stratum
chinook_MPE = sapply(c("A", "B", "C", "D1"), function(s) {
  sub = subset(values, spp == "Chinook" & stratum == s & stat == "est")
  ismp = sub$value[sub$prgm == "ISMP"]
  psmp = sub$value[sub$prgm == "PSMP"]
  errors = KuskoHarvUtils::get_errors(yhat = ismp, yobs = psmp)
  unname(KuskoHarvUtils::percentize(errors$summary["MPE"]))
})

# 'p-covered' = PSMP corrected divided by PSMP non-corrected
# intended to represent the proportion of season-wide harvest captured by ISMP for the 11 communities

# get the total harvest by villages in the ISMP, but estimated by the PSMP
# and prior to any corrections
uncorrected = truth_pre_correct |> 
  group_by(spp, year) |> 
  summarize(uncorrected = sum(harvest))

# get the fraction that was left after corrections were applied
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

rownames(p_covered) = c("chinook", "chum", "sockeye")

# calculate the average p_covered by species across years
mean_p_covered = rowMeans(p_covered)

# for each species, get the year with the min and max p_covered
min_p_covered = apply(p_covered, 1, which.min)
max_p_covered = apply(p_covered, 1, which.max)

yrs = 2016:2023

# Chinook: get min and max p-covered and the years they happened
chinook_minp = report_num(p_covered["chinook",min_p_covered["chinook"]], is_percent = TRUE)
chinook_minp_Y = yrs[min_p_covered["chinook"]]
chinook_maxp = report_num(p_covered["chinook",max_p_covered["chinook"]], is_percent = TRUE)
chinook_maxp_Y = yrs[max_p_covered["chinook"]]

# chum: get min and max p-covered and the years they happened
chum_minp = report_num(p_covered["chum",min_p_covered["chum"]], is_percent = TRUE)
chum_minp_Y = yrs[min_p_covered["chum"]]
chum_maxp = report_num(p_covered["chum",max_p_covered["chum"]], is_percent = TRUE)
chum_maxp_Y = yrs[max_p_covered["chum"]]

# sockeye: get min and max p-covered and the years they happened
sockeye_minp = report_num(p_covered["sockeye",min_p_covered["sockeye"]], is_percent = TRUE)
sockeye_minp_Y = yrs[min_p_covered["sockeye"]]
sockeye_maxp = report_num(p_covered["sockeye",max_p_covered["sockeye"]], is_percent = TRUE)
sockeye_maxp_Y = yrs[max_p_covered["sockeye"]]

##### IN-TEXT NUMBERS: SECTION 'RESULTS > EVALUATION OF EFFORT ESTIMATOR VIA STOCHASTIC SIMULATION > SIMPLE SIMULATION #####

# read in effort estimates
obs_effort_ests = read.csv(file.path(table_dir, "obs-effort-ests.csv"), check.names = FALSE)

# get the pi and psi estimates for the opener with their lowest values
lowest_p_info = round(unlist(obs_effort_ests[obs_effort_ests$date == "2016-06-23",c("pi1_hat", "pi2_hat", "psi_hat")]), 2)
