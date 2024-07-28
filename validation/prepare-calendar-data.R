
# load the opener meta-data: for info about the timing of openers monitored by the in-season program
data(openers_all, package = "KuskoHarvData")

# read in the data file
dat_raw = read.csv(file.path(data_dir, "Calendar Data.csv"))

# rename the "location" column header to "village"
dat_raw = rename(dat_raw, village = location)

# the villages in each calendar geographical grouping
# i've assigned these based on rough spatial locale.
# these do not need to match up with the in-season geographical strata
v_list = list(
  lwr = c("Tuntutuliak"),
  tundra = c("Atmautluak", "Kasigluk", "Nunapitchuk"),
  mid1 = c("Napakiak", "Napaskiak", "Oscarville"),
  mid2 = c("Bethel", "Kwethluk"), 
  upr = c("Akiachak", "Akiak")
)

# convert this to a data frame; useful later
v_df = data.frame(
  group = unlist(lapply(1:length(v_list), function(i) rep(names(v_list)[i], length(v_list[[i]])))),
  village = unlist(v_list)
); rownames(v_df) = NULL

# retain the specified order of groups and villages
v_df$group = factor(v_df$group, levels = c(unique(v_df$group), "aggrt"))
v_df$village = factor(v_df$village, levels = v_df$village)

# discard any records from villages not in in-season study area
dat_raw = dat_raw[dat_raw$village %in% unlist(v_list),]

# keep only years that have in-season data
dat_raw = dat_raw[dat_raw$year %in% unique(year(openers_all$start)),]

# years to calculate calendars for
yrs = unique(dat_raw$year)

# how many records 
table(factor(dat_raw$village, levels = levels(v_df$village)), dat_raw$year)

# function to prepare the calendar data
calendar_prep = function(yr, v) {
  
  # extract only the year supplied in the yr argument
  dat = dat_raw[dat_raw$year == yr,]
  
  # extract only villages supplied in v argument
  dat_v = unique(dat$village)
  keep_v = dat_v[dat_v %in% v]
  if (length(keep_v) == 0) keep_v = dat_v
  dat = dat[dat$village %in% keep_v,]
  
  # fix dates: original was M/D/YYYY; want it as_date()
  mn = str_pad(str_extract(dat$date, "^[:digit:]+"), 2, "left", "0")
  dy = str_pad(str_remove_all(str_extract(dat$date, "/[:digit:]+/"), "/"), 2, "left", "0")
  dat$date = as_date(paste(yr, mn, dy, sep = "-"))
  
  # these dates should capture the time period that the post-season harvest estimates cover
  earliest_keep_date = as_date(paste0(yr, "06-01"))
  latest_keep_date = as_date(paste0(yr, "09-30"))
  
  empty = data.frame(date = seq(earliest_keep_date, latest_keep_date, by = "day"))
  
  # extract only important variables
  dat = dat[,c("date", "chinook", "chum", "sockeye")]
  
  dat %<>% 
    group_by(date) %>%
    summarize(chinook = sum(chinook, na.rm = TRUE),
              chum = sum(chum, na.rm = TRUE),
              sockeye = sum(sockeye, na.rm = TRUE), .groups = "drop") %>%
    filter(date >= earliest_keep_date & date <= latest_keep_date) %>%
    merge(empty, by = "date", all = TRUE)
  dat[is.na(dat)] = 0
  
  dat = dat %>%
    mutate(p_chinook = cumsum(chinook)/sum(chinook),
           p_chum = cumsum(chum)/sum(chum),
           p_sockeye = cumsum(sockeye)/sum(sockeye)) %>%
    select(date, starts_with("p_"))
  
  return(dat)
}

# obtain the calendar summaries for each group and year
# combines the calendars for all villages in a group before calculating daily proportion
# year calculations done independently
calendars = lapply(names(v_list), function(x) {
  out = do.call(rbind, lapply(yrs, calendar_prep, v = v_list[[x]]))
  out = cbind(group = x, out)
  return(out)
}); calendars = do.call(rbind, calendars)

# calendars$group = factor(calendars$group, levels = levels(v_df$group))

# obtain the calendar summaries for the aggregate of all villages (i.e., remove group structure)
calendars_agg = do.call(rbind, lapply(yrs, calendar_prep, v = unlist(v_list)))

# obtain averages to plug in for 2017 chum/sockeye
avg_calendars_agg = calendars_agg |>
  mutate(day = KuskoHarvUtils::to_days_past_may31(date)) |>
  group_by(day) |>
  summarize(p_chinook = mean(p_chinook, na.rm = TRUE),
            p_chum = mean(p_chum, na.rm = TRUE),
            p_sockeye = mean(p_sockeye, na.rm = TRUE))

calendars_agg$p_chum[year(calendars_agg$date) == 2017] = avg_calendars_agg$p_chum
calendars_agg$p_sockeye[year(calendars_agg$date) == 2017] = avg_calendars_agg$p_sockeye

# add on the aggregate to the stratefied estimates
calendars_agg = cbind(group = "aggrt", calendars_agg)

# combine aggregate with groups
calendars = rbind(calendars, calendars_agg)

# get the last opener that was monitored in each year
last_opener = lapply(split(openers_all[openers_all$flights_flown > 0,], year(openers_all$start[openers_all$flights_flown > 0])), function(df) date(df$start[nrow(df)]))

# cumulative harvest proportion by year and species up to and including the last monitored opener
p_timing = do.call(rbind, lapply(last_opener, function(d) calendars[calendars$date == d,]))
rownames(p_timing) = NULL
p_timing$year = year(p_timing$date)
p_timing$group = factor(p_timing$group, levels = levels(v_df$group))
p_timing = p_timing[,c("year", "group", "p_chinook", "p_chum", "p_sockeye")]

# fill in the 2017 missing chum/sockeye values based on average of other years
p_timing$p_chum[p_timing$year == 2017] = tapply(p_timing$p_chum, p_timing$group, mean, na.rm = TRUE)
p_timing$p_sockeye[p_timing$year == 2017] = tapply(p_timing$p_sockeye, p_timing$group, mean, na.rm = TRUE)

# fill in the 2021 missing uppr timing group for chum
p_timing$p_chum[p_timing$year == 2021 & p_timing$group == "upr"] = mean(p_timing$p_chum[p_timing$year == 2021], na.rm = TRUE)

# reformat: this is the output used later
colnames(p_timing) = c("year", "group", "Chinook", "Chum", "Sockeye")
p_timing = melt(p_timing, id.vars = c("year", "group"), value.name = "p_timing", variable.name = "spp")
