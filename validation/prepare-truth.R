# Division of commercial fisheries subsistence harvest reports:
# season-wide, village-specific total harvest estimates found in
# Tables 3, 4, 5 for Chinook, chum, and sockeye, respectively
# Table 10 for composition of reported primary harvesting gear

# 2016: http://www.adfg.alaska.gov/FedAidPDFs/FDS19-09.pdf
# 2017: http://www.adfg.alaska.gov/FedAidPDFs/FDS21-07.pdf

# Division of subsistence; subsistence harvest reports:
# 2018: http://www.adfg.alaska.gov/techpap/TP%20467.pdf
# 2019: http://www.adfg.alaska.gov/techpap/TP475.pdf
# 2020: http://www.adfg.alaska.gov/techpap/TP483.pdf
# 2021: http://www.adfg.alaska.gov/techpap/TP489.pdf
# 2022: https://www.adfg.alaska.gov/techpap/TP502.pdf
# 2023: Unpublished, data provided by D. Koster

# prepare timing data (post-season source)
source(file.path(proj_dir, "validation/prepare-calendar-data.R"))

# prepare gear usage data (post-season source)
gear = read.csv(file.path(data_dir, "post-season-gear-comps.csv"))
gear$p_drift = gear$drift/rowSums(gear[,-which(colnames(gear) %in% c("village", "year"))])
gear = gear[,c("village", "year", "p_drift")]
gear$p_drift[is.na(gear$p_drift) & gear$village == "Bethel"] = mean(gear$p_drift[gear$village == "Bethel"], na.rm = TRUE)

# load/prepare all years/villages of harvest data (post-season source)
harv_all = read.csv(file.path(data_dir, "post-season-harvest-ests.csv"))
harv_all %<>%
  melt(id.vars = c("spp", "village"), value.name = "harvest", variable.name = "year") %>%
  mutate(year = as.numeric(str_remove(year, "X"))) %>%
  select(village, year, spp, harvest)

# filter post-season harvest data to only relevant years/villages
harv = harv_all %>%
  filter(village %in% v_df$village & year %in% p_timing$year)

# create the "truth" data set with the gear correction proportions
truth = merge(harv, gear, by = c("year", "village"))

# merge the truth data set with the village grouping data set for timing apportionment
truth = merge(truth, v_df, by = c("village"))

# merge the truth data with the timing correction proportions
truth = merge(truth, p_timing, by = c("year", "spp", "group"))

# save the "pre-correction truth" data set
truth_pre_correct = truth

# reduce harvest to only taken during monitored part of season and using drift nets
truth$harvest = round(truth$harvest * truth$p_drift * truth$p_timing)

# drop the "group" variable, no longer needed, and may just confuse with the stratum used below
truth = truth[,-which(colnames(truth) == "group")]

# obtain stratum-specific estimates for each village
source(file.path(proj_dir, "make-tables.R"))
stratum_counts = read.csv(file.path(table_dir, "interviews-by-village-and-stratum.csv"))
stratum_counts = stratum_counts[stratum_counts$village %in% v_df$village,]
strata = c("A", "B", "C", "D1")
p_strata = paste("p", strata, sep = "_")
stratum_props = t(apply(stratum_counts[,strata], 1, function(x) x/sum(x)))
stratum_props = data.frame(stratum_props)
colnames(stratum_props) = p_strata
stratum_props = cbind(village = stratum_counts$village, stratum_props)
NA_props = stratum_props[1:2,]
NA_props[,p_strata] = NA
NA_props$village = c("Nunapitchuk", "Oscarville")
stratum_props = rbind(stratum_props, NA_props)
stratum_props[stratum_props$village == "Nunapitchuk",p_strata] = 
  colMeans(stratum_props[stratum_props$village %in% c("Atmautluak", "Kasigluk"),p_strata])
stratum_props[stratum_props$village == "Oscarville",p_strata] = 
  colMeans(stratum_props[stratum_props$village %in% c("Napakiak", "Napaskiak"),p_strata])
truth = merge(truth, stratum_props, by = "village", all = TRUE)

truth %<>% 
  mutate(A = harvest * p_A,
         B = harvest * p_B,
         C = harvest * p_C,
         D1 = harvest * p_D1,
         ) %>%
  select(village, spp, year, A, B, C, D1) %>%
  group_by(spp, year) %>%
  summarize(A = sum(A), B = sum(B), C = sum(C), D1 = sum(D1), .groups = "drop") %>%
  as.data.frame

truth[,strata] = t(sapply(1:nrow(truth), function(i) KuskoHarvUtils::smart_round(as.numeric(truth[i,strata]))))
truth = cbind(truth, total = rowSums(truth[,strata]))
