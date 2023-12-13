
# set up session
source(file.path(this.path::this.proj(), "session-setup.R"))

# prepare the best estimates of true harvest
# derived from post-season data*
# *in-season interviews used to inform how the total harvest is distributed spatially
source(file.path(proj_dir, "validation/prepare-truth.R"))

# load the in-season harvest estimates
data(harvest_estimate_master, package = "KuskoHarvData")

# give it a shorter name
ests = harvest_estimate_master
head(ests)

# these are the total estimates as obtained by the in-season program
ests %<>%
  mutate(year = year(date), species = KuskoHarvUtils::capitalize(species)) %>%
  filter(quantity == "mean" & species != "Total") %>%
  group_by(year, stratum, species) %>%
  summarize(est = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  reshape2::dcast(year + species ~ stratum, value.var = "est") %>%
  filter(year %in% truth$year) %>%
  rename(spp = species)

# correct any slight rounding errors
ests$total = rowSums(ests[,strata])

# extract an "id" data frame
id = ests[,c("year", "spp")]

# calculate errors
errors = ests[,c(strata, "total")] - truth[,c(strata, "total")]
p_errors = round(errors/truth[,c(strata,"total")], 2)

# combine estimates
values = rbind(
  cbind(stat = "est", prgm = "ISMP", ests),
  cbind(stat = "est", prgm = "PSMP", truth),
  cbind(stat = "error", prgm = "ISMP", id, errors),
  cbind(stat = "p_error", prgm = "ISMP", id, p_errors)
)

# add spatial composition
est_comp = t(apply(values[values$stat == "est" & values$prgm == "ISMP",strata], 1, function(x) KuskoHarvUtils::smart_round(x/sum(x), 2)))
truth_comp = t(apply(values[values$stat == "est" & values$prgm == "PSMP",strata], 1, function(x) KuskoHarvUtils::smart_round(x/sum(x), 2)))
values = rbind(
  values,
  cbind(stat = "comp", prgm = "ISMP", id, est_comp, total = rowSums(est_comp)),
  cbind(stat = "comp", prgm = "PSMP", id, truth_comp, total = rowSums(truth_comp))
)

# reshape to put strata as a new variable
values = melt(values, id.vars = c("stat", "prgm", "year", "spp"), variable.name = "stratum", value.name = "value")
