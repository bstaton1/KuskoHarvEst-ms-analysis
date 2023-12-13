
# load packages
library(lubridate)
library(dplyr)
library(reshape2)
library(stringr)

# location of the project: allows scripts to be sourced from anywhere
proj_dir = this.path::this.proj()

# location of data files for producing post-season estimates
data_dir = file.path(proj_dir, "validation/data")

# location of figure file output
figure_dir = file.path(proj_dir, "figures")

# location of table file output
table_dir = file.path(proj_dir, "tables")

# create figure and table output directories if non-existent
if (!dir.exists(figure_dir)) dir.create(figure_dir)
if (!dir.exists(table_dir)) dir.create(table_dir)

# resolution for figures
ppi = 600

# Load data sets
data(meta, package = "KuskoHarvData")
data(harvest_estimate_master, package = "KuskoHarvData")
data(effort_estimate_master, package = "KuskoHarvData")
data(interview_data_master, package = "KuskoHarvData")
data(flight_data_master, package = "KuskoHarvData")

# which dates will be reported: those not in August and that had non-zero flights
report_dates = lubridate::date(meta$start)[lubridate::month(meta$start) != 8 & meta$flights_flown > 0]

# filter out the non-reporting dates from each info set
H_ests = subset(harvest_estimate_master, date %in% report_dates)
E_ests = subset(effort_estimate_master, date %in% report_dates)
I_data = subset(interview_data_master, lubridate::date(trip_start) %in% report_dates)
F_data = subset(flight_data_master, lubridate::date(start_time) %in% report_dates)
M_info = subset(meta, lubridate::date(start) %in% report_dates)

