#getData is a script that filters species data from eBird, including absence data
#getData_curlew assumes that data has been downloaded already. Data can be downloaded from
# the eBird data repository at: https://ebird.org/data/download (requires a Cornell account).

#For this analysis I have downloaded all records for Far Eastern Curlews using the custom download
#feature of eBird. I also downloaded the sampling event data. Version is June 2020.
#data comes as a tarball, which I have extracted to an external drive (D:)

#get the auk package to manipulate the eBird data. 
#more info, see: https://cran.r-project.org/web/packages/auk/vignettes/auk.html
library(auk)
library(dplyr)
library(lubridate)
library(tidyverse)

speciesName <- "Far Eastern Curlew"

#Check species names in ebird_taxonomy dataframe-- i.e. Limosa lapponica baueri/menzbieri; Bar-tailed Godwit (Siberian)

workingdir <- getwd()
datadir <- './Data_files'
scriptdir <- './R_scripts'
outputdir <- './Outputs'
#workingdir <- 'C:/Users/nic24k/Dropbox/Migratory birds network reconstruction/Data analysis- node definition'
#'C:/Users/nic24k/Documents/birdnet'

setwd(workingdir)

#path to ebird data file
f_ebd <- paste(datadir, '/eBird sightings data/',tolower(speciesName),'/ebd_faecur_relJun-2020/ebd_faecur_relJun-2020.txt', sep="")
f_smp <- paste(datadir, '/eBird sightings data/ebd_sampling_relJun-2020/ebd_sampling_relJun-2020.txt', sep="")

filters <- auk_ebd(f_ebd, file_sampling = f_smp) %>% 
  auk_country(country = c("US","Russia", "Mongolia", "China","Hong Kong", "Macao", "South Korea", "North Korea", "Taiwan",
                          "Japan","Philippines", "Vietnam","Cambodia", "Laos", "Thailand", "Myanmar",
                          "Bangladesh", "India", "Malaysia", "Singapore", "Brunei", "Indonesia","Timor leste",
                          "Papua New Guinea", "Australia", "CC", "New Zealand", "FM",
                          "MP","PW","SB","VU","WS") ) %>% 
  #auk_date(date = c("1970-01-01", "2018-09-01")) %>%
  auk_complete() 
filters  #check filters via printing

setwd(datadir)
#define output files and run filter-- takes ages so comment out if not using. Outputs are ebd-filtered and sampling_filtered
ebd_sed_filtered <- auk_filter(filters, 
                                 file = "ebd-filtered.txt",
                                 file_sampling = "sampling-filtered.txt", overwrite = TRUE)
memory.limit(size=16000000000)

#combine datasets to get zero-filled dataset
ebd_zf <- auk_zerofill("ebd-filtered.txt", "sampling-filtered.txt" )
ebd_zf



#collapse dataframes together

ebd_zf_dfx <- collapse_zerofill(ebd_zf)
class(ebd_zf_dfx)

#get rid of unnecessary columns to reduce the memory required to store the object: list columns we want to keep
ebd_zf_df <- ebd_zf_dfx[, c("checklist_id", "country", "country_code", "iba_code","state", "state_code","locality_id","locality_type",
                           "latitude", "longitude", "observation_date", "sampling_event_identifier", "time_observations_started",
                           "duration_minutes", "effort_distance_km", "effort_area_ha", "number_observers", "scientific_name",
                           "observation_count", "species_observed", "protocol_type")] 
#dump all USA records that are not from AK
ebd_zf_df <- ebd_zf_df[!(ebd_zf_df$country_code== "US" & ebd_zf_df$state != "AK"), ]

#dump any records from western Russia by setting limits on the longitude
ebd_zf_df <- ebd_zf_df[!(ebd_zf_df$longitude <80 & ebd_zf_df$longitude>= -141),]

# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

#clean up
ebd_zf_df2 <- ebd_zf_df %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
   # effort_distance_km = if_else(protocol_type != "Traveling", 
    #                             0, effort_distance_km),
    # convert time to decimal hours since midnight
    time_observations_started = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )


# additional filtering-- get rid of long duration and long distance checklists
# see https://cornelllabofornithology.github.io/ebird-best-practices/ebird.html 
ebd_zf_filtered <- ebd_zf_df2 %>% 
  filter(
    # effort filters
    duration_minutes <= 5 * 60,
    effort_distance_km <= 5,
    # data since 1980 only
    year >= 1980,
    # 10 or fewer observers
    number_observers <= 10)
#can write to csv with the line below but note that the file is very large-- more efficient to save the workspace and re-load when you run format_weekly_data.R
#write_csv(ebd_zf_filtered, "eastern_curlew_all_eBird2020.csv", na = "")

setwd(workingdir)
setwd(outputdir)
#save workspace to output directory
saveRDS(ebd_zf_filtered, "eastern_curlew_all_eBird2020Rds")

setwd(workingdir)

#---------------------


 


