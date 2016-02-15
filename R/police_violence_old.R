# Original attempt…overlooked second 
# Attempt to aggregate violence data and separately-downloaded BLS statistics on police department size and served population

# Makeover Monday challenge 2016-02-08
# \url{http://vizwiz.blogspot.com/p/makeover-monday-challenges.html}
# Original graphs at \url{http://mappingpoliceviolence.org/2015/}
# Mapping police violence data from \url{http://vizwiz.blogspot.com/p/makeover-monday-challenges.html}
# Zip code tabulation area population data from http://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml
# Police department data from \url{http://www.bjs.gov/index.cfm?ty=pbdetail&iid=5279}

library(magrittr)
library(readxl)
library(readr)
#library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(qcc)
library(Cairo)

source("~/Documents/R/qcc_ggplot/qcc.plot.R")

if (!file.exists("data/MPVDataset_ind.rds")){
  if (!file.exists("data-raw/MPVDatasetDownload-8s8q.xlsx"))
    download.file("http://mappingpoliceviolence.org/s/MPVDatasetDownload-8s8q.xlsx", "data-raw/MPVDatasetDownload-8s8q.xlsx")
  pv_ind_df <- read_excel("data-raw/MPVDatasetDownload-8s8q.xlsx", sheet = 1)
  pv_ind_df <- pv_ind_df[,-(20:NCOL(pv_ind_df))]
  old_names <- colnames(pv_ind_df)
  new_names <- c("Victim_name", "Victim_age", "Victim_gender","Victim_race", "URL_image", "Date_injury", "Address_injury", "Location_city", "Location_state", "Location_zip", "Location_county", "Agency_responsible", "Cause_death", "Description", "Official_disposition", "Criminal_charges", "Link_news", "Mental_illness", "Unarmed")
  colnames(pv_ind_df) <- new_names
  for (i in 1:length(old_names)) {
    attr(pv_ind_df[[i]], "original_name") <- old_names[i]
  }
  c2f <- new_names[c(3, 4, 8:13, 15, 16, 18, 19)]
  c2n <- new_names[c(2)]
  pv_ind_df %<>% mutate_each_(funs(factor), c2f) %>% 
    mutate_each_(funs(as.numeric), c2n) %>% 
    mutate(week = wee)
  saveRDS(object = pv_ind_df, file = "data/MPVDataset_ind.rds")
} else {
  pv_ind_df <- readRDS("data/MPVDataset_ind.rds")
}

# Control chart for days between events
# pv_date <- sort(pv_ind_df$Date_injury)
# pv_days <- (pv_date[2:NROW(pv_date)] - pv_date[1:(NROW(pv_date) - 1)])/86400
# pv_days_m <- matrix(c(pv_days[1:(NROW(pv_days) - 1)], pv_days[2:NROW(pv_days)]), ncol = 2)
# 
# qcc(pv_days, type = "xbar.one")
# qcc(pv_days_m, type = "R")
# Insufficient resolution...almost all points are 0, 1 or 2

# Control chart for events/day
pv_agg <- pv_ind_df %>% group_by(Date_injury) %>% 
  summarise(deaths = length(Victim_name)) %>% 
  mutate(year = year(Date_injury))

all_dates <- seq(from = range(pv_agg$Date_injury)[1], to = range(pv_agg$Date_injury)[2], by = "day")

all_dates_df <- data.frame(Date_injury = all_dates[which((all_dates %in% pv_agg$Date_injury) == FALSE)], 
                           deaths = as.integer(rep(0, times = length(which((all_dates %in% pv_agg$Date_injury) == FALSE)))),
                           year = as.integer(year(all_dates[which((all_dates %in% pv_agg$Date_injury) == FALSE)])))

pv_agg <- rbind(pv_agg, all_dates_df)
pv_agg <- pv_agg[order(pv_agg$Date_injury),]

ggplot(pv_agg, aes(x = Date_injury, y = deaths)) +
  geom_line(aes(colour = deaths, group = as.factor(year))) +
  scale_colour_continuous(low = muted("green"), high = "red", guide = FALSE) +
  ylab("Police killings per day") +
  theme_minimal() +
  geom_smooth(method = "lm", aes(group = as.factor(year)), colour = muted("blue")) +
  ggtitle("Killings by America's Largest Police Departments\n2013 - 2015") +
  theme(axis.title.x = element_blank())
ggsave("figs/killings_per_day.png", type = "cairo-png", width = 12, height = 3, units = "in")

acf(pv_agg$deaths)

qcc(pv_agg$deaths, type = "xbar.one")
qcc(matrix(c(pv_agg$deaths[1:(NROW(pv_agg)-1)], pv_agg$deaths[2:NROW(pv_agg)]), ncol = 2), type = "R")
qcc(pv_agg$deaths[which(pv_agg$year == 2013)], type = "xbar.one", newdata = pv_agg$deaths[which(pv_agg$year != 2013)])

pv_agg_cause <- pv_ind_df %>% 
  mutate(year = year(Date_injury)) %>%
  filter(year == 2015 & !is.na(Cause_death)) %>% 
  group_by(Cause_death) %>% 
  summarize(cause_count = length(Victim_name))

level_order <- pv_agg_cause[order(pv_agg_cause$cause_count), ]$Cause_death
pv_agg_cause$Cause_death <- factor(pv_agg_cause$Cause_death, levels = level_order)

ggplot(pv_agg_cause, aes(x = Cause_death, y = cause_count)) +
  geom_point() +
  coord_flip() +
  #ylim(0, 75) +
  ggtitle("Cause of deaths in 2015") +
  theme_bw() +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())

#if (file.exists("data-raw/ACS_14_5YR_B01003_with_ann.csv")) {
#  zip_df <- read_csv("data-raw/ACS_14_5YR_B01003_with_ann.csv", skip = 1, na = c("", "NA", "*****"), col_types = c("cccii"))
#  zip_df %<>% mutate_each_(funs(factor), c("Id2"))
#} else {
#  stop("Cannot find zipcode population data file.")
#}

# \url{http://www.bjs.gov/index.cfm?ty=pbdetail&iid=5279}
if (!file.exists("data/")) {
  if (!file.exists("data-raw/lpd13pppat02.csv")){
    if (!file.exists("data-raw/lpd13ppp.zip"))
      download.file("http://www.bjs.gov/content/pub/sheets/lpd13ppp.zip", "data-raw/lpd13ppp.zip", method = "libcurl")
    unzip("data-raw/lpd13ppp.zip", exdir = "data-raw")
  }
  lpd_df <- read_csv("data-raw/lpd13pppat02.csv", skip = 12, col_names = c("Department_name","Population","Sworn_num","Sworn_per10000","","Fulltime_num","Fulltime_per10000")) %>% select(-5) %>% slice(1:(NROW(lpd_df)-1))
  saveRDS(lpd_df, "data/lpd13pppat02.rds")
} else{
  lpd_df <- readRDS("data/lpd13pppat02.rds")
}



# Summarize by city
pv_ind_sum_df <- pv_ind_df %>% group_by(Location_city) %>% 
  summarise(number = length(Victim_name)) %>% 
  arrange(desc(number))

pv_ind_wk_df <- pv_ind_df %>% group_by()
ggplot(pv_ind_df) +
  geom_point(aes(x = Date_injury, y = ))

lpd_df$Department_name[c(grep(c("Miami|Baltimore"), lpd_df$Department_name))]
pv_sum_df$Location_city[c(grep("Miami|Baltimore", pv_sum_df$Location_city))]

pv_cities <- unique(pv_ind_df$Location_city)
lpd_cities <- unique(lpd_df$Department_name)



# Need to combine "Miami" and "Baltimore" matches in both dfs, or find another way to match.
# Then create a single column in each df that will act as a key to combine the two.

match_v <- charmatch(as.character(pv_sum_df$Location_city), lpd_df$Department_name)



lpd_df$Department_name[match_v %>% na.omit()]

pv_sum_df %<>% mutate(Population = lpd_df$Population[match_v])

pv_sum_df$Location_city %>% as.character()  %>% sort(., decreasing = FALSE)

lpd_df$Department_name %>% sort(., decreasing = FALSE)
