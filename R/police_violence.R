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
library(ggplot2)
library(scales)
library(Cairo)

if (!file.exists("data/MPVDataset.rds")){
  if (!file.exists("data-raw/MPVDatasetDownload-8s8q.xlsx"))
    download.file("http://mappingpoliceviolence.org/s/MPVDatasetDownload-8s8q.xlsx", "data-raw/MPVDatasetDownload-8s8q.xlsx")
  pv_df <- read_excel("data-raw/MPVDatasetDownload-8s8q.xlsx", sheet = 2, na = c("NA"))
  pv_df <- pv_df[-NROW(pv_df),]
  old_names <- colnames(pv_df)
  new_names <- make.names(colnames(pv_df))
  colnames(pv_df) <- new_names
  pv_df$Police.Department <- as.character(sapply(X = pv_df$Police.Department, FUN = function(x) {RIGHT(string = x, n = (nchar(x) - regexpr('. ', x) - 1))}, simplify = TRUE))
  pv_df$Police.Department[which(pv_df$Police.Department == "Average")] <- "U.S. Average"
  pv_df[[1]] <- factor(pv_df[[1]], levels = unique(pv_df[[1]]))
  pv_df[[2]] <- factor(pv_df[[2]], levels = unique(pv_df[[2]]))
  pv_df[[11]] <- as.numeric(pv_df[[11]])
  pv_totals_df <- pv_df %>% filter(Police.Department == "Cities Total" | Police.Department == "U.S. Average")
  pv_df %<>% filter(Police.Department != "Cities Total" & Police.Department != "U.S. Average")
  # for(i in 3:NCOL(pv_df))
  #   pv_df[[i]] <- as.numeric(pv_df[[i]])
  # for (i in 1:length(old_names)) {
  #   attr(pv_df[[i]], "original_name") <- old_names[i]
  #}
  saveRDS(object = pv_df, file = "data/MPVDataset.rds")
  saveRDS(object = pv_totals_df, file = "data/MPVDataset_totals.rds")
} else {
  pv_df <- readRDS("data/MPVDataset.rds")
  pv_totals_df <- readRDS("data/MPVDataset_totals.rds")
}

#if (file.exists("data-raw/ACS_14_5YR_B01003_with_ann.csv")) {
#  zip_df <- read_csv("data-raw/ACS_14_5YR_B01003_with_ann.csv", skip = 1, na = c("", "NA", "*****"), col_types = c("cccii"))
#  zip_df %<>% mutate_each_(funs(factor), c("Id2"))
#} else {
#  stop("Cannot find zipcode population data file.")
#}

# \url{http://www.bjs.gov/index.cfm?ty=pbdetail&iid=5279}
# if (!file.exists("data/")) {
#   if (!file.exists("data-raw/lpd13pppat02.csv")){
#     if (!file.exists("data-raw/lpd13ppp.zip"))
#       download.file("http://www.bjs.gov/content/pub/sheets/lpd13ppp.zip", "data-raw/lpd13ppp.zip", method = "libcurl")
#     unzip("data-raw/lpd13ppp.zip", exdir = "data-raw")
#   }
#   lpd_df <- read_csv("data-raw/lpd13pppat02.csv", skip = 12, col_names = c("Department_name","Population","Sworn_num","Sworn_per10000","","Fulltime_num","Fulltime_per10000")) %>% select(-5) %>% slice(1:(NROW(lpd_df)-1))
#   saveRDS(lpd_df, "data/lpd13pppat02.rds")
# } else{
#   lpd_df <- readRDS("data/lpd13pppat02.rds")
# }

pv_log_df <- pv_df %>% filter(!(pv_df$X2014.population..US.Census. == max(pv_df$X2014.population..US.Census.)) & pv_df$Rate.of.Police.Killings.per.Million.Population != 0) %>% 
  mutate(log_Rate = log(Rate.of.Police.Killings.per.Million.Population))
# Compare rate of killings to population
pv_log_df %>% 
  ggplot(aes(x = X2014.population..US.Census., y = Rate.of.Police.Killings.per.Million.Population)) +
  geom_point() + #aes(colour = Percent.victims.black)
  stat_smooth(method = "lm", formula = y ~ I(1/x)) +
  #geom_smooth(method = "lm", aes(x = X2014.population..US.Census., y = Rate.of.Police.Killings.per.Million.Population), formula = log(y) ~ (x)) +
  #  geom_abline(intercept = 0, slope = 1, colour = "grey") +
  #  annotate(geom = "text", x = 12, y = 11, label = "Relationship if\nit was about the violence", colour = "grey", vjust = 0, hjust = 0) +
  ylab("Police killings per million population") +
  #xlab("City population") +
  scale_x_continuous(labels = comma, breaks = c(1e6, 2e6, 3e6), name = "City population") +
  ggtitle("Smaller cities have a bigger problem") +
  theme_minimal()
ggsave(filename = "figs/bypopulation.png", type = "cairo-png", width = 6, height = 3.5, units = "in")


# Compare rate of killings to violent crime rate
ggplot(pv_df, aes(x = Violent.Crime.per.1.000.residents, y = Rate.of.Police.Killings.per.Million.Population)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "blue") +
  annotate(geom = "text", x = 15, y = 6, hjust = 0, vjust = 0, label = "Best-guess trend line", colour = "blue") +
  annotate(geom = "text", x = 15, y = 8, hjust = 0, vjust = 0, label = "Possible range of\ntrue trend line", colour = "grey40") +
  #  geom_abline(intercept = 0, slope = 1, colour = "grey") +
  #  annotate(geom = "text", x = 12, y = 11, label = "Relationship if\nit was about the violence", colour = "grey", vjust = 0, hjust = 0) +
  ylab("Police killings per million population") +
  xlab("Violent crimes per thousand residents") +
  ggtitle("It's not the violence") +
  theme_minimal()
ggsave(filename = "figs/byviolentcrime.png", type = "cairo-png", width = 6, height = 3.5, units = "in")

# Compare rate of killings to percent black population
ggplot(pv_df, aes(x = Percent.population.black, y = Rate.of.Police.Killings.per.Million.Population)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "blue") +
  ylab("Police killings per million population") +
  xlab("Percent of population that is black") +
  scale_x_continuous(labels = percent) +
  ggtitle("It's not the racial make-up of the community") +
  theme_minimal()
ggsave(filename = "figs/byrace.png", type = "cairo-png", width = 6, height = 3.5, units = "in")

pv_black_lm <- lm(data = pv_df, formula = Percent.victims.black ~ Percent.population.black)
# Compare percent of victims black to percent black population
ggplot(pv_df, aes(x = Percent.population.black, y = Percent.victims.black)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, colour = "grey") +
  ylab("Percent of victims who are black") +
  xlab("Percent of population that is black") +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  annotate(geom = "text", x = 0.75, y = 0.70, label = "Expected\nrelationship", colour = "grey", hjust = 0, vjust = 1) +
  annotate(geom = "text", x = (0.75 - coefficients(pv_black_lm)[1])/coefficients(pv_black_lm)[2], y = 0.7, label = "Actual relationship", colour = "blue", hjust = 0, vjust = 0) +
  ggtitle("Black victims rise faster with community racial makeup") +
  theme_bw()
ggsave(filename = "figs/black_victims.png", type = "cairo-png", width = 6, height = 3.5, units = "in")

# Sort police department by rate of killings
levels_order <- pv_df[order(pv_df$Rate.of.Police.Killings.per.Million.Population, decreasing = FALSE),]$Police.Department
pv_df$Police.Department <- factor(pv_df$Police.Department, levels = levels_order)
#pv_df %<>% mutate(left = min(pv_df[Police.Department == "U.S. Average",]$Rate.of.Police.Killings.per.Million.Population, pv_df$Rate.of.Police.Killings.per.Million.Population),
#                  right = max(pv_df[Police.Department == "U.S. Average",]$Rate.of.Police.Killings.per.Million.Population, pv_df$Rate.of.Police.Killings.per.Million.Population))

# Plot departments by rate of killings
ggplot(pv_df, aes(x = Police.Department, y = Rate.of.Police.Killings.per.Million.Population)) +
  geom_point(aes(colour = Rate.of.Police.Killings.per.Million.Population)) +
  scale_color_gradient(low = muted("green"), high = ("red"), guide = FALSE) +
  geom_point(data = pv_df %>% filter(Police.Department == "U.S. Average"), colour = "grey") +
  #geom_point(data = pv_df %>% filter(Police.Department == "Cities Total"), colour = "blue") +
  geom_hline(yintercept = pv_totals_df$Rate.of.Police.Killings.per.Million.Population[which(pv_totals_df$Police.Department == "U.S. Average")], colour = "grey") +
  annotate(geom = "text", x = "Bakersfield", y = pv_totals_df$Rate.of.Police.Killings.per.Million.Population[which(pv_totals_df$Police.Department == "U.S. Average")] + 0.25, label = paste("U.S. Average =", format(x = pv_totals_df$Rate.of.Police.Killings.per.Million.Population[pv_totals_df$Police.Department == "U.S. Average"], digits = 2)), vjust = 0.5, hjust = 0, size = 3, colour = "grey25", angle = 270) +
  ylab("Police killings per million population") +
  coord_flip() +
  theme_bw() +
  ggtitle("Killings by America's Largest\nCity Police Departments in 2015") +
  theme(axis.title.y = element_blank())
ggsave(filename = "figs/bycity.png", type = "cairo-png", width = 5, height = 10, units = "in")
