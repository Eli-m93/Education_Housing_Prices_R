## Preliminaries
rm(list = ls())

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("plyr",
"dplyr", "readr",
"readxl", "reshape2",
"stringi", "datasets",
"ggplot2", "choroplethr",
"choroplethrMaps")
invisible(lapply(packages, pkgTest))

## These lines set several options
options(scipen = 999)  # Do not print scientific notation
options(stringsAsFactors = FALSE)  ## Do not load strings as factors

#load in zillow data
zillow <- read_csv("http://files.zillowstatic.com/research/public/County/County_Zhvi_AllHomes.csv",
                   col_names = TRUE)
zillow <- melt(zillow, id = c("RegionID", "RegionName", "SizeRank"))
zillow <- zillow %>%
  mutate(date = substr(variable, 1, 4)) %>%
  mutate(date = as.numeric(date)) %>%
  filter(!is.na(date))

zillow <- zillow %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  group_by(RegionID, date) %>%
  summarise(val = mean(value))


#load in metrodata
zillow_county <- read_csv("http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv",
                          col_names = TRUE)
#merge
zillow <- merge(zillow, zillow_county,
by.x = "RegionID", by.y = "CountyRegionID_Zillow")
zillow <- zillow[!duplicated(zillow), ]
zillow <- zillow[!duplicated(zillow), ]

write.csv(zillow, "zillow_housing_prices.csv")

