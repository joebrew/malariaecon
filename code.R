library(tidyverse)
library(tiff)
library(rgdal)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(sp)
library(maptools)
library(databrew)
library(rgeos)
library(plm)


if('prepared_data.RData' %in% dir()){
  load('prepared_data.RData')
} else {
  # Read in shapefile of africa
  africa1 <- africa <- readOGR('data/africa_level_1', 'africa1')
  africa_fortified <- broom::tidy(africa1, id = 'NAME_0')
  
  # Read in malaria atlas project data
  mosq00 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2000.tif')
  mosq01 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2001.tif')
  mosq02 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2002.tif')
  mosq03 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2003.tif')
  mosq04 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2004.tif')
  mosq05 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2005.tif')
  mosq06 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2006.tif')
  mosq07 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2007.tif')
  mosq08 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2008.tif')
  mosq09 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2009.tif')
  mosq10 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2010.tif')
  mosq11 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2011.tif')
  mosq12 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2012.tif')
  mosq13 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2013.tif')
  mosq14 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2014.tif')
  mosq15 <- raster('data/malaria_atlas_project/2015_Nature_Africa_PR.2015.tif')
  
  u = brick(mosq00,
            mosq01,
            mosq02,
            mosq03,
            mosq04,
            mosq05,
            mosq06,
            mosq07,
            mosq08,
            mosq09,
            mosq10,
            mosq11,
            mosq12,
            mosq13,
            mosq14,
            mosq15)
  names(u) <- paste0('year_', as.character(2000:2015))
  u <- as(u, 'SpatialPixelsDataFrame')
  # projection(mosq) <- "+proj=utm +zone=48 +datum=WGS84"
  
  africa_projected <- africa
  projection(africa_projected) <- "+proj=utm +zone=48 +datum=WGS84"
  areas <- rgeos::gArea(africa_projected, byid = TRUE)
  # a <- extract(cowsquito, africa, fun = mean, na.rm = TRUE)
  years <- 2000:2015
  results <- data.frame(country = africa@data$NAME_0,
                        area = areas,
                        year = NA,
                        avg = NA,
                        a_max = NA,
                        a_median = NA,
                        a_p = NA)
  results_list <- list()
  for(i in 1:length(years)){
    this_year <- years[i]
    message(this_year)
    this_year_digits <- substr(this_year, 3, 4)
    this_raster <- get(paste0('mosq', this_year_digits))
    values(this_raster)[!is.finite(values(this_raster))] <- 0
    b <- extract(this_raster, africa)
    a_mean <- unlist(lapply(b, FUN=mean, na.rm = T))
    a_max <- unlist(lapply(b, FUN=max, na.rm = T))
    a_median <- unlist(lapply(b, function(x){y <- median(x, na.rm = TRUE); ifelse(is.null(y), 0, y)}))
    a_p <- unlist(lapply(b, FUN=function(x){length(which(x > median(values(this_raster), na.rm = TRUE))) / length(x) * 100}))
    results_this_year <- results
    results_this_year$year <- this_year
    results$avg<- a_mean
    results_this_year$a_max <- a_max
    results_this_year$a_median <- a_median
    results_this_year$a_p <- a_p
    results_list[[i]] <- results_this_year
  }
  results_bound <- bind_rows(results_list)
  weighted_mean <- function(x, w, na.rm, ...){
    finites <- which(is.finite(x))
    x <- x[finites]
    w <- w[finites]
    weighted.mean(x, w, na.rm)
  }
  results <- results_bound %>%
    group_by(country, year) %>%
    summarise(avg = weighted_mean(avg, w = area, na.rm = TRUE),
              a_max = weighted_mean(a_max, w = a_max, na.rm = TRUE),
              a_median = weighted_mean(a_median, w = a_median, na.rm = TRUE),
              a_p = weighted_mean(a_p, w = a_p, na.rm = TRUE)) %>%
    ungroup
  
  africa_fortified <- fortify(africa, region = 'OBJECTID')
  
  
  save.image('prepared_data.RData')
}

# Clean up
results <- results %>% 
  filter(!is.na(avg))

# Get iso3
library(countrycode)

cc <- countrycode(results$country, origin = 'country.name',destination = 'iso3c')
results$iso3 <- cc

# # Read in Acemoglu data
library(readxl)
ace <- read_excel('data/Acemoglu/Income and Democracy Data AER adjustment.xls',
                  sheet = '500 Year Panel')
# Clean up variable names and keep only variables of interest
ace <- ace %>%
  dplyr::rename(independence_year = indyear,
                # augmented_freedom_house_political_rights_index = fhpolrigaug,
                # log_real_gdp_per_capita = lrgdpch,
                # polity_iv_index = polity4,
                former_colony = colony,
                percent_catholic = rel_catho80,
                percent_muslim = rel_muslim80,
                percent_protestant = rel_protmg80,
                log_settler_mortality = logem4,
                log_pop_density_1500 = lpd1500s,
                iso3 = code) 

# Read in world bank data
wb <- read_csv('data/world_bank/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_9944664/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_9944664.csv', skip = 4)
wb <- wb %>%
  dplyr::rename(country = `Country Name`,
                iso3 = `Country Code`,
                indicator = `Indicator Name`) %>%
  gather(year, gdp_per_capita_current_usd, `1960`:`2017`) %>%
  dplyr::select(-`Indicator Code`, -`X63`, -indicator)

# Join the Acemoglu data (which is "timeless") to the wb panel data
panel <- left_join(wb, ace %>%
                 dplyr::select(-country),
               by = 'iso3') %>%
  mutate(year = as.numeric(year),
         gdp_per_capita_current_usd = as.numeric(gdp_per_capita_current_usd)) %>%
  mutate(former_colony = ifelse(former_colony == 1, TRUE, FALSE))

# Read in poverty data
poverty <- read_csv('data/world_bank/API_SI.POV.DDAY_DS2_en_csv_v2_9945318.csv', skip = 4)
poverty <- poverty %>%
  dplyr::rename(iso3 = `Country Code`) %>%
  dplyr::select(iso3, `1960`:`2017`) %>%
  tidyr::gather(key = year, value = 'poverty_headcount', `1960`:`2017`) %>%
  mutate(year = as.numeric(as.character(year)))

# Read in population data
population <- read_csv('data/world_bank/API_SP.POP.TOTL_DS2_en_csv_v2_9944650.csv', skip = 4) %>%
  dplyr::rename(iso3 = `Country Code`) %>%
  dplyr::select(iso3, `1960`:`2017`) %>%
  tidyr::gather(key = year, value = 'population', `1960`:`2017`) %>%
  filter(!is.na(population)) %>%
  mutate(year = as.numeric(as.character(year)))

# Read in tourism data
tourism <- read_csv('data/world_bank/API_ST.INT.XPND.MP.ZS_DS2_en_csv_v2_9986587.csv', skip = 4) %>%
  dplyr::rename(iso3 = `Country Code`) %>%
  dplyr::select(iso3, `1960`:`2017`) %>%
  tidyr::gather(key = year, value = 'tourism', `1960`:`2017`) %>%
  filter(!is.na(tourism)) %>%
  mutate(year = as.numeric(as.character(year)))

# Read in gdp per cap
gdp_per_cap <- read_csv('data/world_bank/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_9944664/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_9944664.csv', skip = 4) %>%
  dplyr::rename(iso3 = `Country Code`) %>%
  dplyr::select(iso3, `1960`:`2017`) %>%
  tidyr::gather(key = year, value = 'gdp_per_capita', `1960`:`2017`) %>%
  filter(!is.na(gdp_per_capita)) %>%
  mutate(year = as.numeric(as.character(year)))

# Read in gini
gini <- read_csv('data/world_bank/API_SI.POV.GINI_DS2_en_csv_v2_9944669.csv', skip = 4) %>%
  dplyr::rename(iso3 = `Country Code`) %>%
  dplyr::select(iso3, `1960`:`2017`) %>%
  tidyr::gather(key = year, value = 'gini_index', `1960`:`2017`) %>%
  filter(!is.na(gini_index))%>%
  mutate(year = as.numeric(as.character(year)))

# Read in % of wealth belonging to bottom 20%
wealth20 <- read_csv('data/world_bank/API_SI.DST.FRST.20_DS2_en_csv_v2_9948163.csv',
                     skip = 4) %>%
  dplyr::rename(iso3 = `Country Code`) %>%
  dplyr::select(iso3, `1960`:`2017`) %>%
  tidyr::gather(key = year, value = 'wealth_20', `1960`:`2017`) %>%
  filter(!is.na(wealth_20)) %>%
  mutate(year = as.numeric(as.character(year)))

fdi <- read_csv('data/world_bank/API_BX.KLT.DINV.CD.WD_DS2_en_csv_v2_9984841.csv',
                     skip = 4) %>%
  dplyr::rename(iso3 = `Country Code`) %>%
  dplyr::select(iso3, `1960`:`2017`) %>%
  tidyr::gather(key = year, value = 'fdi', `1960`:`2017`) %>%
  filter(!is.na(fdi)) %>%
  mutate(year = as.numeric(as.character(year)))

inflation <- read_csv('data/world_bank/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_9984803.csv',
                      skip = 4) %>%
  dplyr::rename(iso3 = `Country Code`) %>%
  dplyr::select(iso3, `1960`:`2017`) %>%
  tidyr::gather(key = year, value = 'inflation', `1960`:`2017`) %>%
  filter(!is.na(inflation)) %>%
  mutate(year = as.numeric(as.character(year)))

# Join the above to the panel
panel <- 
  panel %>%
  left_join(wealth20) %>%
  left_join(gini) %>%
  left_join(population) %>%
  left_join(gdp_per_cap) %>%
  left_join(poverty) %>%
  left_join(tourism) %>%
  left_join(fdi) %>%
  left_join(inflation)

# Get with gaminder data
library(gapminder)
gm <- gapminder::gapminder %>%
  left_join(gapminder::country_codes,
            by = 'country') %>%
  dplyr::rename(iso3 = iso_alpha) %>%
  dplyr::select(-country)
panel <- left_join(panel, gm,
                   by = c('iso3', 'year'))

# Years since independence
panel$years_since_independence <- 
  panel$year - panel$independence_year

# Bring in malaria
panel <- panel %>%
  left_join(results %>%
              dplyr::select(iso3,
                            year, avg) %>%
              dplyr::rename(malaria_prevalence = avg),
            by = c('iso3', 'year'))

# Make an easier named gdp variable
panel$gdp <- panel$gdp_per_capita_current_usd

# Read in who data
who <- read_excel('data/who/AllData.xlsx')
# Clean up
who <- who %>%
  dplyr::rename(year = Year,
                country = `Country Name`,
                iso3 = `Country Code`,
                income_group_who = `Country Income Group`,
                indicator_name = `Indicator Name`,
                value = Value) %>%
  dplyr::select(year, country, iso3, income_group_who,
                indicator_name,
                value)
indicators <- sort(unique(who$indicator_name))
# Keep only indicators of relevance
indicators <- c('Capital health expenditure',
                'Current Health Expenditure (CHE) as % Gross Domestic Product (GDP)',
                'Domestic General Government Health Expenditure (GGHE-D) as % Gross Domestic Product (GDP)',
                'External Health Expenditure (EXT) per Capita in US$',
                'External sources of spending on Malaria',
                'External sources of spending on Injuries',
                'Public domestic sources of spending on Malaria')
for(i in 1:length(indicators)){
  this_indicator <- indicators[i]
  sub_data <- who %>% filter(indicator_name == this_indicator)
  sub_data <- sub_data %>% 
    dplyr::select(year, iso3, value)
  names(sub_data)[3] <- gsub('$', 'dollars', gsub('%', 'percent', gsub(' ', '_', tolower(this_indicator)), fixed = TRUE), fixed = TRUE)
  sub_data <- sub_data %>% dplyr::distinct(year, iso3, .keep_all = TRUE)
  panel <- left_join(panel, sub_data)
}
#

# Define whether 2000 level of malaria was high
panel <- panel %>%
  group_by(country) %>%
  mutate(malaria_prevalence_2001 = malaria_prevalence[year== 2001]) %>%
  ungroup %>%
  mutate(malaria_prevalence_2001_high = malaria_prevalence_2001 >= 0.1)

# Get reduction in malaria and increase in gdp
panel <- panel %>%
  arrange(year) %>%
  group_by(country) %>%
  mutate(malaria_reduction = 
           dplyr::lag(malaria_prevalence, n = 1) - 
           malaria_prevalence,
         malaria_reduction_relative = 1 - 
           (malaria_prevalence / 
           dplyr::lag(malaria_prevalence, n = 1)),
         malaria_reduction_relative_lag = 1 - 
           (dplyr::lag(malaria_prevalence, 1) / 
              dplyr::lag(malaria_prevalence, n = 2)),
         gdp_growth = 
           gdp - 
           dplyr::lag(gdp, n = 1),
         gdp_growth_relative = 
           (gdp / 
           dplyr::lag(gdp, n = 1) - 1),
         gdp_growth_relative_lag = 
           (dplyr::lag(gdp, n = 1) / 
              dplyr::lag(gdp, n = 2) - 1)) %>%
  ungroup

# Read in more MAP data
if('map_prepared.RData' %in% dir()){
  load('map_prepared.RData')
} else {
  # Read in malaria atlas project data
  mosq00 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2000.tif')
  mosq01 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2001.tif')
  mosq02 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2002.tif')
  mosq03 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2003.tif')
  mosq04 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2004.tif')
  mosq05 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2005.tif')
  mosq06 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2006.tif')
  mosq07 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2007.tif')
  mosq08 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2008.tif')
  mosq09 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2009.tif')
  mosq10 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2010.tif')
  mosq11 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2011.tif')
  mosq12 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2012.tif')
  mosq13 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2013.tif')
  mosq14 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2014.tif')
  mosq15 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ACT.2015.tif')
  
  u = brick(mosq00,
            mosq01,
            mosq02,
            mosq03,
            mosq04,
            mosq05,
            mosq06,
            mosq07,
            mosq08,
            mosq09,
            mosq10,
            mosq11,
            mosq12,
            mosq13,
            mosq14,
            mosq15)
  names(u) <- paste0('year_', as.character(2000:2015))
  u <- as(u, 'SpatialPixelsDataFrame')
  # projection(mosq) <- "+proj=utm +zone=48 +datum=WGS84"
  
  africa_projected <- africa
  projection(africa_projected) <- "+proj=utm +zone=48 +datum=WGS84"
  areas <- rgeos::gArea(africa_projected, byid = TRUE)
  # a <- extract(cowsquito, africa, fun = mean, na.rm = TRUE)
  years <- 2000:2015
  results <- data.frame(country = africa@data$NAME_0,
                        area = areas,
                        year = NA,
                        avg = NA,
                        a_max = NA,
                        a_median = NA,
                        a_p = NA)
  results_list <- list()
  for(i in 1:length(years)){
    this_year <- years[i]
    message(this_year)
    this_year_digits <- substr(this_year, 3, 4)
    this_raster <- get(paste0('mosq', this_year_digits))
    values(this_raster)[!is.finite(values(this_raster))] <- 0
    b <- extract(this_raster, africa)
    a_mean <- unlist(lapply(b, FUN=mean, na.rm = T))
    a_max <- unlist(lapply(b, FUN=max, na.rm = T))
    a_median <- unlist(lapply(b, function(x){y <- median(x, na.rm = TRUE); ifelse(is.null(y), 0, y)}))
    a_p <- unlist(lapply(b, FUN=function(x){length(which(x > median(values(this_raster), na.rm = TRUE))) / length(x) * 100}))
    results_this_year <- results
    results_this_year$year <- this_year
    results$avg<- a_mean
    results_this_year$a_max <- a_max
    results_this_year$a_median <- a_median
    results_this_year$a_p <- a_p
    results_list[[i]] <- results_this_year
  }
  results_bound <- bind_rows(results_list)
  weighted_mean <- function(x, w, na.rm, ...){
    finites <- which(is.finite(x))
    x <- x[finites]
    w <- w[finites]
    weighted.mean(x, w, na.rm)
  }
  results <- results_bound %>%
    group_by(country, year) %>%
    summarise(avg = weighted_mean(avg, w = area, na.rm = TRUE),
              a_max = weighted_mean(a_max, w = a_max, na.rm = TRUE),
              a_median = weighted_mean(a_median, w = a_median, na.rm = TRUE),
              a_p = weighted_mean(a_p, w = a_p, na.rm = TRUE)) %>%
    ungroup
  save(results,
       file = 'map_prepared.RData')
}
act <- results

if('itn_prepared.RData' %in% dir()){
  load('itn_prepared.RData')
} else {
  # Read in malaria atlas project data
  mosq00 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2000.tif')
  mosq01 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2001.tif')
  mosq02 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2002.tif')
  mosq03 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2003.tif')
  mosq04 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2004.tif')
  mosq05 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2005.tif')
  mosq06 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2006.tif')
  mosq07 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2007.tif')
  mosq08 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2008.tif')
  mosq09 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2009.tif')
  mosq10 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2010.tif')
  mosq11 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2011.tif')
  mosq12 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2012.tif')
  mosq13 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2013.tif')
  mosq14 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2014.tif')
  mosq15 <- raster('data/malaria_atlas_project/2015_Nature_Africa_ITN.2015.tif')
  
  u = brick(mosq00,
            mosq01,
            mosq02,
            mosq03,
            mosq04,
            mosq05,
            mosq06,
            mosq07,
            mosq08,
            mosq09,
            mosq10,
            mosq11,
            mosq12,
            mosq13,
            mosq14,
            mosq15)
  names(u) <- paste0('year_', as.character(2000:2015))
  u <- as(u, 'SpatialPixelsDataFrame')
  # projection(mosq) <- "+proj=utm +zone=48 +datum=WGS84"
  
  africa_projected <- africa
  projection(africa_projected) <- "+proj=utm +zone=48 +datum=WGS84"
  areas <- rgeos::gArea(africa_projected, byid = TRUE)
  # a <- extract(cowsquito, africa, fun = mean, na.rm = TRUE)
  years <- 2000:2015
  results_itn <- data.frame(country = africa@data$NAME_0,
                        area = areas,
                        year = NA,
                        avg = NA,
                        a_max = NA,
                        a_median = NA,
                        a_p = NA)
  results_itn_list <- list()
  for(i in 1:length(years)){
    this_year <- years[i]
    message(this_year)
    this_year_digits <- substr(this_year, 3, 4)
    this_raster <- get(paste0('mosq', this_year_digits))
    values(this_raster)[!is.finite(values(this_raster))] <- 0
    b <- extract(this_raster, africa)
    a_mean <- unlist(lapply(b, FUN=mean, na.rm = T))
    a_max <- unlist(lapply(b, FUN=max, na.rm = T))
    a_median <- unlist(lapply(b, function(x){y <- median(x, na.rm = TRUE); ifelse(is.null(y), 0, y)}))
    a_p <- unlist(lapply(b, FUN=function(x){length(which(x > median(values(this_raster), na.rm = TRUE))) / length(x) * 100}))
    results_itn_this_year <- results_itn
    results_itn_this_year$year <- this_year
    results_itn$avg<- a_mean
    results_itn_this_year$a_max <- a_max
    results_itn_this_year$a_median <- a_median
    results_itn_this_year$a_p <- a_p
    results_itn_list[[i]] <- results_itn_this_year
  }
  results_itn_bound <- bind_rows(results_itn_list)
  weighted_mean <- function(x, w, na.rm, ...){
    finites <- which(is.finite(x))
    x <- x[finites]
    w <- w[finites]
    weighted.mean(x, w, na.rm)
  }
  itn <- results_itn_bound %>%
    group_by(country, year) %>%
    summarise(avg = weighted_mean(avg, w = area, na.rm = TRUE),
              a_max = weighted_mean(a_max, w = a_max, na.rm = TRUE),
              a_median = weighted_mean(a_median, w = a_median, na.rm = TRUE),
              a_p = weighted_mean(a_p, w = a_p, na.rm = TRUE)) %>%
    ungroup
  save(itn,
       file = 'itn_prepared.RData')
}

# Reformat act and itn data to get ready to join
act <- act %>% dplyr::select(country, year, avg) %>%
  mutate(avg = ifelse(is.na(avg), 0, avg)) %>%
  dplyr::rename(act = avg)
itn <- itn %>% dplyr::select(country, year, avg) %>%
  mutate(avg = ifelse(is.na(avg), 0, avg)) %>%
  dplyr::rename(itn = avg)
itn <- itn %>% dplyr::distinct(country, year, .keep_all = TRUE)
act <- act %>% dplyr::distinct(country, year, .keep_all = TRUE)

itn <- itn %>% mutate(iso3 = countrycode(country, origin = 'country.name',destination = 'iso3c')) %>% dplyr::select(-country)
act <- act %>% mutate(iso3 = countrycode(country, origin = 'country.name',destination = 'iso3c')) %>% dplyr::select(-country)

panel <- left_join(panel, itn)
panel <- left_join(panel, act)

# Get weather data
africa <- cism::africa
africa@data$iso3 <- countrycode(africa@data$COUNTRY, origin = 'country.name',destination = 'iso3c')
library(gsod)
if('weather.RData' %in% dir()){
  load('weather.RData')
} else {
  weather_list <- list()
  years <- 2001:2015
  for (i in 1:length(years)){
    message(i)
    xsp <- x <- get(paste0('gsod', years[i])) %>%
      dplyr::filter(!is.na(lon), !is.na(lat)) 
    coordinates(xsp) <- ~lon+lat
    proj4string(xsp) <- proj4string(africa)
    polys <- over(xsp, polygons(africa))
    the_iso3 <- africa@data$iso3[polys]
    x$iso3 <- the_iso3
    x <- x %>% filter(!is.na(iso3))
    out <- x %>%
      dplyr::group_by(iso3) %>%
      dplyr::summarise(temp = mean(temp, na.rm = TRUE),
                prcp = mean(prcp, na.rm = TRUE),
                max = max(max, na.rm = TRUE),
                min = min(min, na.rm = TRUE),
                stations = length(unique(stnid))) %>%
      ungroup %>%
      # mutate(prcp = prcp / stations) %>%
      dplyr::mutate(year = years[i])
    weather_list[[i]] <- out
  }
  weather <- bind_rows(weather_list)
  save(weather, file = 'weather.RData')
}

# Read in oil exports data
oil <- read_csv('data/undata/UNdata_Export_20180702_093858550.csv')
oil <- oil %>%
  dplyr::rename(country = `Country or Area`,
                year = Year,
                oil = Quantity) %>%
  dplyr::select(country, year, oil)
oil$iso3 <- countrycode(oil$country, origin = 'country.name',destination = 'iso3c',
                        nomatch = NULL)
oil <- oil %>%
  filter(!is.na(country),
         !is.na(year),
         !is.na(iso3)) %>%
  mutate(oil = ifelse(is.na(oil), 0, oil))
oil$country <- NULL
panel <-
  left_join(panel, oil)


# Keep only those countries in Africa
africa_panel <- panel %>%
  group_by(country) %>%
  filter(!all(is.na(malaria_prevalence))) %>%
  ungroup

# Join weather
africa_panel <- africa_panel %>% 
  left_join(weather)


# # Plot of gdp by year
# plot_data <- africa_panel %>% filter(year >= 2012) %>%
#   arrange(year) %>%
#   group_by(iso3) %>%
#   mutate(down = gdp[year==2015] < gdp[year == 2013]) %>%
#   ungroup %>%
#   filter(!is.na(down)) %>%
#   mutate(down = ifelse(down, 'GDP decrease', 'GDP increase'))
# label_df <- plot_data %>%
#   filter(year == 2016)
# ggplot(data = plot_data,
#        aes(x = year,
#            y = gdp,
#            group = iso3,
#            color = iso3)) + 
#   geom_path() + 
#   geom_point() +
#   scale_y_log10(breaks = c(100, 500, 1000, 2000, 5000, 10000)) +
#   facet_wrap(~down) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   geom_label(data = label_df,
#              aes(x = year,
#                  y = gdp,
#                  color = iso3,
#                  label = country),
#              alpha = 0.6,
#              size = 2) +
#   theme(legend.position = 'none') +
#   labs(x = 'Year',
#        y = 'GDP per capita')

# Remove duplicates
africa_panel <- africa_panel %>%
  dplyr::distinct(year, iso3, .keep_all = TRUE)


model_data <- africa_panel %>%
  filter(year >= 2000) %>%
  dplyr::filter(malaria_prevalence_2001_high) %>%
  dplyr::filter(!is.na(gdp),
                !is.na(malaria_prevalence)) %>%
  # dplyr::select(country, year, gdp,
  #               malaria_prevalence) %>%
  # Balance the panel
  dplyr::group_by(country) %>%
  dplyr::filter(!any(gdp > 10000)) %>%
  dplyr::mutate(n = n()) %>%
  ungroup %>%
  # Make balanced
  dplyr::filter(n == median(n)) %>%
  dplyr::select(-n) %>%
  # Calculate growth
  dplyr::group_by(country) %>%
  dplyr::mutate(growth = gdp -
                  dplyr::lag(gdp, 1),
                growth_relative = 1 - (gdp / 
                                         dplyr::lag(gdp, 1)))
model_data <- model_data %>% arrange(year) %>% ungroup

model_data <- model_data %>%
  group_by(country) %>%
  mutate(gdp_lag1 = dplyr::lag(gdp, 1),
         malaria_prevalence_lag1 = dplyr::lag(malaria_prevalence),
         gdp_lead1 = dplyr::lead(gdp, 1),
         malaria_prevalence_lead1 = dplyr::lead(malaria_prevalence, 1)) %>%
  ungroup 

model_data <- model_data %>%
  arrange(country,year)


# Distribution of gdp in post growth year
model_data <- model_data %>%
  arrange(year) %>%
  group_by(country) %>%
  mutate(post_gdp_growth = dplyr::lag(gdp_growth_relative, 1) > 0,
         post_malaria_reduction = dplyr::lag(malaria_reduction_relative, 1) > 0,
         post_gdp_growth_2 = dplyr::lag(gdp_growth_relative, 2) > 0,
         post_malaria_reduction_2 = dplyr::lag(malaria_reduction_relative, 2) > 0) %>%
  mutate(post_gdp_growth_consec = post_gdp_growth & post_gdp_growth_2,
         post_malaria_reduction_consec = post_malaria_reduction & post_malaria_reduction_2) %>%
  ungroup

# Get log change
model_data <- model_data %>%
  mutate(log_change_gdp = log(gdp) - log(gdp_lag1),
         log_change_malaria = log(malaria_prevalence) - log(malaria_prevalence_lag1)) %>%
  group_by(country) %>%
  mutate(log_change_gdp_lag = log(lag(gdp, 1)) - log(lag(gdp,2)),
         log_change_malaria_lag = log(lag(malaria_prevalence, 1)) - log(lag(malaria_prevalence, 2))) %>%
  ungroup

# Get malaria spending variables
model_data <- model_data %>%
  mutate(malaria_spending = 
           external_sources_of_spending_on_malaria + 
           public_domestic_sources_of_spending_on_malaria,
         health_spending = `current_health_expenditure_(che)_as_percent_gross_domestic_product_(gdp)`) 
# Interpolate some variables
model_data <- model_data %>%
  arrange(year) %>%
  dplyr::distinct(iso3, year, .keep_all = TRUE) %>%
  group_by(iso3) %>%
  mutate(wealth_20 = zoo::na.approx(object = as.numeric(wealth_20), x = year, na.rm = FALSE),
         gini_index = zoo::na.approx(object = as.numeric(gini_index), x = year, na.rm = FALSE),
         population = zoo::na.approx(object = as.numeric(population), x = year, na.rm = FALSE),
         gdp_per_capita = zoo::na.approx(object = as.numeric(gdp_per_capita), x = year, na.rm = FALSE),
         poverty_headcount = zoo::na.approx(object = as.numeric(poverty_headcount), x = year, na.rm = FALSE))

# # Read in bruckner's commodity prices # TOO OLD
# library(readstata13)
# bruck <- readstata13::read.dta13('bruckner/data-stata.dta')
# bruck_meaning <- data.frame(var = names(bruck)[1:15], label = attr(bruck, 'var.labels'))
# 
# # Use bruckner commodity
# bruck$iso3 <- countrycode(bruck$country, origin = 'country.name',destination = 'iso3c')
# bruck <- data.frame(bruck)
# bruck$commodity <- bruck$index_g_l
# bruck <- bruck %>% dplyr::select(iso3, year, commodity)
# 
# # Join bruckner to model data
# model_data <- left_join(model_data, bruck)

# Get malaria spending and health spending lag
model_data <- model_data %>%
  arrange(year) %>%
  group_by(iso3) %>%
  mutate(malaria_spending_lag = dplyr::lag(malaria_spending, 1),
         health_spending_lag = dplyr::lag(health_spending, 1)) %>%
  ungroup

# Remove duplicates
model_data <- model_data %>%
  dplyr::distinct(year, iso3, .keep_all = TRUE)

# Diff-in-diff
# gdp = price of oil (country specific) + oil-non-oil + before/after crisis + interaction
# malaria = gdp + other things + unexplained variance from above
# Find commodity price index


# IV with fixed effects
plm_data <- plm::pdata.frame(x = model_data, index = c('iso3', 'year'))
plm_data <- plm_data %>% filter(!is.na(malaria_reduction_relative_lag))
fit_plm <-  plm(gdp_growth_relative ~ malaria_reduction_relative_lag, data=plm_data, index = c('iso3', 'year'), model="within")
fit_plm_iv <- plm(gdp_growth_relative ~ malaria_reduction_relative_lag | act + itn, data= plm_data, index = c('iso3', 'year'), model="within")

summary(fit_plm_iv)

# Causality using VAR 
# like Blanchard and Perotti (2002) and Bruckner (2011)
bruckner_data <- model_data %>% filter(!is.na(log_change_malaria),
                                       !is.na(log_change_gdp_lag),
                                       # !is.na(prcp),
                                       # !is.na(malaria_spending),
                                       # !is.na(health_spending),
                                       # !is.na(health_spending),
                                       !is.na(commodity),
                                       !is.na(log_change_gdp),
                                       !is.na(log_change_malaria_lag))
bruckner1 <- lm(malaria_reduction_relative ~ gdp_growth_relative_lag + commodity, data = bruckner_data)
summary(bruckner1)
bruckner2 <- lm(gdp_growth_relative ~ malaria_reduction_relative_lag + malaria_spending + residuals(bruckner1), data = bruckner_data) 
summary(bruckner2)

# Ideas
# - Use FDI
# - Use Fx
# - Use commodity price index
# - Switch the equation, and find something that affects malaria but not GDP (such as ACT or ITN coverage at national level from MAP, already downloaded)

library(lfe)
bruck1 <- felm(malaria_reduction_relative ~ gdp_growth_relative_lag + malaria_spending_lag  | iso3 | 0 | 0,
               data = bruckner_data)
summary(bruck1)
bruck2 <- felm(gdp_growth_relative ~ malaria_reduction_relative_lag + malaria_spending_lag + residuals(bruck1) | iso3 | 0 | 0, data = bruckner_data)
summary(bruck2)


# Make some table of results here
library(knitr)
library(kableExtra)
table2 <- kable(broom::tidy(summary(bruckner2)),
                'latex',
                linesep = "",
                # longtable = T,
                booktabs = T,
                caption = 'Placeholder table for Bruckner results') %>%
  kable_styling() %>%
  add_footnote(label = 'Some comments on the table.', notation = 'symbol')
cat(table2, file = 'tables/table2.tex')


library(MASS)
x <- vars::VAR(y = model_data %>%
                 dplyr::filter(!is.na(log_change_gdp),
                               !is.na(log_change_malaria)) %>%
                 dplyr::select(log_change_gdp,
                               log_change_malaria),
               p = 1,
               type = 'both')
out <- vars::causality(x = x,
                       cause = 'log_change_malaria')
# cause = 'malaria_prevalence')
out

library(MASS)
x <- vars::VAR(y = model_data %>%
                 dplyr::filter(!is.na(gdp),
                               !is.na(malaria_prevalence)) %>%
                 dplyr::select(gdp,
                               malaria_prevalence),
               p = 1,
               type = 'both')
out <- vars::causality(x = x,
                       cause = 'gdp')
# cause = 'malaria_prevalence')
out


library(cowplot)

# # Lead and lag for malaria / gdp
# ggplot(data = model_data,
#        aes(x = gdp_growth_relative,
#            y = malaria_reduction_relative)) +
#   geom_point() 

# GDP and malaria prevalence over time
x <- model_data %>%
  group_by(year) %>%
  summarise(gdp = mean(gdp, na.rm = TRUE),
            malaria = mean(malaria_prevalence, na.rm = TRUE)) %>%
  ungroup %>%
  # mutate(gdp = gdp / dplyr::first(gdp),
  #        malaria = malaria / dplyr::first(malaria)) 
  gather(key, value, gdp:malaria)
g1 <- ggplot() +
  geom_line(data = model_data %>%
              dplyr::select(year, country, gdp,
                            malaria_prevalence) %>%
              gather(key, value, gdp: malaria_prevalence) %>%
              mutate(key = ifelse(key == 'gdp',
                                  'GDP per Capita',
                                  'Malaria prevalence')),
            aes(x = year,
                y = value,
                group = country,
                color = country),
            alpha = 0.6) +
  # scale_y_log10() +
  facet_wrap(~key, scales = 'free_y') +
  labs(x = 'Year',
       y = '',
       title = 'A') +
  geom_line(data = x %>% dplyr::mutate(key = ifelse(key == 'gdp', 'GDP per Capita', 'Malaria prevalence')), 
            aes(x = year,
                y = value),
            color = 'black', 
            size = 2,
            alpha = 0.7) +
  scale_color_manual(name = '',
                     values = colorRampPalette(brewer.pal(n = 9, 
                                                          name = 'Spectral'))(length(unique(model_data$country)))) +
  theme(legend.position = 'none') 



# See gdp and malaria prevalence
label_df <- data.frame(x = c(0.7, 0.7, -0.7, -0.7),
                       y = c(0.7, -0.7, 0.7, -0.7),
                       label = c('GDP increase\nMalaria decrease',
                                 'GDP increase\nMalaria increase',
                                 'GDP decrease\nMalaria decrease',
                                 'GDP decrease\nMalaria increase'))
g2 <- ggplot(data = model_data,
       aes(x = gdp_growth_relative,
           y = malaria_reduction_relative)) +
  geom_point(size = 1, alpha = 0.8,
             aes(color = country)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = 'GDP growth',
       y = 'Malaria prevalence reduction',
       title = 'B') +
  xlim(-1,1) +
  ylim(-1,1) + scale_color_manual(name = '',
                                values = colorRampPalette(brewer.pal(n = 9, 
                                                                     name = 'Spectral'))(length(unique(model_data$country)))) +
  theme(legend.position = 'none') +
  geom_text(data = label_df,
             aes(x = x,
                 y = y,
                 label = label),
            alpha = 0.6)


library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(dplyr)

avg <- model_data %>%
  group_by(year) %>%
  summarise(gdp = mean(gdp),
            malaria_prevalence = mean(malaria_prevalence)) %>%
  ungroup
avg <- avg %>%
  dplyr::filter(year == dplyr::first(year) |
                  year == dplyr::last(year))

g3 <- 
  ggplot(data = data.frame(model_data) %>%
           group_by(country) %>%
           mutate(x = dplyr::first(gdp),
                  y = dplyr::first(malaria_prevalence),
                  xend = dplyr::last(gdp),
                  yend = dplyr::last(malaria_prevalence)) %>%
             ungroup,
         aes(x = gdp,
             y = malaria_prevalence)) +
  # geom_point(alpha = 0.6,
  #            size = 0.1) +
  geom_path(alpha = 0.9,
            aes(group = country,
                color = country),
  arrow = arrow(type = "closed", ends = 'last', angle = 15, length = unit(0.2, "cm"))) +
  # ) +
  # scale_y_log10() +
  scale_x_log10() +
  # facet_wrap(~country, scales = 'free') +
  labs(x = 'GDP per Capita (log)',
       y = 'Malaria prevalence',
       title = 'C') +
  # stat_smooth(n = 50, span = 1.5) +
  # theme(legend.position = 'none') +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 8)) +
  scale_color_manual(name = '',
                     values = colorRampPalette(brewer.pal(n = 9, 
                                                          name = 'Spectral'))(length(unique(model_data$country)))) +
  # facet_wrap(~country, ncol = 3) +
  theme(strip.text = element_text(size=8),
        # strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 8)) +
  geom_path(data = avg,
            size = 1.5,
            alpha = 0.7,
            arrow = arrow(type = "closed", ends = 'last', angle = 15, length = unit(0.4, "cm")))

# Study area
africa <- cism::africa
africa@data$iso3 <- countrycode(africa@data$COUNTRY, origin = 'country.name',destination = 'iso3c')
map <- broom::tidy(africa, region = 'COUNTRY')
map <- 
  left_join(map,
            africa@data %>%
              dplyr::select(COUNTRY, iso3) %>%
              mutate(country = COUNTRY) %>%
              dplyr::rename(id = COUNTRY))
map$study <- ifelse(map$iso3 %in% model_data$iso3,
                    'In study',
                    '')
map_study <- map %>% filter(study == 'In study')
gmap <- ggplot(data = map,
       aes(x = long,
           y = lat,
           group = group)) +
  geom_polygon(fill = NA,
               color = 'black',
               size = 0.3) +
  geom_polygon(data = map_study,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = country),
               color = 'black',
               size = 0.3) +
  # geom_polygon(aes(fill = study),
  #              alpha = 0.6) +
  # scale_fill_manual(name = '',
  #                   values = c(NA, 'orange')) +
  scale_fill_manual(name = '',
                     values = colorRampPalette(brewer.pal(n = 9, 
                                                          name = 'Spectral'))(length(unique(model_data$country)))) +
  labs(title = 'D') +
  theme_map() +
  theme(legend.position = 'none',
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5,
                                  margin = margin(b = 7)))

png(filename = 'figures/descriptive.png', res = 600, width = 6000, height = 4800)
layout <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE)
Rmisc::multiplot(plotlist = list(g1, g2, g3, gmap), layout = layout)
# Rmisc::multiplot(g1, g2, cols = 2)
dev.off()

# T-Tests
# Simultaneous malaria reduction causing gdp 
t.test(x = model_data$gdp_growth_relative[model_data$malaria_reduction_relative > 0],
       y = model_data$gdp_growth_relative[model_data$malaria_reduction_relative <= 0])
# Simultaneous gdp causing malaria reduction
t.test(x = model_data$malaria_reduction_relative[model_data$gdp_growth_relative > 0],
       y = model_data$malaria_reduction_relative[model_data$gdp_growth_relative <= 0])
# Precedent malaria reduction causing gdp
t.test(x = model_data$gdp_growth_relative[model_data$post_malaria_reduction],
       y = model_data$gdp_growth_relative[!model_data$post_malaria_reduction])
# Precedent gdp grwoth causing malaria reduction
t.test(x = model_data$malaria_reduction_relative[model_data$post_gdp_growth],
       y = model_data$malaria_reduction_relative[!model_data$post_gdp_growth])

# Precedent malaria reduction 2 causing gdp
t.test(x = model_data$gdp_growth_relative[model_data$post_malaria_reduction_2],
       y = model_data$gdp_growth_relative[!model_data$post_malaria_reduction_2])
# Precedent gdp grwoth 2 causing malaria reduction
t.test(x = model_data$malaria_reduction_relative[model_data$post_gdp_growth_2],
       y = model_data$malaria_reduction_relative[!model_data$post_gdp_growth_2])

# Precedent malaria reduction consecutive causing gdp
t.test(x = model_data$gdp_growth_relative[model_data$post_malaria_reduction_consec],
       y = model_data$gdp_growth_relative[!model_data$post_malaria_reduction_consec])
# Precedent gdp grwoth consecutive causing malaria reduction
t.test(x = model_data$malaria_reduction_relative[model_data$post_gdp_growth_consec],
       y = model_data$malaria_reduction_relative[!model_data$post_gdp_growth_consec])

# Overall
cors <- data.frame(year = 2001:2015,
                   cor = NA,
                   lwr = NA,
                   upr = NA,
                   p = NA)
years <- 2001:2015
for(i in 1:length(years)){
  this_year <- years[i]
  this_data <- model_data %>% dplyr::filter(year == this_year)
  cc <- cor.test(this_data$gdp, this_data$malaria_prevalence, method = "pearson", conf.level = 0.95)
  cors$cor[i] <- cc$estimate
  cors$lwr[i] <- as.numeric(cc$conf.int[1])
  cors$upr[i] <- as.numeric(cc$conf.int[2])
  cors$p[i] <- cc$p.value
}

gcor <- ggplot(data = cors) +
  geom_line(data = cors,
    aes(x = year,
        y = cor)) +
  geom_point(data = cors,
            aes(x = year,
                y = cor)) +
  # geom_ribbon(aes(x = year,
  #                 ymin = lwr,
  #                 ymax = upr),
  #             alpha = 0.5) +
  geom_errorbar(aes(x = year,
                     ymin = lwr,
                     ymax = upr),
                 alpha = 0.5) +
  geom_hline(yintercept = 0, color = 'black', lty = 2) +
  labs(x = 'Year',
       y = 'Correlation coefficient')

png(filename = 'figures/cor.png', res = 600, width = 4000, height = 2600)
gcor
dev.off()


# Models
summary(lm(gdp_growth_relative ~ malaria_reduction_relative, data = model_data))
summary(lm(gdp_growth_relative ~ post_malaria_reduction_consec, data = model_data))
summary(glm(gdp_growth_relative > 0 ~ malaria_reduction_relative > 0, data = model_data, family = binomial('logit')))

cor.test(model_data$gdp, model_data$malaria_prevalence, method = 'pearson')
summary(lm(gdp ~ mp, data = model_data %>% mutate(mp = malaria_prevalence * 100)))


plm_data <- plm::pdata.frame(x = model_data,
                             index = c('country'))

plm_results <- list()
out <- data.frame(model = 1:2,
                  statement = c('Malaria causes GDP',
                                'GDP causes malaria'),
                  p = NA,
                  zbar = NA,
                  null = c('Malaria decrease does not cause GDP increase',
                           'GDP increase does not cause Malaria decrease'))

fit <- pgrangertest(formula = 
                      growth ~
                      # log(gdp) ~ 
                      malaria_prevalence,
                    test = 'Zbar',
                    # order = 3,
                    data = plm_data)
plm_results[[1]] <- data.frame(fit$indgranger) %>% mutate(statement = out$null[1])
out$p[1] <- fit$p.value; out$p[1]
out$zbar[1] <- fit$statistic
fit <- pgrangertest(formula = malaria_prevalence ~ 
                      growth,
                    # log(gdp),
                    # order = 3,
                    test = 'Zbar',
                    data = plm_data)
plm_results[[2]] <- data.frame(fit$indgranger) %>% mutate(statement = out$null[2])

out$p[2] <- fit$p.value; out$p[2]
out$zbar[2] <- fit$statistic
out
out$title <- paste0(out$null, ' | P=', round(out$p, digits = 3))
plm_results <- bind_rows(plm_results)
plm_results <- plm_results %>%
  dplyr::select(country, Chisq, p.value, statement)
plm_results <- left_join(plm_results,
                           out %>%
                           mutate(statement = null) %>%
                           dplyr::select(statement, title)) %>%
  dplyr::select(-statement) %>%
  dplyr::rename(statement = title)
# Write tables
library(xtable)
library(knitr)
library(kableExtra)

seperator <- max(which(plm_results$statement == out$title[1]))
table1 <- kable(plm_results %>% dplyr::select(-statement) %>%
                  dplyr::mutate(Chisq = round(Chisq, digits = 3),
                                p.value = round(p.value, digits = 3)) %>%
                  dplyr::rename(Country = country,
                                `Chi-squared` = Chisq,
                                P = p.value),# %>%
                  # dplyr::mutate(P = cell_spec(P, 'latex',
                  #                  color = ifelse(P <= 0.1, 'darkorange',
                  #                                 ifelse(P < 0.05, 'darkred', 'black')))),
                'latex',
                linesep = "",
                # longtable = T,
                booktabs = T,
                caption = 'Pangel Granger Causality test results') %>%
  kable_styling() %>%
  group_rows(paste0('A. ', as.character(out$title[1])), 1,seperator) %>%
  group_rows(paste0('B. ', as.character(out$title[2])), (seperator+1),nrow(plm_results)) %>%
  add_footnote(label = 'Country-specific Granger causality test values. The null hypotheses (in bold) preceding each section can be rejected at a P-value of less than 0.05, both at the individual country level, or as the aggregate Granger test statistic (also in bold).', notation = 'symbol')
# the_statement <- 'Malaria causes GDP'
# table1 <- xtable(plm_results %>% filter(statement == the_statement) %>%
#          dplyr::select(-statement))
cat(table1, file = 'tables/table1.tex')


plm_results %>%
  group_by(statement) %>%
  summarise(chi = mean(Chisq))
out %>% dplyr::select(model, title, p, zbar)
# Misc charts

# png(filename = 'figures/map.png', res = 600, width = 2500, height = 2500)
# gmap
# dev.off()

# What happens in years following gdp shrinkage

# ggplot(data = model_data %>%
#          filter(!is.na(post_malaria_reduction_cat)),
#        aes(x = gdp_growth_relative)) +
#   geom_density(aes(fill = post_malaria_reduction_cat),
#                alpha = 0.6)


g4a <- ggplot(data = model_data %>%
                filter(!is.na(post_malaria_reduction)),
              aes(x = post_malaria_reduction,
                  y = gdp_growth_relative)) +
  geom_jitter() +
  geom_boxplot(alpha = 0.6) +
  ylim(-0.2, 0.4) +
  labs(x = 'Previous year\'s malaria reduction',
       y = 'Relative GDP growth')+
  geom_hline(yintercept = 0, color = 'red')

g4b <- ggplot(data = model_data %>%
                filter(!is.na(post_gdp_growth)),
              aes(x = post_gdp_growth,
                  y = malaria_reduction_relative)) +
  geom_jitter() +
  geom_boxplot(alpha = 0.6) +
  ylim(-0.2, 0.4) +
  labs(x = 'Previous year\'s GDP growth',
       y = 'Relative malaria reduction') +
  geom_hline(yintercept = 0, color = 'red')
Rmisc::multiplot(g4a, g4b, cols = 2)

ggplot(data = model_data %>%
         filter(!is.na(post_malaria_reduction)),
       aes(x = gdp_growth_relative)) +
  geom_density(aes(group = post_malaria_reduction,
                   fill = post_malaria_reduction),
               alpha = 0.6)
ggplot(data = model_data %>%
         filter(!is.na(post_gdp_growth)),
       aes(x = malaria_reduction_relative)) +
  geom_density(aes(group = post_gdp_growth,
                   fill = post_gdp_growth),
               alpha = 0.6)

# g3 <- 
#   ggplot(data = model_data,
#          aes(x = gdp,
#              y = malaria_prevalence,
#              frame = year,
#              color = country)) +
#   geom_point(alpha = 0.6,
#              size = 3) +
#   geom_path(alpha = 0.6,
#             aes(group = country,
#                 color = country,
#                 cumulative = TRUE)) +
#             # arrow = arrow(type = "closed", ends = 'last', angle = 30, length = unit(0.1, "inches"))
#             # ) +
#   # scale_y_log10() + 
#   scale_x_log10() +
#   # facet_wrap(~country, scales = 'free') +
#   labs(x = 'GDP',
#        y = 'Malaria prevalence') +
#   # stat_smooth(n = 50, span = 1.5) +
#   theme(legend.position = 'none') +
#   scale_color_manual(name = 'Country',
#                      values = colorRampPalette(brewer.pal(n = 9, 
#                                                           name = 'Spectral'))(length(unique(model_data$country))))
# 
# g3
# gganimate(g3, interval = 1)


# Did malaria increase and gdp decrease in all countries
x <- model_data %>%
  arrange(year) %>%
  filter(year == dplyr::first(year) |
           year == dplyr::last(year)) %>%
  group_by(country) %>%
  summarise(m1 = dplyr::first(malaria_prevalence),
            m2 = dplyr::last(malaria_prevalence),
            g1 = dplyr::first(gdp),
            g2 = dplyr::last(gdp)) %>%
  ungroup %>%
  mutate(m = m2 > m1,
         g = g2 > g1)
# Which countries had growth in malaria
x %>% filter(m)
# Which countries had shrinkage in gdp
x %>% filter(!g)
# # Demean
# demeaner <- function(x) if(is.numeric(x)) x - mean(x) else x
# model_data <- model_data %>%
#   group_by(country) %>%
#   mutate(gdp_demeaned = demeaner(gdp),
#          malaria_prevalence_demeaned = demeaner(malaria_prevalence)) %>%
#   ungroup

# # Estimate the trends
# malaria_trend <- lm(malaria_prevalence ~ year, data = model_data)
# gdp_trend <- lm(gdp ~ year, data = model_data)
# # Get de-trended


ggplot(data = x,
       aes(x = gdp,
           y = malaria)) +
  geom_point()

x <- model_data %>%
  group_by(country) %>%
  # mutate(gdp_rel = gdp / dplyr::first(gdp),
  #        malaria_rel = 1 - (malaria_prevalence / dplyr::first(malaria_prevalence))) %>%
  # ungroup %>%
  mutate(gdp_rel = gdp / dplyr::first(gdp),
         malaria_rel = malaria_prevalence / dplyr::first(malaria_prevalence)) %>%
  mutate(gdp_rel = scale(gdp_rel),
         malaria_rel = scale(malaria_rel),
         growth_relative = scale(growth_relative)) %>%
  ungroup %>%
  dplyr::select(country, year, gdp_rel, growth_relative, malaria_rel) %>%
  gather(key, value, gdp_rel:malaria_rel) %>%
  dplyr::select(country, year, key, value)
ggplot(data = x %>% filter(key %in% c('growth_relative', 'malaria_rel')),
       aes(x = year,
           y = value)) +
  geom_line(aes(color = key)) +
  facet_wrap(~country, ncol = 4) +
  theme(legend.position = 'bottom')

