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
  africa_fortified <- broom::tidy(africa1, id = 'OBJECTID')
  
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
         gdp_growth = 
           gdp - 
           dplyr::lag(gdp, n = 1),
         gdp_growth_relative = 
           (gdp / 
           dplyr::lag(gdp, n = 1) - 1)) %>%
  ungroup

# Keep only those countries in Africa
africa_panel <- panel %>%
  group_by(country) %>%
  filter(!all(is.na(malaria_prevalence))) %>%
  ungroup

model_data <- africa_panel %>%
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
                group = country),
            alpha = 0.6) +
  # scale_y_log10() +
  facet_wrap(~key, scales = 'free_y') +
  labs(x = 'Year',
       y = 'Value',
       title = 'A') +
  geom_line(data = x %>% dplyr::mutate(key = ifelse(key == 'gdp', 'GDP per Capita', 'Malaria prevalence')), 
            aes(x = year,
                y = value),
            color = 'red', 
            size = 2,
            alpha = 0.7)



# See gdp and malaria prevalence
g2 <- ggplot(data = model_data,
       aes(x = gdp_growth_relative,
           y = malaria_reduction_relative)) +
  geom_point(size = 1, alpha = 0.6) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = 'GDP growth',
       y = 'Malaria prevalence reduction',
       title = 'B') +
  xlim(-1,1) +
  ylim(-1,1)

library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(dplyr)

g3 <- 
  ggplot(data = model_data,
         aes(x = gdp,
             y = malaria_prevalence,
             color = country)) +
  geom_point(alpha = 0.6,
             size = 0.1) +
  geom_path(alpha = 0.7,
            aes(group = country),
  arrow = arrow(type = "open", ends = 'first', angle = 30, length = unit(0.2, "cm"))) +
  # ) +
  # scale_y_log10() +
  scale_x_log10() +
  # facet_wrap(~country, scales = 'free') +
  labs(x = 'GDP per Capita (log)',
       y = 'Malaria prevalence') +
  # stat_smooth(n = 50, span = 1.5) +
  theme(legend.position = 'none') +
  scale_color_manual(name = 'Country',
                     values = colorRampPalette(brewer.pal(n = 9, 
                                                          name = 'Spectral'))(length(unique(model_data$country)))) +
  facet_wrap(~country, ncol = 3) +
  theme(strip.text = element_text(size=8),
        # strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 8))
g3

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

png(filename = 'figures/descriptive.png', res = 600, width = 6000, height = 2200)
# layout <- matrix(c(1,2,3,3,3,3), nrow = 3, byrow = TRUE)
# Rmisc::multiplot(plotlist = list(g1, g2, g3), layout = layout)
Rmisc::multiplot(g1, g2, cols = 2)
dev.off()

png(filename = 'figures/paths.png', res = 600, width = 4000, height = 4000)
g3
dev.off()




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

plm_data <- plm::pdata.frame(x = model_data,
                             index = c('country'))

out <- data.frame(model = 1:2,
                  statement = c('Malaria causes GDP',
                                'GDP causes malaria'),
                  p = NA)

fit <- pgrangertest(formula = 
                      growth ~
                      # log(gdp) ~ 
                      malaria_prevalence,
                    test = 'Zbar',
                    # order = 3,
                    data = plm_data)
out$p[1] <- fit$p.value; out$p[1]
fit <- pgrangertest(formula = malaria_prevalence ~ 
                      growth,
                    # log(gdp),
                    # order = 3,
                    test = 'Zbar',
                    data = plm_data)
out$p[2] <- fit$p.value; out$p[2]
out

# Misc charts



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


# x <- vars::VAR(y = africa_panel %>%
#            dplyr::filter(!is.na(gdp),
#                   !is.na(malaria_prevalence)) %>%
#            dplyr::select(gdp,
#                          malaria_prevalence),
#          p = 1,
#          type = 'both')
# out <- vars::causality(x = x,
#                  cause = 'gdp')
#                  # cause = 'malaria_prevalence')
# out