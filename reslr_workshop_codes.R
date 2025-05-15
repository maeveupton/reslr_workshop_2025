# Workshop for reslr package

# Clear workspace
rm(list = ls())
#---------Set working directory--------------
setwd("/Users/maeve.upton/Dropbox/PostDoc_UL_2023:24/Presentations/SeaLevelIreland_reslr_workshop2025/code_plots")
#----------Load packages--------------------
library(devtools)
#install_github("maeveupton/reslr", force = TRUE)
library(reslr)
library(tidyverse)
library(ggplot2)
library(ggtext)# for the map
library(geosphere) #distm
library("rnaturalearth")#n_states for map
library(ggrepel)# labels_repel
library(ggspatial)# annotation_scale
library(xtable)# Writing latex tables of results

#------ Input Proxy Data-------------
# Read in Andy's updated data:
global_data <- read_csv("https://www.dropbox.com/s/kqv3o10dnnbx38w/CommonEra2023_new.csv?dl=1")
colnames(global_data)<- c("Basin","Region","Site","Reference","Indicator",
                          "Latitude","Longitude","RSL","RSL_err_upr","RSL_err_lwr","Age","Age_2_err_upr","Age_2_err_lwr")
# Updating the column names:
global_df <- global_data %>% 
  group_by(Site) %>% 
  # Change the RSL_err from 2 sigma back to 1 sigma
  mutate(RSL_err = (RSL_err_upr + RSL_err_lwr)/2,
  # Average of upper and lower error for Age
         Age_err = ((Age_2_err_upr/2) + (Age_2_err_lwr/2))/2) %>% 
  # Only looking at Common era
  filter(Age >= 0) 


# -------- Single Site -------------------------
# Irish site only
ire_site <- global_df %>% filter(Region == "Ireland")

# We include tide gauges for each proxy site:
ire_reslr <- reslr_load(data = ire_site)
print(ire_reslr)
saveRDS(ire_reslr,file = "ire_input.rds")

# Plotting the raw data
raw_plot <- plot(ire_reslr)
ggsave(raw_plot,filename = "fig/ireland_site.pdf", width = 10, height = 6)

# Run mcmc-----------------
ire_reslr_output <- reslr_mcmc(ire_reslr,
                               model_type = "ni_spline_t")
saveRDS(ire_reslr_output,file = "ire_reslr_output.rds")

cat("Finished model")

ire_reslr_output <- readRDS("ire_reslr_output.rds")
summary(ire_reslr_output)

# Plotting full dataset------------------------------------
totalfit <- plot(ire_reslr_output)
ggsave(totalfit,filename = "fig/total_mod.pdf", width = 10, height = 6)
totalrate <- plot(ire_reslr_output,
                  plot_type = "rate_plot")
ggsave(totalrate,filename = "fig/total_rate.pdf", width = 10, height = 6)

# Code for mapping sites
world <- ne_countries(scale = "medium", returnclass = "sf")
ire_data <- ire_reslr$data
map <-ggplot(world) +
  geom_sf()+
  geom_point(data=ire_data,aes(x = Longitude,y = Latitude),colour = "red")+
  geom_label(data=ire_data,aes(x = Longitude-5,
                                     y = Latitude+1,
                                     label = unique(SiteName)))+
  coord_sf(xlim = c(-20, 10), ylim = c(45, 58), expand = FALSE)+
  theme_bw()
ggsave(map,filename = "fig/map.pdf", width = 10, height = 6)


# ------ Multiple Site Example ------------
# Irish site & UK
multi_site <- global_df %>% filter(Region %in% c("Ireland",
                                                 "South West England",
                                                 "Scotland"))
# Input dataframe
multi_site_reslr <- 
  reslr_load(data = multi_site)
print(multi_site_reslr)
saveRDS(multi_site_reslr,file = "multi_input.rds")

# Plotting the raw data
raw_plot_multi <- plot(multi_site_reslr)
ggsave(raw_plot_multi,filename = "fig/multi_site.pdf", width = 10, height = 6)
raw_plot_multi

# Run mcmc-----------------
# Warning 1minute to run
multi_reslr_output <- reslr_mcmc(multi_site_reslr,
                                 model_type = "ni_spline_st")
saveRDS(multi_reslr_output,file = "reslr_output_st_multi.rds")

cat("Finished model")

multi_reslr_output <- readRDS("reslr_output_st_multi.rds")
print(multi_reslr_output)
summary(multi_reslr_output)

# Plot results
totalfit_multi <- plot(multi_reslr_output)
ggsave(totalfit_multi,filename = "fig/total_mod_multi.pdf", width = 10, height = 6)
totalrate_multi <- plot(multi_reslr_output,
                        plot_type = "rate_plot")
ggsave(totalrate_multi,filename = "fig/total_rate_multi.pdf", width = 10, height = 6)
# Map
world <- ne_countries(scale = "medium", returnclass = "sf")
multi_data <- multi_site_reslr$data
labels_issue <- multi_data  %>% dplyr::select(SiteName,Longitude,Latitude) %>% unique
map_multi <-ggplot(world) +
  geom_sf()+
  geom_point(data=multi_data,aes(x = Longitude,y = Latitude),colour = "red")+
  geom_label_repel(data=labels_issue,aes(x = Longitude,
                                         y = Latitude,label = Site))+
  coord_sf(xlim = c(-20, 10), ylim = c(45, 60), expand = FALSE)+
  theme_bw()
map_multi
ggsave(map_multi,filename = "fig/map_multi.pdf", width = 10, height = 6)

# ------ NIGAM example -----------
# Extra sites 
nigam_site <- global_df %>% filter(Region %in% c("Ireland",
                                                 "South West England",
                                                 "Scotland",
                                                 "Iceland",
                                                 "Isle of Wight"))

# We include tide gauges for each proxy site:
nigam_site_reslr <- 
  reslr_load(data = nigam_site,
             include_tide_gauge = TRUE,
             TG_minimum_dist_proxy = TRUE,
             include_linear_rate = TRUE)

print(nigam_site_reslr)
saveRDS(nigam_site_reslr,file = "nigam_input.rds")

# Map
world <- ne_countries(scale = "medium", returnclass = "sf")
nigam_data <- nigam_site_reslr$data
labels_issue <- nigam_data  %>% dplyr::select(SiteName,Longitude,Latitude) %>% unique
map_nigam <-ggplot(world) +
  geom_sf()+
  geom_point(data=nigam_data,aes(x = Longitude,y = Latitude),colour = "red")+
  geom_label_repel(data=labels_issue,aes(x = Longitude,
                                         y = Latitude,label = SiteName))+
  coord_sf(xlim = c(-30, 10), ylim = c(45, 70), expand = FALSE)+
  theme_bw()
map_nigam
ggsave(map_nigam,filename = "fig/map_nigam.pdf", width = 10, height = 6)

# Plotting the raw data
raw_plot_nigam <- plot(nigam_site_reslr)
ggsave(raw_plot_nigam,filename = "fig/nigam_site.pdf", width = 10, height = 6)
raw_plot_nigam

# Run mcmc-----------------
# Warning ~10 minute to run
nigam_reslr_output <- reslr_mcmc(nigam_site_reslr,
                                 model_type = "ni_gam_decomp")
saveRDS(nigam_reslr_output,file = "reslr_output_nigam.rds")

cat("Finished model")

nigam_reslr_output <- readRDS("reslr_output_nigam.rds")
print(nigam_reslr_output)
summary(nigam_reslr_output)

# Plot results
totalfit_nigam<- plot(nigam_reslr_output)
ggsave(totalfit_nigam,filename = "fig/total_mod_nigam.pdf", width = 10, height = 6)
totalfit_nigam
totalrate_nigam <- plot(nigam_reslr_output,
                        plot_type = "rate_plot")
ggsave(totalrate_nigam,filename = "fig/total_rate_nigam.pdf", width = 10, height = 6)
totalrate_nigam

# Advanced-----
# Changing the smoothness parameters 

res_ni_sp_t <- 
  reslr_mcmc(nigam_site_reslr, 
             model_type = "ni_gam_decomp",
             spline_nseg_t = 15,
             spline_nseg_st = 6)
saveRDS(res_ni_sp_t,file = "reslr_output_nigam_smoother.rds")
totalrate_nigam_smooth <- plot(res_ni_sp_t,
                        plot_type = "rate_plot")


