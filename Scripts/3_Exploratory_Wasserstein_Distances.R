### We perform an exploratory analysis based on wasserstein distances at Provinces levels.
### The following code both perform the analysis and saves the plots in the form of ggplot objects. 
### If you need the numeric results, just use the corresponding data frame, which are also saved. 
### Plots are saved for all classes and years.
rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/Aggregated_Provinces.Rdata")
load("Output/Data/Province_geometry_polygons.Rdata")

provinces_reduced = provinces_aggregated %>%  dplyr::select(NOME_PROVINCIA,partial_date_death,CL_ETA, starts_with("T")) %>% group_by(NOME_PROVINCIA,CL_ETA)
### If you want other years, just change the following lines. Here we compute wasserstein distances from the mean
### of the previous four years
provinces_reduced = provinces_reduced %>% mutate_at(.vars = paste0("T_",11:20),.funs = function(x){return(x/sum(x))}) %>% mutate (M_16 = 1/4*(T_16 + T_15 + T_14 + T_13), M_17 = 1/4*(T_17 + T_16 + T_15 + T_14), M_18 = 1/4*(T_18 + T_17 + T_16 + T_15), M_19 = 1/4*(T_19 + T_18 + T_17 + T_16)  )
prov_df_wass = provinces_reduced %>% 
  dplyr::summarise(wass17 = as.numeric(try(wasserstein1d(a = as.numeric(partial_date_death[which(M_16 > 0)]), b = as.numeric(partial_date_death[which(T_17 > 0)]), wa = M_16[which(M_16 > 0)], wb = T_17[which(T_17 > 0)]),silent = T)),
                   wass18 = as.numeric(try(wasserstein1d(a = as.numeric(partial_date_death[which(M_17 > 0)]), b = as.numeric(partial_date_death[which(T_18 > 0)]), wa = M_17[which(M_17 > 0)], wb =  T_18[which(T_18 > 0)]),silent = T)),
                   wass19 = as.numeric(try(wasserstein1d(a = as.numeric(partial_date_death[which(M_18 > 0)]), b = as.numeric(partial_date_death[which(T_19 > 0)]), wa = M_18[which(M_18 > 0)], wb = T_19[which(T_19 > 0)]),silent = T)),
                   wass20 = as.numeric(try(wasserstein1d(a = as.numeric(partial_date_death[which(M_19 > 0)]), b = as.numeric(partial_date_death[which(T_20 > 0)]), wa = M_19[which(M_19 > 0)], wb = T_20[which(T_20 > 0)]),silent = T)))

classe = unique(provinces_reduced$CL_ETA) ## age classes
years = paste0("T_",11:20)
class_labs = c("0 - 49 years", "50 - 69 years", "70 years +") ## labels for each age classes
year_labs = paste0("20",11:20)
names(class_labs) =  classe
names(year_labs) = years

names = year_labs[7:10]
names(prov_df_wass) = c("NOME_PROVINCIA","CL_ETA",names)

plot_list_wass_fullyear = list()

for (class in classe){
  prov_poly_t = st_as_sf(prov_poly)  %>% left_join(prov_df_wass %>% filter (CL_ETA == class), by = c("DEN_PCM" = "NOME_PROVINCIA")) 
  prov_poly_t = prov_poly_t %>% pivot_longer(cols = paste0("20",17:20), values_to = "dist", names_to = "year" ) %>% group_by(DEN_PCM,CL_ETA)
  prov_poly_t = st_as_sf(prov_poly_t)
  prov_poly_t$year = factor(prov_poly_t$year, levels = names)
  g = prov_poly_t %>% ggplot() + geom_sf(aes(fill = log(dist))) + scale_fill_viridis(option = "inferno") + ggtitle(aes(lab = year)) + facet_wrap(.~ year,nrow = 1) +
  clean_theme()+theme_pubclean(base_size = 40) + labs_pubr(base_size = 40) + theme(legend.position="bottom") + rremove("xlab") + rremove("ylab") + rremove("xy.text") + rremove("ticks") + ggtitle("") 
  plot_list_wass_fullyear[[class]] = g
}

save(plot_list_wass_fullyear, file = Rdata_path("Wasserstein_plots_fullyear"))


#### Now we go to time windows. We also want to compute the correlation with mortality.

rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/Aggregated_Provinces.Rdata")

windows_starts = seq.Date(as.Date("2020-01-01"),as.Date("2020-11-01"),by = "month")
windows_end = seq.Date(as.Date("2020-03-01"),as.Date("2020-12-01"),by = "month")
windows_end = c(windows_end,as.Date("2020-12-31"))
Provinces_wass_temporal_windows = provinces_aggregated %>% group_by(NOME_PROVINCIA,CL_ETA) %>% summarise(n()) %>% dplyr::select(NOME_PROVINCIA,CL_ETA)
classe = c("0_10","11_14","15_21") ## age classes

nwinds = length(windows_starts)
corrs = list()
x11()
par(mfrow = c(3,4))
for (class in classe) corrs[[class]] = NULL
for(j in 1:nwinds)
{
  provinces_reduced = provinces_aggregated %>% filter(partial_date_death <= windows_end[j] & partial_date_death >= windows_starts[j]) %>% 
    dplyr::select(NOME_PROVINCIA,partial_date_death,CL_ETA, starts_with("T")) %>% group_by(NOME_PROVINCIA,CL_ETA) %>% 
    left_join(provinces_data, by = c("CL_ETA" = "CL_ETA", "NOME_PROVINCIA" = "provincia"))
  provinces_reduced = provinces_reduced %>% mutate(T_15_19 = (T_11 + T_12 + T_13 + T_14 + T_15 + T_16 + T_17 + T_18 + T_19)/9)
  provinces_reduced = provinces_reduced  %>% mutate(dens_15_19 = T_15_19 /sum(T_15_19), dens_20 = T_20/sum(T_20), ecdf_15_19 = cumsum(dens_15_19), ecdf_20 = cumsum(dens_20) ) 
  temp = provinces_reduced %>% 
    summarise(wass = as.numeric(try(wasserstein1d(a = as.numeric(partial_date_death[which(dens_15_19 > 0)]), b = as.numeric(partial_date_death[which(dens_20 > 0)]), wa = dens_15_19[which(dens_15_19 > 0)], wb = dens_20[which(dens_20 > 0)]),silent = T))
              ,mortality = sum(T_20)/max(num_residenti))
  temp[is.na(temp)] = 0
  temp[[paste0("window",j)]] = temp$wass
  for ( class in classe){
    mort = temp$mortality[temp$CL_ETA == class]
    dist = temp$wass[temp$CL_ETA == class]
    corrs[[class]] = c(corrs[[class]], cor(mort,dist))
    if (class == "15_21"){
    plot(mort,dist, main = class)
    }
  }
  temp$mortality = NULL
  Provinces_wass_temporal_windows  = Provinces_wass_temporal_windows %>% left_join(temp, by = c("NOME_PROVINCIA" = "NOME_PROVINCIA", "CL_ETA" = "CL_ETA")) %>% dplyr::select(-wass)
}


save(Provinces_wass_temporal_windows, corrs, file = Rdata_path("Wasserstein_windows"))
##### For Plots on temporal windows see PaperPlots.r, they are a bit more involved. 


