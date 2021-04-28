rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/dataset_aggregato_provinciale.Rdata")
load("Output/Data/dataset_aggregato_comunale.Rdata")

windows_starts = seq.Date(as.Date("2020-01-01"),as.Date("2020-11-01"),by = "month")
windows_end = seq.Date(as.Date("2020-03-01"),as.Date("2020-12-01"),by = "month")
windows_end = c(windows_end,as.Date("2020-12-31"))
prov_df_wass_temporal_windows = prov_df_aggr %>% group_by(NOME_PROVINCIA,CL_ETA) %>% summarise(n()) %>% dplyr::select(NOME_PROVINCIA,CL_ETA)

nwinds = length(windows_starts)

for(j in 1:nwinds)
{
  prov_df_aggr_red = prov_df_aggr %>% filter(partial_date_death <= windows_end[j] & partial_date_death >= windows_starts[j]) %>% dplyr::select(NOME_PROVINCIA,partial_date_death,CL_ETA, starts_with("T")) %>% group_by(NOME_PROVINCIA,CL_ETA)
  prov_df_aggr_red = prov_df_aggr_red %>% mutate(T_15_19 = (T_11 + T_12 + T_13 + T_14 + T_15 + T_16 + T_17 + T_18 + T_19)/9)
  prov_df_aggr_red = prov_df_aggr_red  %>% mutate(dens_15_19 = T_15_19 /sum(T_15_19), dens_20 = T_20/sum(T_20), ecdf_15_19 = cumsum(dens_15_19), ecdf_20 = cumsum(dens_20) ) 
  temp = prov_df_aggr_red %>% 
    summarise(wass = as.numeric(try(wasserstein1d(a = as.numeric(partial_date_death[which(dens_15_19 > 0)]), b = as.numeric(partial_date_death[which(dens_20 > 0)]), wa = dens_15_19[which(dens_15_19 > 0)], wb = dens_20[which(dens_20 > 0)]),silent = T)))
  temp[is.na(temp)] = 0
  temp[[paste0("window",j)]] = temp$wass
  prov_df_wass_temporal_windows  = prov_df_wass_temporal_windows %>% left_join(temp, by = c("NOME_PROVINCIA" = "NOME_PROVINCIA", "CL_ETA" = "CL_ETA")) %>% dplyr::select(-wass)
}

com_df_wass_temporal_windows = com_df_aggr %>% group_by(COD_PROVCOM,CL_ETA) %>% summarise(n()) %>% dplyr::select(COD_PROVCOM,CL_ETA)
for(j in 1:nwinds)
{
  com_df_aggr_red = com_df_aggr %>% filter(partial_date_death <= windows_end[j] & partial_date_death >= windows_starts[j]) %>% dplyr::select(COD_PROVCOM,partial_date_death,CL_ETA, starts_with("T")) %>% group_by(COD_PROVCOM,CL_ETA)
  com_df_aggr_red = com_df_aggr_red %>% mutate(T_15_19 = (T_11 + T_12 + T_13 + T_14 + T_15 + T_16 + T_17 + T_18 + T_19)/9)
  com_df_aggr_red = com_df_aggr_red  %>% mutate(dens_15_19 = T_15_19 /sum(T_15_19), dens_20 = T_20/sum(T_20), ecdf_15_19 = cumsum(dens_15_19), ecdf_20 = cumsum(dens_20) ) 
  temp = com_df_aggr_red %>% 
    summarise(wass = as.numeric(try(wasserstein1d(a = as.numeric(partial_date_death[which(dens_15_19 > 0)]), b = as.numeric(partial_date_death[which(dens_20 > 0)]), wa = dens_15_19[which(dens_15_19 > 0)], wb = dens_20[which(dens_20 > 0)]),silent = T)))
  temp[[paste0("window",j)]] = temp$wass
  temp[is.na(temp)] = 0
  com_df_wass_temporal_windows  = com_df_wass_temporal_windows %>% left_join(temp, by = c("COD_PROVCOM" = "COD_PROVCOM", "CL_ETA" = "CL_ETA")) %>% dplyr::select(-wass)
}

save(prov_df_wass_temporal_windows, file = Rdata_path("Wasserstein_temporal_windows"))
save(com_df_wass_temporal_windows, file = Rdata_path("Wasserstein_temporal_windows_comuni"))


### Plotting
rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/Wasserstein_temporal_windows.Rdata")
load("Output/Data/Province_geometry_polygons.Rdata")
plist = list()

prov_poly = st_as_sf(prov_poly)  %>% left_join(prov_df_wass_temporal_windows, by = c("DEN_PCM" = "NOME_PROVINCIA")) 
prov_poly = prov_poly %>% pivot_longer(cols = window1:window11, values_to = "distance", names_to = "window" ) %>% group_by(DEN_PCM,CL_ETA)
prov_poly = st_as_sf(prov_poly)
prov_poly$window = factor(prov_poly$window, levels = paste0("window",1:11))

classe = unique(prov_df_wass_temporal_windows$CL_ETA)
for (class in classe){
g = prov_poly %>% filter(CL_ETA == class) %>% ggplot() + geom_sf(aes(fill = log(distance))) + scale_fill_viridis(option = "inferno") +  ggrepel::geom_text_repel(aes(label = DEN_PCM,geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20) + facet_wrap(.~ window,nrow = 2) + ggtitle(class) 
pdf(paste0("Output/Plot/wass_windows_",class,".pdf"),width = 20,height = 14)
plot(g)
dev.off()
}

load("Output/Data/Wasserstein_temporal_windows_comuni.Rdata")
load("Output/Data/Comuni_geometry_polygons.Rdata")

plist = list()

com_poly = st_as_sf(com_poly) %>% left_join(com_df_wass_temporal_windows, by = c("PRO_COM" = "COD_PROVCOM")) %>% pivot_longer(cols = window1:window11, values_to = "distance", names_to = "window" ) %>% group_by(PRO_COM,CL_ETA) %>%dplyr::select(PRO_COM,CL_ETA,distance,window,geometry)
com_poly = st_as_sf(com_poly)
com_poly = na.omit(com_poly)
com_poly$window = factor(com_poly$window, levels = paste0("window",1:11))

for (class in classe){
  temp = com_poly %>% filter(CL_ETA == class & distance > 0 & !(is.na(distance)))
  temp = st_as_sf(temp)
  g = ggplot(temp %>% group_by(window)) + geom_sf(aes(fill = log(distance)), color = NA) + scale_fill_viridis(option = "inferno") + facet_wrap(.~ window) + ggtitle(class)
  png(paste0("Output/Plot/wass_windows_comuni",class,".png"),width = 20,height = 14, units = "in", res = 1200)
  plot(g)
  dev.off()
}