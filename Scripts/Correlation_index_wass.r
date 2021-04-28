rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/dataset_aggregato_provinciale.Rdata")
load("Output/Data/dataset_aggregato_comunale.Rdata")
load("Output/Data/dati_comuni_lisa.Rdata")
load("Output/Data/dati_province_lisa.Rdata")

truncation_date = as.Date("2020-12-31") ## you can play with truncation dates as you like, you can also make windows. Look at the next lines

prov_df_aggr_red = prov_df_aggr %>% filter(partial_date_death <= truncation_date) %>% dplyr::select(NOME_PROVINCIA,partial_date_death,CL_ETA, starts_with("T")) %>% group_by(NOME_PROVINCIA,CL_ETA)
prov_df_aggr_red = prov_df_aggr_red %>% mutate(T_15_19 = (T_11 + T_12 + T_13 + T_14 + T_15 + T_16 + T_17 + T_18 + T_19)/9)
prov_df_aggr_red = prov_df_aggr_red  %>% mutate(dens_15_19 = T_15_19 /sum(T_15_19), dens_20 = T_20/sum(T_20), ecdf_15_19 = cumsum(dens_15_19), ecdf_20 = cumsum(dens_20) ) 
prov_df_wass = prov_df_aggr_red %>% 
  summarise(wass = as.numeric(try(wasserstein1d(a = as.numeric(partial_date_death[which(dens_15_19 > 0)]), b = as.numeric(partial_date_death[which(dens_20 > 0)]), wa = dens_15_19[which(dens_15_19 > 0)], wb = dens_20[which(dens_20 > 0)]),silent = T)))
prov_df_wass[is.na(prov_df_wass)] = 0

com_df_aggr_red = com_df_aggr %>% filter(partial_date_death <= truncation_date) %>% dplyr::select(COD_PROVCOM,partial_date_death,CL_ETA, starts_with("T")) %>% group_by(COD_PROVCOM,CL_ETA)
com_df_aggr_red = com_df_aggr_red %>%  mutate(T_15_19 = (T_11 + T_12 + T_13 + T_14 + T_15 + T_16 + T_17 + T_18 + T_19)/9)
com_df_aggr_red = com_df_aggr_red  %>% mutate(dens_15_19 = T_15_19 /sum(T_15_19), dens_20 = T_20/sum(T_20), ecdf_15_19 = cumsum(dens_15_19), ecdf_20 = cumsum(dens_20) )
com_df_wass = com_df_aggr_red %>%
  summarise(wass = as.numeric(try(wasserstein1d(a = as.numeric(partial_date_death[which(dens_15_19 > 0)]), b = as.numeric(partial_date_death[which(dens_20 > 0)]), wa = dens_15_19[which(dens_15_19 > 0)], wb = dens_20[which(dens_20 > 0)]),silent = T)))
com_df_wass[is.na(com_df_wass)] = 0

classe = c("0_10","11_14","15_21")
for (class in classe)
  print(cor((prov_df_aggr_lisa %>% filter(CL_ETA == class) %>% arrange(NOME_PROVINCIA))$T_20, (prov_df_wass %>% filter(CL_ETA == class) %>% arrange(NOME_PROVINCIA))$wass ))


for (class in classe){
  codes = com_df_aggr_lisa$COD_PROVCOM[which(com_df_aggr_lisa$num_residenti > 5000)]
  print(cor((com_df_aggr_lisa %>% filter(CL_ETA == class, COD_PROVCOM %in% codes) %>% arrange(COD_PROVCOM))$T_20, (com_df_wass %>% filter(CL_ETA == class, COD_PROVCOM %in% codes) %>% arrange(COD_PROVCOM))$wass ))

}
