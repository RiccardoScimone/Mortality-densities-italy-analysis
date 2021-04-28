##### Plots for Spasta contribution ##################

##### Preliminary plots of raw data. Only 2017-2020 are plotted.
rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/Aggregated_Provinces.Rdata")
load("Output/Data/Smoothing_datasets.Rdata")
classe = c("0_10","11_14","15_21")
folder_names = c("0-49","50-69","70+")
names(folder_names) = classe
### Prepare Provinces to plot
for ( class in classe){
  plot_provs = provinces_aggregated %>% filter(CL_ETA == class) %>% dplyr::select(NOME_PROVINCIA,partial_date_death,T_17,T_18, T_19, T_20)
  names(plot_provs) = c("name","date", "2017","2018", "2019", "2020")
  plot_provs = plot_provs %>% pivot_longer(cols = c("2017","2018", "2019","2020"), names_to = "Year", values_to = "deaths") %>% filter(!(date == "2020-02-29" & Year != "2020"))
  colors = rep("gray",107)
  plot_provs$name = factor(plot_provs$name, levels = c(setdiff(plot_provs$name, c("Roma", "Milano", "Bergamo","Napoli") ), c("Roma", "Milano", "Bergamo","Napoli")))
  names(colors) = levels(factor(plot_provs$name))
  colors[["Roma"]] = "purple"
  colors[["Milano"]] = "green"
  colors[["Bergamo"]] = "red"
  colors[["Napoli"]] = "blue"
  colScale = scale_colour_manual(name = "name", values = colors)
  tot_res = sum ((provinces_data %>% filter(CL_ETA == class))$num_residenti)
  means_provs = plot_provs %>% left_join(provinces_data %>% filter(CL_ETA == class) %>% dplyr::select(provincia,num_residenti), by = c("name" = "provincia")) %>% ungroup() %>% group_by(date, Year) %>% summarise(md = sum(deaths*num_residenti)/tot_res)
  plot_prov = ggplot(plot_provs, mapping = aes(x = date, y = deaths, color = name) )+ colScale +   geom_line(aes(alpha  = ifelse(name %in% c("Roma", "Milano", "Bergamo","Napoli"), 1, 0.5))) + geom_line(aes(y = md, x = date, color = year), data = means_provs, col = "black", lwd = 1.4) +
    facet_grid(~Year) +scale_x_date(date_labels = "%b") + theme_pubr(base_size = 20)  + rremove("legend")+ rremove("xlab") + rremove("ylab")  + labs_pubr(base_size = 20) + font("xy.text",size = 17)
#  x11()
#  plot(plot_prov)
  pdf(paste0("Output/Plot/",folder_names[[class]],"/Paper_data_presentation.pdf"), width = 40, height = 20)
  plot(plot_prov)
  dev.off()
  
  #### Plot of normalized data
  plot_provs = data_to_smooth_provinces %>% filter(CL_ETA == class) %>% ungroup() %>% dplyr::select(NOME_PROVINCIA,partial_date_death,T_17,T_18, T_19, T_20)
  names(plot_provs) = c("name","date", "2017","2018", "2019", "2020")
  plot_provs = plot_provs %>% pivot_longer(cols = c("2017","2018", "2019","2020"), names_to = "Year", values_to = "deaths") %>% filter(!(date == "2020-02-29" & Year != "2020"))
  colors = rep("gray",107)
  plot_provs$name = factor(plot_provs$name, levels = c(setdiff(plot_provs$name, c("Roma", "Milano", "Bergamo","Napoli") ), c("Roma", "Milano", "Bergamo","Napoli")))
  names(colors) = levels(factor(plot_provs$name))
  colors[["Roma"]] = "purple"
  colors[["Milano"]] = "green"
  colors[["Bergamo"]] = "red"
  colors[["Napoli"]] = "blue"
  colScale = scale_colour_manual(name = "name", values = colors)
  means_provs = plot_provs %>% left_join(provinces_data %>% filter(CL_ETA == class) %>% dplyr::select(provincia,num_residenti), by = c("name" = "provincia")) %>% ungroup() %>% group_by(date, Year) %>% summarise(md = sum(deaths*num_residenti)/tot_res)
  plot_prov = ggplot(plot_provs, mapping = aes(x = date, y = deaths, color = name) )+ colScale +   geom_line(aes(alpha  = ifelse(name %in% c("Roma", "Milano", "Bergamo","Napoli"), 1, 0.5))) + geom_line(aes(y = md, x = date, color = year), data = means_provs, col = "black", lwd = 1.4) +
    facet_wrap(~Year, nrow = 1) +scale_x_date(date_labels = "%b") + theme_pubr(base_size = 20)  + rremove("legend")+ rremove("xlab") + rremove("ylab")  + labs_pubr(base_size = 20) + font("xy.text",size = 17)
  #  x11()
  #  plot(plot_prov)
  pdf(paste0("Output/Plot/",folder_names[[class]],"/Paper_data_presentation2.pdf"), width = 40, height = 20)
  plot(plot_prov)
  dev.off()
  
}

#### Plots of scalar indicators 
load("Output/Data/Exploratory_plots_province.Rdata")
load("Output/Data/Exploratory_plots_municipalities.Rdata")

for ( class in classe)
{
g = ggarrange(  
            ggarrange(plotlist = list(plot_list_provinces_heatmaps[[class]]$T_17,plot_list_provinces_heatmaps[[class]]$T_18,plot_list_provinces_heatmaps[[class]]$T_19,plot_list_provinces_heatmaps[[class]]$T_20), common.legend = T, nrow = 1, labels = NULL,  legend = "bottom"),
            ggarrange(plotlist = list(plot_list_provinces_moran[[class]]$T_17,plot_list_provinces_moran[[class]]$T_18,plot_list_provinces_moran[[class]]$T_19,plot_list_provinces_moran[[class]]$T_20), nrow = 1, labels = NULL,common.legend = T, legend = "bottom"),
            ggarrange(plotlist = list(plot_list_municipalities_moran[[class]]$T_17,plot_list_municipalities_moran[[class]]$T_18,plot_list_municipalities_moran[[class]]$T_19,plot_list_municipalities_moran[[class]]$T_20), nrow = 1, labels = NULL, common.legend = T, legend = "none"),
            nrow = 3, common.legend = F)
pdf(paste0("Output/Plot/",folder_names[[class]],"/Exploration1.pdf"), width = 15, height = 20)
plot(g)
dev.off()
}


##### Wasserstein plots ##############
############### Wasserstein Plots, Windows #####################################################
rm(list = ls())
source("Scripts/Utility_Functions.R")
load("Output/Data/Wasserstein_windows.Rdata")
load("Output/Data/Province_geometry_polygons.Rdata")
classe = c("0_10","11_14","15_21")
folder_names = c("0-49","50-69","70+")
names(folder_names) = classe
to_select = seq(3,13,by = 2)
Provinces_wass_temporal_windows = Provinces_wass_temporal_windows %>% dplyr::select (c(1,2,to_select))
names = c("Jan-Feb", "Mar-Apr", "May-Jun", "Jul-Aug", "Sept-Oct", "Nov-Dec")
names(Provinces_wass_temporal_windows) = c("NOME_PROVINCIA","CL_ETA",names)
dum_label = function(name, window)
{
  if (window == "Jan-Feb" & name == "Lodi")
    return(name)
  if( window == "Mar-Apr" & (name == "Bergamo" | name == "Cremona")){
    return(name)
  }
  return(NA)
}
dum_label = Vectorize(dum_label)

for ( class in classe)  {
  Provinces_wass_temporal_windows_t = Provinces_wass_temporal_windows %>% filter (CL_ETA == class) 
  prov_poly_t = st_as_sf(prov_poly)  %>% left_join(Provinces_wass_temporal_windows_t, by = c("DEN_PCM" = "NOME_PROVINCIA")) 
  prov_poly_t = prov_poly_t %>% pivot_longer(cols = names, values_to = "distance", names_to = "window" ) %>% group_by(DEN_PCM,CL_ETA)
  prov_poly_t = st_as_sf(prov_poly_t)
  prov_poly_t$window = factor(prov_poly_t$window, levels = names)
  prov_poly_t = prov_poly_t %>% mutate(dum = dum_label(DEN_PCM,window))
  dum_corr = data.frame(window = names, R = corrs[[class]][to_select-2], x = 550000, y = 4000000)
  dum_corr$window = factor(dum_corr$window, levels = names)
  g = prov_poly_t %>% ggplot() + geom_sf(aes(fill = log(distance))) + scale_fill_viridis(option = "inferno") + 
    ggtitle("")  +ggrepel::geom_label_repel(aes(label = dum,geometry = geometry), size = 5, stat = "sf_coordinates",box.padding = 0.5, max.overlaps = 5, min.segment.length = 0,force = 50, segment.size = 0.01,nudge_x = 500000,segment.alpha = 0.5)+
    geom_text(data = dum_corr, aes(x = x, y = y, label = paste0("R = ", round(R,4))), size = 15) + 
    facet_wrap(.~ window,nrow = 2) + clean_theme()+theme_pubclean(base_size = 40) + labs_pubr(base_size = 40) + theme(legend.position="bottom") + rremove("xlab") + rremove("ylab") + rremove("xy.text") + rremove("ticks") 
  pdf(file = paste0("Output/Plot/",folder_names[[class]],"/Paper_Wasserstein2.pdf"),width = 40,height = 20)
  plot(g)
  dev.off()
  knitr::plot_crop(paste0("Output/Plot/",folder_names[[class]],"/Paper_Wasserstein2.pdf"), quiet = T)
}

##### Wasserstein Plot, years ####################
load("Output/Data/Wasserstein_plots_fullyear.Rdata")
for ( class in classe){
  pdf(file = paste0("Output/Plot/",folder_names[[class]],"/Paper_Wasserstein1.pdf"),width = 15,height = 15)
  plot(plot_list_wass_fullyear[[class]])
  dev.off()
  knitr::plot_crop(paste0("Output/Plot/",folder_names[[class]],"/Paper_Wasserstein1.pdf"), quiet = T)
}


###################################### Smoothing Plot #############################à
rm(list = ls())
source("Scripts/Utility_Functions.R")
load("Output/Data/Smoothing_datasets.Rdata")
load("Output/Data/Smoothing_Provinces_52.Rdata")
load("Output/Data/Smoothing_municipalities.Rdata")
load("Output/Data/Aggregated_Municipalities.Rdata")
load("Output/Data/Aggregated_Provinces.Rdata")

classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
class_labs = c("0-49 years", "50-69 years", "70+ years")
year_labs = paste0("20",11:20)
names(class_labs) =  classe
names(year_labs) = years
xcp = as.numeric((as.numeric(as.Date("2020-01-01")):as.numeric(as.Date("2020-12-31"))))
names(year_labs) = years
folder_names = c("0-49","50-69","70+")
names(folder_names) = classe

example_prov = "Bergamo"
example_com = "16024"
com_name = unique(data_to_smooth_municipalities$NOME_COMUNE[data_to_smooth_municipalities$COD_PROVCOM == example_com])
year = "T_20"

for (class in classe)
{
  #x11()
  pdf(paste0("Output/Plot/",folder_names[[class]],"/Smoothing_paper1.pdf"), width = 20, height = 10)
  par(mfrow = c(1,2), cex = 1.5)
  index = which(provinces_names == example_prov)
  plot(flist_province[[class]][[year]][index], col = "red", ylim = c(0,0.02), lwd = 2, xaxt = "n", main = paste0("Prov. Bergamo, ",year_labs[[year]],", ", class_labs[[class]]), ylab = "density",xlab ="")
  points(xcp, (data_to_smooth_provinces %>% filter(CL_ETA == class,NOME_PROVINCIA == example_prov))[[year]], col = "black", pch = 20, cex = 0.7)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  
  index = which(municipalities_codes == example_com)
  plot(flist_comuni[[class]][[year]][index], col = "red", ylim = c(0,0.02), lwd = 2,xaxt = "n", main = paste0("Mun. Bergamo, ",year_labs[[year]],", ", class_labs[[class]]), ylab = "",xlab ="")
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="3 months"), format="%b")
  points(as.numeric((data_to_smooth_municipalities %>% filter(CL_ETA == class,COD_PROVCOM == example_com))$partial_date_death),(data_to_smooth_municipalities %>% filter(CL_ETA == class,COD_PROVCOM == example_com))$T_20 , col = "black", pch = 20, cex = 0.7)
  dev.off()
}

#### Now we plot the whole smoothing for the years we are interested in
for ( class in classe){
  plot_provs = fdata_to_ggplot_df(fdatalist = flist_province[[class]],names = provinces_names, years = c("T_17","T_18","T_19","T_20"), year_labs = year_labs[7:10] )
  colors = rep("gray",107)
  plot_provs$name = factor(plot_provs$name, levels = c(setdiff(plot_provs$name, c("Roma", "Milano", "Bergamo","Napoli") ), c("Roma", "Milano", "Bergamo","Napoli")))
  plot_provs_clr = fdata_to_ggplot_df(fdatalist = clist_province[[class]],names = provinces_names, years = c("T_17","T_18","T_19","T_20"), year_labs = year_labs[7:10] )
  plot_provs_clr$name = factor(plot_provs$name, levels = c(setdiff(plot_provs$name, c("Roma", "Milano", "Bergamo","Napoli") ), c("Roma", "Milano", "Bergamo","Napoli")))
  provinces_data_t = provinces_data %>% filter(CL_ETA == class)
  provinces_data_t = provinces_data_t %>% mutate(weight = num_residenti/sum(provinces_data$num_residenti)) %>% dplyr::select(provincia, weight)
  plot_provs_clr = plot_provs_clr %>% left_join(provinces_data_t, by = c("name" = "provincia"))
  plot_provs_clr =  plot_provs_clr %>% mutate(cl_dens = dens*weight)
  mean_provs = plot_provs_clr %>% group_by(x, Year) %>% summarise(mdens = mean(dens))
  Integrals = mean_provs %>% group_by(Year) %>% summarise(Integrals = int.simpson2(x = x,y = exp(mdens)))
  mean_provs = mean_provs %>% left_join(Integrals)
  mean_provs = mean_provs %>% group_by(Year,x) %>% mutate(meandens = exp(mdens)/Integrals)
  names(colors) = levels(factor(plot_provs$name))
  colScale = scale_colour_manual(name = "name", values = colors)
  lims = range(as.POSIXct( as.Date(plot_provs$x,origin = "1970-01-01")))
  g = ggplot(plot_provs, mapping = aes(x = as.POSIXct( as.Date(x,origin = "1970-01-01")), y = dens, color = name) )+ colScale  +  ylim(0,0.02) +   geom_line(aes(alpha  = ifelse(name %in% c("Roma", "Milano", "Bergamo","Napoli"), 1, 1)), size = 1.1 )  + geom_line(data = mean_provs, mapping=aes(x = as.POSIXct(as.Date(x,origin = "1970-01-01")), y = meandens,color = Year), color = "black",size = 1.4)+
    scale_x_datetime(labels = time_format("%b"),limits = lims) + facet_grid(~Year) + theme_pubr(base_size = 20)  + rremove("legend")+ rremove("xlab") + rremove("ylab")  + labs_pubr(base_size = 20) + font("xy.text",size = 17) 
  #x11()
  pdf(paste0("Output/Plot/",folder_names[[class]],"/Smoothing_paper2_province.pdf"), width = 20, height = 10)
  
  plot(g)
  dev.off()
}


# rm(municipalities_aggregated)
# for ( class in classe){
#   plot_coms = fdata_to_ggplot_df(fdatalist = flist_comuni[[class]],names = municipalities_codes, years = c("T_17","T_18","T_19","T_20"), year_labs = year_labs[7:10] )
#   colors = rep("gray",7900)
#   plot_coms$name = factor(plot_coms$name, levels = c(setdiff(plot_coms$name, c("58091", "15146", "16024","63049") ), c("58091", "15146", "16024","63049")))
#   plot_coms_clr = fdata_to_ggplot_df(fdatalist = clist_comuni[[class]],names = municipalities_codes, years = c("T_17","T_18","T_19","T_20"), year_labs = year_labs[7:10] )
#   plot_coms_clr$name = factor(plot_coms$name)
#   municipality_data_t = municipality_data %>% filter(CL_ETA == class)
#   municipality_data_t = municipality_data_t %>% mutate(weight = num_residenti/sum(municipality_data$num_residenti)) %>% dplyr::select(istat, weight)
#   plot_coms_clr = plot_coms_clr %>% left_join(municipality_data_t, by = c("name" = "istat"))
#   plot_coms_clr =  plot_coms_clr %>% mutate(cl_dens = dens)
#   mean_coms = plot_coms_clr %>% group_by(x, Year) %>% summarise(mdens = mean(cl_dens))
#   Integrals = mean_coms %>% group_by(Year) %>% summarise(Integrals = int.simpson2(x = x,y = exp(mdens)))
#   mean_coms = mean_coms %>% left_join(Integrals)
#   mean_coms = mean_coms %>% group_by(Year,x) %>% mutate(meandens = exp(mdens)/Integrals)
#   names(colors) = levels(factor(plot_coms$name))
#   colScale = scale_colour_manual(name = "name", values = colors)
#   rm(plot_coms_clr)
#   flist_comuni[[class]] = NULL
#   clist_comuni[[class]] = NULL
#   pdf(paste0("Output/Plot/",folder_names[[class]],"/Smoothing_paper2_province.pdf"), width = 20, height = 10)
#   g = ggplot(plot_coms, mapping = aes(x = as.POSIXct( as.Date(x,origin = "1970-01-01")), y = dens, color = name) )+ colScale  +  ylim(0,0.02) +   geom_line(aes(alpha  = ifelse(name %in% c("Roma", "Milano", "Bergamo","Napoli"), 1, 1)), size = 1.2 )  +  geom_line(data = mean_coms, mapping=aes(x = as.POSIXct(as.Date(x,origin = "1970-01-01")), y = meandens,color = Year), color = "black",size = 1.4)+
#     scale_x_datetime(labels = time_format("%b")) + facet_grid(~Year) + theme_pubr(base_size = 20)  + rremove("legend")+ rremove("xlab") + rremove("ylab")  + labs_pubr(base_size = 20) + font("xy.text",size = 17) 
#   plot(g)
#   dev.off()
# }

######################################################## Regression plots ##############
rm(list = ls())
source("Scripts/Utility_Functions.R")
load("Output/Data/Aggregated_Provinces.Rdata")
load("Output/Data/Regression_Province.Rdata")


classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
class_labs = c("0-49 years", "50-69 years", "70+ years")
year_labs = paste0("20",11:20)
names(class_labs) =  classe
names(year_labs) = years
folder_names = c("0-49","50-69","70+")
names(folder_names) = classe
years_to_plot = years[7:10]



temp_res = errors_province
for (class in classe) for(year in years_to_plot) temp_res[[class]][[year]] = inv_clr(temp_res[[class]][[year]])

for (class in classe){
  plot_provs = fdata_to_ggplot_df(fdatalist = temp_res[[class]],names = provinces_names, years = c("T_17","T_18","T_19","T_20"), year_labs = year_labs[7:10] )
  colors = rep("gray",107)
  plot_provs$name = factor(plot_provs$name, levels = c(setdiff(plot_provs$name, c("Roma", "Milano", "Bergamo","Napoli") ), c("Roma", "Milano", "Bergamo","Napoli")))
  plot_provs_clr = fdata_to_ggplot_df(fdatalist = errors_province[[class]],names = provinces_names, years = c("T_17","T_18","T_19","T_20"), year_labs = year_labs[7:10] )
  plot_provs_clr$name = factor(plot_provs$name, levels = c(setdiff(plot_provs$name, c("Roma", "Milano", "Bergamo","Napoli") ), c("Roma", "Milano", "Bergamo","Napoli")))
  provinces_data_t = provinces_data %>% filter(CL_ETA == class)
  provinces_data_t = provinces_data_t %>% mutate(weight = num_residenti/sum(provinces_data$num_residenti)) %>% dplyr::select(provincia, weight)
  plot_provs_clr = plot_provs_clr %>% left_join(provinces_data_t, by = c("name" = "provincia"))
  plot_provs_clr =  plot_provs_clr %>% mutate(cl_dens = dens*weight)
  mean_provs = plot_provs_clr %>% group_by(x, Year) %>% summarise(mdens = mean(dens))
  Integrals = mean_provs %>% group_by(Year) %>% summarise(Integrals = int.simpson2(x = x,y = exp(mdens)))
  mean_provs = mean_provs %>% left_join(Integrals)
  mean_provs = mean_provs %>% group_by(Year,x) %>% mutate(meandens = exp(mdens)/Integrals)
  names(colors) = levels(factor(plot_provs$name))
  colScale = scale_colour_manual(name = "name", values = colors)
  lims = range(as.POSIXct( as.Date(plot_provs$x,origin = "1970-01-01")))
  g = ggplot(plot_provs, mapping = aes(x = as.POSIXct( as.Date(x,origin = "1970-01-01")), y = dens, color = name) )+ colScale  +  ylim(0,0.02) +   geom_line(aes(alpha  = ifelse(name %in% c("Roma", "Milano", "Bergamo","Napoli"), 1, 1)), size = 1.1 )  + geom_line(data = mean_provs, mapping=aes(x = as.POSIXct(as.Date(x,origin = "1970-01-01")), y = meandens,color = Year), color = "black",size = 1.4)+
    scale_x_datetime(labels = time_format("%b"),limits = lims) + facet_grid(~Year) + theme_pubr(base_size = 20)  + rremove("legend")+ rremove("xlab") + rremove("ylab")  + labs_pubr(base_size = 20) + font("xy.text",size = 17) 
  #x11()
  pdf(paste0("Output/Plot/",folder_names[[class]],"/Paper_res_regr.pdf"), width = 20, height = 10)
  
  plot(g)
  dev.off()
}


##### Residual Norms Plot ###############################

load("Output/Data/Province_geometry_polygons.Rdata")

for(class in classe)
{
  prov_poly_t = prov_poly
  for (year in years_to_plot){
    set.seed(170000)
    if (year == "T_20" & class != "0_10")
      cluster = try(kmeans.fd(errors_province[[class]][[year]], ncl = 3, max.iter = 200, draw = F)$cluster)
    else cluster = sample(c(1,2,3),107,replace = T)
    temp = data.frame(Provincia = provinces_names, residuals =int.simpson(errors_province[[class]][[year]]^2), cluster = cluster)
    temp$cluster = as.factor(temp$cluster)
    names(temp) = c("Provincia",year_labs[[year]], paste0("cluster",year))
    prov_poly_t@data = prov_poly_t@data %>% left_join(temp, by = c("DEN_PCM" = "Provincia"))
    print(year)
  }
  
  prov_poly_t1 = st_as_sf(prov_poly_t) %>% pivot_longer(starts_with("20"), names_to = "year", values_to = "res") %>% group_by(year)
  prov_poly_t1 = st_as_sf(prov_poly_t1)
  g = ggplot(prov_poly_t1) + geom_sf(aes(fill = log(res))) + scale_fill_viridis(option = "inferno")+ facet_wrap(.~ year, nrow = 1) +
    clean_theme()+theme_pubclean(base_size = 40) + labs_pubr(base_size = 40) + theme(legend.position="bottom") + rremove("xlab") + rremove("ylab") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks")  + labs(fill = "log(||res||)")
  pdf(paste0("Output/Plot/",folder_names[[class]],"/Paper_res_norm",".pdf"),width = 20,height = 10)
  #x11()
  plot(g)
  dev.off()
  prov_poly_t2 = st_as_sf(prov_poly_t) 
  colors = ifelse((prov_poly_t2 %>% arrange(DEN_PCM))$clusterT_20 == 2, "skyblue","salmon")
  colors[which((prov_poly_t2 %>% arrange(DEN_PCM))$clusterT_20 == 3)] = "green"
  colors_sc = c("salmon","skyblue","green")
  names(colors_sc) = levels(factor(prov_poly_t2$clusterT_20))
  colScale = scale_fill_manual(name = "cluster_T_20", values = colors_sc)
  g = ggplot(prov_poly_t2) + geom_sf(aes(fill = clusterT_20))+ colScale  + 
    clean_theme()+theme_pubclean(base_size = 40) + labs_pubr(base_size = 40)+ theme(legend.position = "none" ) + rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text")+ rremove("ticks")
  pdf(paste0("Output/Plot/",folder_names[[class]],"/res_clustering_map",".pdf"),width = 20,height = 10)
 # x11()
  plot(g)
  dev.off()
  
#  x11()
  pdf(paste0("Output/Plot/",folder_names[[class]],"/res_clustering_dens",".pdf"),width = 20,height = 10)
  par(cex = 1.3)
  plot(inv_clr(errors_province[[class]][[year]]), col = colors, ylim = c(0,0.02), lwd = 2, xaxt = "n", ylab = "",xlab ="", main = " " )
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b", xlab = "")
  dev.off()
}



############# Variogram plots
rm(list = ls())
source("Scripts/Utility_Functions.R")
library(fdagstat)
load("Output/Data/Variograms_Province.Rdata")
classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
class_labs = c("0-49 years", "50-69 years", "70+ years")
year_labs = paste0("20",11:20)
names(class_labs) =  classe
names(year_labs) = years
folder_names = c("0-49","50-69","70+")
names(folder_names) = classe
years_to_plot = years[7:10]
graphics.off()
for( class in classe){
  #x11()
  pdf(paste0("Output/Plot/",folder_names[[class]],"/Paper_variog_provs.pdf"),width = 30,height = 18)
  plotlist = list()
  for ( year in years_to_plot){
    plotlist[[year]] = plotVariogram(variograms_province[[class]][[year]],ggReturn = T) 
    plotlist[[year]] = plotlist[[year]] + ylim(4,16) + theme_pubr(base_size = 20)   + rremove("legend")+ rremove("xlab") + rremove("ylab")  + labs_pubr(base_size = 20) + font("xy.text",size = 17) 
  }
  plot(ggarrange(plotlist = plotlist,nrow = 2,ncol = 2))
  dev.off()
}





