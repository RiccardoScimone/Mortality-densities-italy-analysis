### We perform Lisa analysis on total deaths / residents ratios by age, first by
### province and then by comuni. The following code both perform the analysis and
### saves the plots in the form of ggplot objects. If you need the numeric results, 
### just save  the corresponding data frame. Plots are saved for all classes and years.
rm(list = ls())
source("Scripts/Utility_Functions.R")
load("Output/Data/Municipality_data_scalar.Rdata")
load("Output/Data/Province_data_scalar.Rdata")
load("Output/Data/Comuni_geometry_polygons.Rdata")
load("Output/Data/Province_geometry_polygons.Rdata")


classe = unique(provinces_aggregated_lisa$CL_ETA) ## age classes
years = paste0("T_",11:20)

plot_list_provinces_heatmaps = list()
plot_list_provinces_moran = list()
colorlim = list(c(0,2e-3),c(0.0035,0.0075),c(.04,.085)) ## limits for the colorscale in heatmaps
names(colorlim) = classe
class_labs = c("0 - 49 years", "50 - 69 years", "70 years +") ## labels for each age classes
year_labs = paste0("20",11:20)
names(class_labs) =  classe
names(year_labs) = years
dum_label = function(names) ## manage labels
{
  temp = rep("", length(names))
  temp[which(names %in% c("Bergamo","Cremona","Lodi","Arezzo") )] = names[which(names %in% c("Bergamo","Cremona","Lodi","Arezzo") )]
  return(temp)
}
for(class in classe){
  plot_list_provinces_heatmaps[[class]] = list()
  plot_list_provinces_moran[[class]] = list()
  for(year in years){ ### Plot the index and perform LISA analysis
    data = provinces_aggregated_lisa %>% filter(CL_ETA == class) %>% dplyr::select(NOME_PROVINCIA,one_of(year))
    prov_poly_t = prov_poly
    prov_poly_t@data = prov_poly@data %>% left_join(data, by = c("DEN_PCM" = "NOME_PROVINCIA")) 
    nb_prov = poly2nb(prov_poly_t,row.names = prov_poly_t$DEN_PCM)
    listw = nb2listw(nb_prov)
    local = localmoran(prov_poly_t[[year]], listw)
    prov_poly_t$scaled = scale(prov_poly_t[[year]])
    prov_poly_t$lagged = lag.listw(listw,prov_poly_t$scaled)
    prov_poly_t = st_as_sf(prov_poly_t)
    moran.map = cbind(prov_poly, local)
    quadrant = vector(mode = "numeric", length = nrow(local))
    signif = 0.1
    quadrant[prov_poly_t$scaled >0 & prov_poly_t$lagged>0] <- 4  
    quadrant[prov_poly_t$scaled <0 & prov_poly_t$lagged<0] <- 1      
    quadrant[prov_poly_t$scaled <0 &  prov_poly_t$lagged>0] <- 2
    quadrant[prov_poly_t$scaled >0 & prov_poly_t$lagged<0] <- 3
    quadrant[local[,5]>signif] <- 0 
    brks <- c(0,1,2,3,4)
    Moran = c("insignificant","low-low","low-high","high-low", "high-high")
    colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
    names(colors) = Moran
    prov_poly_t$Moran = Moran[findInterval(quadrant,brks,all.inside=FALSE)]
    g = ggplot(prov_poly_t) + geom_sf(aes(fill = Moran))  + ggrepel::geom_text_repel(aes(label = dum_label(DEN_PCM),geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20) + 
      scale_fill_manual(name = "",values = colors)+ theme_pubr() + theme(text = element_text(size=30),legend.key.size = unit(3,"line") )  + rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") 
    plot_list_provinces_heatmaps[[class]][[year]] = g
    g = ggplot(prov_poly_t) + geom_sf(aes_string(fill = year)) + ggtitle(paste0(year_labs[[year]])) + 
      scale_fill_viridis(option = "inferno",limits = colorlim[[class]]) + theme_pubr() +theme(text = element_text(size=30),legend.key.size = unit(3,"line") ) + labs(fill = "deaths/residents") + rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text")+ rremove("ticks")  
    plot_list_provinces_moran[[class]][[year]] = g
  }
}
save( plot_list_provinces_heatmaps,  plot_list_provinces_moran, file = Rdata_path("Exploratory_plots_provinces"))





#### Repeat everything at Comuni Level, only Moran index
load("Output/Data/Province_geometry_polygons.Rdata")
it_poly = st_union(st_as_sf(prov_poly)) ## Italy boundaries

plot_list_municipalities_moran = list()


for(class in classe){
  plot_list_municipalities_moran[[class]] = list()
  for(year in years){
    data = municipalities_aggregated_lisa %>% filter(CL_ETA == class,num_residenti > 500) %>% dplyr::select(COD_PROVCOM,one_of(year))
    com_poly_t = com_poly
    com_poly_t@data = com_poly@data %>% left_join(data, by = c("PRO_COM" = "COD_PROVCOM")) 
    com_poly_t = com_poly_t[-which(is.na(com_poly_t@data[[year]])),]
    com_poly_t$scaled = scale(com_poly_t[[year]])
    nb_com = poly2nb(com_poly_t,row.names = com_poly_t$PRO_COM)
    listw = nb2listw(nb_com,zero.policy =T)
    com_poly_t$lagged = lag.listw(listw,com_poly_t$scaled,zero.policy = T)
    local = localmoran(com_poly_t[[year]], listw,zero.policy = T)
    com_poly_t = st_as_sf(com_poly_t)
    moran.map = cbind(com_poly_t, local)
    quadrant = vector(mode = "numeric", length = nrow(local))
    signif = 0.1
    quadrant[com_poly_t$scaled >0 & com_poly_t$lagged>0] <- 4  
    quadrant[com_poly_t$scaled <0 & com_poly_t$lagged<0] <- 1      
    quadrant[com_poly_t$scaled <0 & com_poly_t$lagged>0] <- 2
    quadrant[com_poly_t$scaled >0 & com_poly_t$lagged<0] <- 3
    quadrant[local[,5]>signif] <- 0 
    brks <- c(0,1,2,3,4)
    Moran = c("insignificant","low-low","low-high","high-low","high-high")
    colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
    names(colors) = Moran
    com_poly_t$Moran = Moran[findInterval(quadrant,brks,all.inside=FALSE)]
    g = ggplot(com_poly_t) + geom_sf(aes(fill = Moran), color = NA) + 
      #ggtitle(paste0(year_labs[[year]],"_",class_labs[[class]])) +
      scale_fill_manual(name = "",values = colors) + theme_pubr() +theme(text = element_text(size=20)) + geom_sf(data = it_poly, fill = NA) + rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") 
    plot_list_municipalities_moran[[class]][[year]] = g
  }
}

save(plot_list_municipalities_moran, file = Rdata_path("Exploratory_plots_municipalities"))


