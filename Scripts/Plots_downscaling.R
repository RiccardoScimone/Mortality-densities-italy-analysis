rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load(Rdata_path("downscales_predictions"))
load("Output/Data/Smoothing_datasets.Rdata")
load("Output/Data/Aggregated_Municipalities.Rdata")

class = "15_21"
aggrdate = 7 ## parameters for aggregating and computing wasserstein distances
ndays = 366

data_to_smooth_municipalities[is.na(data_to_smooth_municipalities)] = 0

### Now we focus on our class
data_municipalities_temp = data_to_smooth_municipalities %>% filter(CL_ETA == class)

#### Compute dates after aggregation
dum_freqs = (data_municipalities_temp %>% filter(COD_PROVCOM == municipalities_codes[1]))$T_20
dates = sort(unique(data_municipalities_temp$partial_date_death))

### compute aggregated dates
rolldates = roll(aggrdate,ndays,dum_freqs,dates)$rolleddates
### discretize densities
discr_density = discr_dens(predicted_municipalities_density[[class]],length(rolldates))

#### Prepare data frame with final results on wasserstein distances

summary = info
wass = rep(0,length(summary$NOME_PROVINCIA))
for ( i in 1:length(summary$NOME_PROVINCIA))
{
  freqs =  (data_municipalities_temp %>% filter(COD_PROVCOM == summary$COD_PROVCOM[i]))$T_20
  rolls = roll(aggrdate,ndays,freqs,dates)
  wass[i] = as.numeric(try(wasserstein1d(p = 2, a = as.numeric(rolls$rolleddates[which(rolls$rolledfreqs > 0)]), b = as.numeric(rolls$rolleddates),  wa =  rolls$rolledfreqs[which(rolls$rolledfreqs > 0)], wb = discr_density[i,])))
}

summary$wass = wass
summary$index = 1:7900
count_data = municipalities_aggregated %>% filter(CL_ETA == class) %>% group_by(COD_PROVCOM) %>% summarise(count = sum(T_20))


summary = summary%>% left_join(count_data) %>% filter(count > 0) %>% arrange(desc(wass))

summary = summary %>% left_join(municipality_data %>% filter(CL_ETA==class), by = c("COD_PROVCOM" = "istat"))
save(summary, file = Rdata_path("krig_comparison_result_downscaled"))


#### Comparison plots

graphics.off()
x11()
par(mfrow = c(1,3), cex = 1.3)
code = "16024"
index = which(municipalities_codes == code)
freqs = (data_to_smooth_municipalities %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
dates = as.numeric((data_to_smooth_municipalities %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
plot(predicted_municipalities_density$`15_21`[index], col = "red", ylim = c(0,0.02), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = "Bergamo Mun.")
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
rolls = roll(aggrdate,ndays,freqs,dates)
add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
lines(predicted_municipalities_density$`15_21`[index], col = "red",lwd = 2)

code = "98031"
index = which(municipalities_codes == code)
freqs = (data_to_smooth_municipalities %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
dates = as.numeric((data_to_smooth_municipalities %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
plot(predicted_municipalities_density$`15_21`[index], col = "red", ylim = c(0,0.02), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = "Lodi Mun.")
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
rolls = roll(aggrdate,ndays,freqs,dates)
add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
lines(predicted_municipalities_density$`15_21`[index], col = "red",lwd = 2)

code = "16077"
index = which(municipalities_codes == code)
freqs = (data_to_smooth_municipalities %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
dates = as.numeric((data_to_smooth_municipalities %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
plot(predicted_municipalities_density$`15_21`[index], col = "red", ylim = c(0,0.02), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = "Clusone Mun.")
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
rolls = roll(aggrdate,ndays,freqs,dates)
add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
lines(predicted_municipalities_density$`15_21`[index], col = "red",lwd = 2)


###########################################################
## Analysis on distances

load("Output/Data/krig_comparison_result_downscaled.Rdata")
load("Output/Data/Province_geometry_polygons.Rdata")
load("Output/Data/Comuni_geometry_polygons.Rdata")
prov_poly = st_as_sf(prov_poly)

data = summary %>% filter(num_residenti > 500) %>% dplyr::select(COD_PROVCOM,wass,count)
com_poly_t = com_poly
com_poly_t@data = com_poly@data %>% left_join(data, by = c("PRO_COM" = "COD_PROVCOM")) 
com_poly_t = com_poly_t[-which(is.na(com_poly_t@data[["wass"]])),]
com_poly_t$scaled = scale(log(com_poly_t$wass))
nb_com = poly2nb(com_poly_t,row.names = com_poly_t$PRO_COM)
listw = nb2listw(nb_com,zero.policy =T)
com_poly_t$lagged = lag.listw(listw,com_poly_t$scaled,zero.policy = T)
local = localmoran(log(com_poly_t$wass), listw,zero.policy = T)
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
Moran[Moran != "high-high"] = "Other"
colors[colors != "red"] = "white"
com_poly_t$Moran = Moran[findInterval(quadrant,brks,all.inside=FALSE)]
com_poly_t$dum = com_poly_t$COMUNE
prov_poly$dum = prov_poly$DEN_PCM
nudge_x = nudge_y = rep(0,107)
nudge_x[which(prov_poly$DEN_PCM %in% c("Alessandria","Lecco", "Cuneo","Bergamo", "Pavia", "Trento", "Bolzano/Bozen","Brescia"))] = c(-9,-3,-5,0,0,0, 3, 6 ) * 1e5
nudge_y[which(prov_poly$DEN_PCM %in% c("Alessandria","Lecco", "Cuneo","Bergamo", "Pavia", "Trento", "Bolzano/Bozen","Brescia"))] = c(3,0,-3,3,2,2, 5, 0 ) * 1e5

prov_poly$dum[which(! prov_poly$DEN_PCM %in% c("Alessandria","Lecco", "Cuneo","Pavia", "Trento", "Bolzano/Bozen", "Brescia") )] =NA
prov_poly$dum[which(prov_poly$DEN_PCM == "Bolzano/Bozen")] = "Bolzano"

#   prov_poly$dum[which(! prov_poly$DEN_PCM %in% c("Bergamo","Bolzano/Bozen", "Brescia", "Trento") )] =NA
com_poly_t$dum[com_poly_t$Moran != "high-high" | ! com_poly_t$COMUNE %in% c("Ortisei","Galbiate","San Pellegrino Terme","Tortona", "MondovÃ¬","Edolo")] = NA
com_poly_t$dum[com_poly_t$COMUNE == "MondovÃ¬"] = "Mondovì"


g = ggplot(com_poly_t) + geom_sf(aes(fill = Moran), color = NA)  + scale_fill_manual(name = "",values = colors,breaks = c("high-high")) +   geom_sf(data = prov_poly, fill = NA) + theme_pubr(base_size = 20)+ theme(legend.position = "none")  +
  #   ggrepel::geom_label_repel(aes(label = dum,geometry = geometry), size = 3, stat = "sf_coordinates",max.overlaps = 3, fontface = "bold", force = 50, segment.size = 0.8, seed = 10000, min.segment.length = 0) +
  ggrepel::geom_text_repel(data = prov_poly, aes(label = dum,geometry = geometry), size = 6.5, stat = "sf_coordinates",max.overlaps = 1, fontface = "bold",force = 30, col = "purple", nudge_x = nudge_x, nudge_y = nudge_y, seed = 1000000) +
  rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") 
x11()
plot(g)
#  dev.off()
com_ind = which(com_poly_t$COD_REG%in% as.character(c(1,2,7,3,4,5,6,8)))
prov_ind =  which(prov_poly$COD_REG%in% as.character(c(1,2,7,3,4,5,6,8)))
com_poly_t = com_poly_t %>% filter(COD_REG %in% as.character(c(1,2,7,3,4,5,6,8)))
prov_poly = prov_poly %>% filter(COD_REG %in% as.character(c(1,2,7,3,4,5,6,8)))
nudge_x = nudge_x[prov_ind]
nudge_y = nudge_y[prov_ind]
g = ggplot(com_poly_t) + geom_sf(aes(fill = Moran), color = NA)  + scale_fill_manual(name = "",values = colors,breaks = c("high-high")) +   geom_sf(data = prov_poly, fill = NA) + theme_pubr(base_size = 20)+ theme(legend.key = element_rect(color="black"))+ theme(legend.key = element_rect(color="black"),legend.key.size = unit(3,"line"),legend.text=element_text(size=25))  +
  ggrepel::geom_label_repel(aes(label = dum,geometry = geometry), size = 8, stat = "sf_coordinates",max.overlaps = 3, fontface = "bold", force = 50, segment.size = 0.8, seed = 10000, min.segment.length = 0) +
  ggrepel::geom_text_repel(data = prov_poly, aes(label = dum,geometry = geometry), size = 11, stat = "sf_coordinates",max.overlaps = 2, fontface = "bold",force = 30, col = "purple", nudge_x = nudge_x, nudge_y = nudge_y, seed = 1000000) +
  rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") 
x11()
plot(g)


load("Output/Data/incriminati.Rdata")
x11()
par(mfrow = c(2,3),cex = 1.3, mar = c(2,2,2,2))
for ( code in (a %>% filter(COMUNE %in% c("Tortona", "Ortisei", "Edolo", "San Pellegrino Terme", "MondovÃ¬", "Galbiate") ))$PRO_COM)
{  
  title = a$COMUNE[a$PRO_COM == code]
  if(title == "MondovÃ¬") title = "Mondovì"
  index = municipalities_codes == code
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = title)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
  
}

