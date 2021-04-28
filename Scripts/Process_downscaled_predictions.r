rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load(Rdata_path("downscales_predictions"))
load("Output/Data/dataset_aggregato_comunale.Rdata")
load("Ordered keys")
load("Output/Data/modified_istat_dataset.Rdata")
load(Rdata_path("index"))
################ Produce comparison with raw jump data, Bergamo, Lecco  ############################
class = "15_21"
trunc_left = as.Date("2020-01-01")
trunc_right = as.Date("2020-12-31")
data_to_smooth = com_df_aggr %>% filter(partial_date_death <= trunc_right & partial_date_death >= trunc_left)
data_to_smooth = data_to_smooth %>% tidyr::complete (COD_PROVCOM,partial_date_death,CL_ETA) 
data_to_smooth[is.na(data_to_smooth)] = 0
data_to_smooth$CL_ETA = factor(data_to_smooth$CL_ETA, levels = c("0_10","11_14","15_21")) 
data_to_smooth = data_to_smooth %>% group_by(COD_PROVCOM,CL_ETA) %>% dplyr::select(-starts_with("F")) %>%  dplyr::select(-starts_with("M"))
data_to_smooth = data_to_smooth %>% mutate_at(.vars = paste0("T_",11:20),.funs = function(x){return(x/sum(x))})
data_to_smooth_comuni = data_to_smooth
aggrdate = 7
ndays = 366

predicted_comuni_distribution = list()
com_ov70= list()
summary = list()
classe = "15_21"

for (class in classe){
  newvals = 18262:18627
#  predicted_comuni_distribution[[class]] = Distribution_Function(predicted_comuni_density[[class]],newvals)
  com_ov70[[class]] = com_df_aggr %>% filter(CL_ETA == class) %>% dplyr::select(COD_PROVCOM,T_20,partial_date_death) %>% tidyr::complete(COD_PROVCOM,partial_date_death,fill = list(T_20 = 0))%>% group_by(COD_PROVCOM) %>% mutate(cum = cumsum(T_20)/sum(T_20))
  com_ov70[[class]]$partial_date_death = as.numeric(com_ov70[[class]]$partial_date_death)
  com_ov70[[class]] = com_ov70[[class]] %>% filter(COD_PROVCOM %in% ordered_codes_comuni) %>% filter(!is.nan(cum)) %>% group_split()
  pre_cum = NULL
  cl = 1:length(com_ov70)
  name = rep(" ",length(com_ov70[[class]]))
  wass = rep(0,length(com_ov70[[class]]))
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == ordered_codes_comuni[1]))$partial_date_death)
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == ordered_codes_comuni[1]))$T_20
  rolldates = roll(aggrdate,ndays,freqs,dates)$rolleddates
  discr_density = discr_dens(predicted_comuni_density[[class]],length(rolldates))
  for ( i in 1:length(com_ov70[[class]]))
  { 
 #   index[i] = which(ordered_codes_comuni == com_ov70[[class]][[i]]$COD_PROVCOM[1])
    cl[i] = infr$cluster[index[i]]
    name[i] = ordered_codes_comuni[index[i]]
 #   temp = as.vector(predicted_comuni_distribution[[class]]$data[index[i],])
 #   pre_cum = c(pre_cum,temp)
    freqs = com_ov70[[class]][[i]]$T_20
    rolls = roll(aggrdate,ndays,freqs,dates)
    wass[i] = as.numeric( try(wasserstein1d(p = 2, a = rolls$rolleddates[which(rolls$rolledfreqs > 0)], b = rolls$rolleddates,  wa =  rolls$rolledfreqs[which(rolls$rolledfreqs > 0)], wb = discr_density[index[i],])))
  }
  com_ov70[[class]]= bind_rows(com_ov70[[class]])
 # com_ov70[[class]]$pre_cum = pre_cum
  summary[[class]] = com_ov70[[class]]%>% group_by(COD_PROVCOM) %>% summarise(count = sum(T_20)) 
  summary[[class]]$cl = cl
  summary[[class]]$name = name
  summary[[class]]$index = index
  summary[[class]]$wass = wass
  summary[[class]] = summary[[class]] %>% filter(count > 0)
 # vec = rep(0,i)
 # for ( i in 1:length(vec)){
 #   res = try (cont_ks_cdf(summary[[class]]$KS[i],summary[[class]]$count[i]))
 #   if (class(res) != "try-error")
 #     vec[i] = res
 #   else
 #     vec[i] = 0.5
 # }
  
#  summary[[class]]$pval = 1-vec
  
}

save(summary, com_ov70, file = Rdata_path("krig_comparison_result_downscaled"))

load("Output/Data/krig_comparison_result.Rdata")

graphics.off()
x11()
par(mfrow = c(1,3), cex = 1.3)
code = "16024"
codes = unique(data_to_smooth_comuni$COD_PROVCOM)
index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.02), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = "Bergamo Mun.")
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
rolls = roll(aggrdate,ndays,freqs,dates)
add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)

code = "98031"
codes = unique(data_to_smooth_comuni$COD_PROVCOM)
index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.02), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = "Lodi Mun.")
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
rolls = roll(aggrdate,ndays,freqs,dates)
add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)

x11()
code = "17195"
codes = unique(data_to_smooth_comuni$COD_PROVCOM)
index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.02), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = "Clusone Mun.")
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
rolls = roll(aggrdate,ndays,freqs,dates)
add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
#############################################################

####################################################################################################

set.seed(3400)
samp = sample(7867,1500)
rank = list()
for ( class in classe){
  col = rep(" ", dim(summary[[class]])[1])
  index = summary[[class]]$index
  cl = summary[[class]]$cl
  col[which(cl == 1)] = "salmon"
  col[which(cl == 2)] = "skyblue"
  col[which(cl == 3)] = "green"
#  x11()
#  pdf( paste0("Output/Plot/kriging_density_downscaled",class,".pdf"), height  = 10, width = 20 )
#  par(cex = 1.3)
#  plot(predicted_comuni_density[[class]][index[samp]], col = col[samp], xaxt = "n", ylab = "", xlab = "", main = "Predictions on Municipalities")
#  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
#  legend("topleft", legend = c("critical", "mild", "absent"), fill = c("salmon","skyblue","green"))
#  dev.off()
#  rank[[class]] = summary[[class]] %>% filter ( count > 100) %>% arrange(desc(wass),count)
}

#save(rank,com_ov70, summary, file = Rdata_path("ranking_comuni"))

#load("Output/Data/ranking_comuni.Rdata")
#load("Output/Data/krig_comparison_result_downscaled.Rdata")

classe = "15_21"
rank = list()
for ( class in classe){
  col = rep(" ", dim(summary[[class]])[1])
  index = summary[[class]]$index
  cl = summary[[class]]$cl
  col[which(cl == 1)] = "salmon"
  col[which(cl == 2)] = "skyblue"
  col[which(cl == 3)] = "green"
#  png( paste0("Output/Plot/Downscaled_kriging_density_",class,".png"), height  = 10, width = 20, units = "in", res = 1200 )
#  par(cex = 1.5)
#  plot(predicted_comuni_density[[class]][index], col = col, xaxt = "n")
#  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months"), format="%m")
#  legend("topleft", legend = c("critical", "mild", "absent"), fill = c("salmon","skyblue","green"))
#  dev.off()
  
  # 
  #   KSplot = ggplot(summary[[class]]) +geom_boxplot(mapping = aes(x = as.factor(cl), y = KS, fill = as.factor(cl)))
  #   x11()
  #   plot(KSplot)
  #   PVplot = summary[[class]] %>% filter(count > 100, pval < 0.1) %>% ggplot() +geom_violin(mapping = aes(x = as.factor(cl), y = pval, fill = as.factor(cl))) + labs(x = "Cluster", fill = "cluster", cex = 2) + theme_pubr(base_size = 20)
  #   x11()
  #   plot(PVplot)
  #   sum1 = summary[[class]] %>% filter (pval < 0.1, count > 100) %>% arrange(desc(KS),pval,count)
  # 
  #   x11()
  #   par(cex = 1.5)
  #   hist(sqrt(summary[[class]]$count)*summary[[class]]$KS, col = "gold", breaks = seq(0,7, by = 0.1), prob = T, ylim = c(0,1.75), main = bquote("distribution of" ~ sqrt(N[k])*D[k]), xlab = bquote(sqrt(N[k])*D[k]))
  #   newvals = seq(0.2,6 , by = 0.01)
  #   lines(newvals, Kolm_density(vals = newvals))
  
  rank[[class]] = summary[[class]]  %>% arrange(desc(wass),count)
  rank[[class]]$cl = as.character(rank[[class]]$cl)
  rank[[class]] = rank[[class]] %>% left_join(dplyr::select(modified_date_dataset %>% distinct(COD_PROVCOM, .keep_all = TRUE), COD_PROVCOM ,NOME_COMUNE ))
}

save(rank,com_ov70, summary, file = Rdata_path("ranking_comuni_downscaled"))




############################################################################################
load("Output/Data/ranking_comuni_downscaled.Rdata")
for (class in classe){
  rank[[class]] = rank[[class]] %>% filter (count >0)
  rank[[class]]$cl = as.character(rank[[class]]$cl)
  rank[[class]]$cl = factor( rank[[class]]$cl, levels = c("1","3","2"))
  rank[[class]] = rank[[class]] %>% left_join(dplyr::select(modified_date_dataset %>% distinct(COD_PROVCOM, .keep_all = TRUE), COD_PROVCOM ,NOME_COMUNE ))
 # quantwass = quantile(rank[[class]]$wass,0)
 # rank[[class]] = rank[[class]] %>% filter(wass > quantwass)
  mycolors = c("salmon","skyblue","green")
  names(mycolors) = c("1","2","3")
  rankplot = ggplot(rank[[class]], mapping = aes(y = wass, x = cl,color = cl, label = NOME_COMUNE)) + #geom_boxplot() +
   scale_color_manual(name = "cl", values = mycolors)  + theme_pubr(base_size = 20)  + labs_pubr(base_size = 20) + font("xy.text",size = 17) + ylab("Wasserstein Distance")+
    rremove("legend")+ rremove("xlab") + rremove("x.text") + geom_text(check_overlap = TRUE,position=position_jitter(width=0.30),  size=8) 
 # pdf(paste0("Output/Plot/Rankplot_downscaled_",class,".pdf"), height = 10, width = 20)
  x11()
  plot(rankplot)
#  dev.off()
  
  # pdf(paste0("Output/Plot/Rankplot_Distr1_downscaled_",class,".pdf"), height = 15, width = 15)
  # par(mfrow = c(2,3))
  # tot = 36
  # for (i in 1:tot)
  # {
  #   tmp = rank[[class]][i,] 
  #   ecdf = (com_ov70[[class]] %>% filter(COD_PROVCOM == tmp$COD_PROVCOM))
  #   ecdf_t = ecdf$partial_date_death
  #   ecdf = ecdf$cum
  #   plot(predicted_comuni_distribution[[class]][tmp$index], col = "red", main = tmp$NOME_COMUNE, ylim = c(0,1))
  #   lines(ecdf_t,ecdf, type = "l", col = "blue")
  #   legend("topleft",legend = c("empirical", "kriged"), fill = c("blue", "red"))
  #   if ( i %% 6 == 0 & i < tot)
  #   {
  #     dev.off()
  #     pdf(paste0("Output/Plot/Rankplot_Distr",ceiling((i+1)/6),"_downscaled_",class,".pdf"), height = 15, width = 15)
  #     par(mfrow = c(2,3))
  #   }
  # }
 # dev.off()
  
}
data_comuni = data_comuni %>% filter(CL_ETA == class)
rank[[class]] = rank[[class]] %>% left_join(data_comuni, by = c("COD_PROVCOM" = "istat" ))
plot(log(rank$`15_21`$num_residenti),log(rank$`15_21`$wass))
x11()
plot(rank$`15_21`$count,rank$`15_21`$wass)
codes = rank$`15_21`$COD_PROVCOM[1:3]
names = rank$`15_21`$NOME_COMUNE[1:3]
index =  rank$`15_21`$index[1:3]
x11()
par(mfrow = c(1,3),cex = 1.3)

for ( i in 1:3){
  code = codes[i]
  name = names[i]
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index[i]], col = "red", ylim = c(0,0.02), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = names[i])
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index[i]], col = "red",lwd = 2)
}

######## Last analysis ###############
load("Output/Data/ranking_comuni_downscaled.Rdata")
load("Output/Data/Province_geometry_polygons.Rdata")
it_poly = st_union(st_as_sf(prov_poly))
load("Output/Data/Comuni_geometry_polygons.Rdata")
plist = list()
plist2 = list()
prov_poly = st_as_sf(prov_poly)

for(class in classe){
  plist[[class]] = list()
  plist2[[class]] = list()
     
    data = rank[[class]] %>% filter(num_residenti > 500) %>% dplyr::select(COD_PROVCOM,wass,count)
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
    nudge_x[which(prov_poly$DEN_PCM %in% c("Alessandria","Lecco", "Cuneo","Bergamo", "Pavia", "Trento", "Bolzano/Bozen","Brescia"))] = c(-6,-3,-5,0,0,0, 3, 6 ) * 1e5
    nudge_y[which(prov_poly$DEN_PCM %in% c("Alessandria","Lecco", "Cuneo","Bergamo", "Pavia", "Trento", "Bolzano/Bozen","Brescia"))] = c(3,0,2,3,2,2, 5, 0 ) * 1e5
    
    prov_poly$dum[which(! prov_poly$DEN_PCM %in% c("Alessandria","Lecco", "Cuneo","Pavia", "Trento", "Bolzano/Bozen", "Brescia") )] =NA
 #   prov_poly$dum[which(! prov_poly$DEN_PCM %in% c("Bergamo","Bolzano/Bozen", "Brescia", "Trento") )] =NA
    com_poly_t$dum[com_poly_t$Moran != "high-high" | ! com_poly_t$COMUNE %in% c("Ortisei","Galbiate","San Pellegrino Terme","Tortona", "MondovÃ¬","Edolo")] = NA
    com_poly_t$dum[com_poly_t$COMUNE == "MondovÃ¬"] = "Mondovì"
    
    g = ggplot(com_poly_t) + geom_sf(aes(fill = Moran), color = NA)  + scale_fill_manual(name = "",values = colors) +   geom_sf(data = prov_poly, fill = NA) + theme_pubr(base_size = 20)+ theme(legend.key = element_rect(color="black"),legend.key.size = unit(2.5,"line"))  +
   #   ggrepel::geom_label_repel(aes(label = dum,geometry = geometry), size = 3, stat = "sf_coordinates",max.overlaps = 3, fontface = "bold", force = 50, segment.size = 0.8, seed = 10000, min.segment.length = 0) +
      ggrepel::geom_text_repel(data = prov_poly, aes(label = dum,geometry = geometry), size = 6.5, stat = "sf_coordinates",max.overlaps = 1, fontface = "bold",force = 10, col = "purple", nudge_x = nudge_x, nudge_y = nudge_y, seed = 1000000) +
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
     g = ggplot(com_poly_t) + geom_sf(aes(fill = Moran), color = NA)  + scale_fill_manual(name = "",values = colors) +   geom_sf(data = prov_poly, fill = NA) + theme_pubr(base_size = 20)+ theme(legend.key = element_rect(color="black"))  +
         ggrepel::geom_label_repel(aes(label = dum,geometry = geometry), size = 8, stat = "sf_coordinates",max.overlaps = 3, fontface = "bold", force = 50, segment.size = 0.8, seed = 10000, min.segment.length = 0) +
      ggrepel::geom_text_repel(data = prov_poly, aes(label = dum,geometry = geometry), size = 11, stat = "sf_coordinates",max.overlaps = 2, fontface = "bold",force = 10, col = "purple", nudge_x = nudge_x, nudge_y = nudge_y, seed = 1000000) +
      rremove("axis") + rremove("axis") + rremove("xy.title") + rremove("axis.text") + rremove("ticks") 
    x11()
    plot(g)
    plist[[class]][[year]] = g
    print(year)
  }

load("incriminati.Rdata")
library(zoo)
library(plotfunctions)
graphics.off()
x11()
par(mfrow = c(2,3),cex = 1.3, mar = c(2,2,2,2))
for ( code in (a %>% filter(COMUNE %in% c("Tortona", "Ortisei", "Edolo", "San Pellegrino Terme", "MondovÃ¬", "Galbiate") ))$PRO_COM)
{  
  title = a$COMUNE[a$PRO_COM == code]
  if(title == "MondovÃ¬") title = "Mondovì"
  index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = title)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
  
}

# x11()
# par( cex = 1.3)
# code = "13254"
# codes = unique(data_to_smooth_comuni$COD_PROVCOM)
# index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
# freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
# dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
# plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.015), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = "Tortona")
# axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
# rolls = roll(aggrdate,ndays,freqs,dates)
# add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
# lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
# 
# code = "4130"
# codes = unique(data_to_smooth_comuni$COD_PROVCOM)
# index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
# freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
# dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
# plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.015), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = "Mondovì")
# axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
# rolls = roll(aggrdate,ndays,freqs,dates)
# add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
# lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
# 
# code = "97036"
# codes = unique(data_to_smooth_comuni$COD_PROVCOM)
# index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
# freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
# dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
# plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.015), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = "Galbiate")
# axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
# rolls = roll(aggrdate,ndays,freqs,dates)
# add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
# lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)


x11()
par(mfrow = c(3,5))
for ( code in (a %>% filter(DEN_PCM == "Brescia"))$PRO_COM)
{  
  title = a$COMUNE[a$PRO_COM == code]
  index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = title)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
  
}

x11()
par(mfrow = c(3,3))
for ( code in (a %>% filter(DEN_PCM == "Bolzano/Bozen"))$PRO_COM)
{  
  title = a$COMUNE[a$PRO_COM == code]
  index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = title)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
  
}

x11()
par(mfrow = c(1,5))
for ( code in (a %>% filter(DEN_PCM == "Bergamo"))$PRO_COM)
{  
  title = a$COMUNE[a$PRO_COM == code]
  index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = title)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
  
}

x11()
par(mfrow = c(1,5))
for ( code in (a %>% filter(DEN_PCM == "Lecco"))$PRO_COM)
{  
  title = a$COMUNE[a$PRO_COM == code]
  index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = title)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
  
}

x11()
par(mfrow = c(2,3))
for ( code in (a %>% filter(DEN_PCM == "Novara"))$PRO_COM)
{  
  title = a$COMUNE[a$PRO_COM == code]
  index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = title)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
  
}

x11()
par(mfrow = c(2,3))
for ( code in (a %>% filter(DEN_PCM == "Verona"))$PRO_COM)
{  
  title = a$COMUNE[a$PRO_COM == code]
  index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = title)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
  
}

x11()
par(mfrow = c(1,2))
for ( code in (a %>% filter(DEN_PCM == "Perugia"))$PRO_COM)
{  
  title = a$COMUNE[a$PRO_COM == code]
  index = summary$`15_21`$index[which(summary$`15_21`$COD_PROVCOM == code)]
  freqs = (data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$T_20
  dates = as.numeric((data_to_smooth_comuni %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death)
  plot(predicted_comuni_density$`15_21`[index], col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n", ylab = "",xlab ="", main = title)
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  rolls = roll(aggrdate,ndays,freqs,dates)
  add_bars(x = rolls$rolleddates,y = rolls$rolledfreqs/aggrdate, width = 0.9*aggrdate,col = alpha("grey"))
  lines(predicted_comuni_density$`15_21`[index], col = "red",lwd = 2)
  
}


