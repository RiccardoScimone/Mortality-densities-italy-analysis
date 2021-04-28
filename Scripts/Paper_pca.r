rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/Variograms_Comuni.Rdata")
load("Output/Data/Variograms_Province.Rdata")
load("Output/Data/Smoothing_Province 52.pdf.Rdata")
load("Output/Data/Province_geometry_polygons.Rdata")
load("Output/Data/Comuni_geometry_polygons.Rdata")
load("Ordered keys")
load("Output/Data/Residuals.Rdata")
library(gstat)

pca_province = list()
variogram_scores = list()

prov_poly = st_as_sf(prov_poly)
com_poly = st_as_sf(com_poly)

prov_poly = prov_poly %>% arrange(DEN_PCM)
com_poly = com_poly %>% arrange(PRO_COM)

year = "T_20"
select = 5
classe = c("0_10","11_14","15_21")
for (class in classe) pca_province[[class]] = fdata2pc(True_Residuals_province[[class]][[year]],ncomp = 107)

reconstruct = part_reconstruct = True_Residuals_province[[class]][[year]]

for ( i in 1:107) {
  reconstruct$data[i,] = pca_province[[class]]$mean$data
  part_reconstruct$data[i,] = pca_province[[class]]$mean$data
  for ( k in 1:select){
   reconstruct$data[i,] = reconstruct$data[i,] + pca_province[[class]]$x[i,k] * pca_province[[class]]$rotation$data[k,]
  }
}

x11()
par(mfrow = c(1,2))
plot(inv_clr(reconstruct), col ="grey", xaxt = "n", ylim = c(0,0.02))
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="3 months"), format="%b")
plot(inv_clr(True_Residuals_province[[class]][[year]]), col = "grey", xaxt = "n", ylim = c(0,0.02))
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="3 months"), format="%b")

### Plot perturbations #####
x11()
par(mfrow = c(1,3), cex = 1.5,mai = c(1.2, 0.5, 0.3, 0.3))
totvar = sum(diag(cov(pca_province[[class]]$x)))
for ( k in 1:3)
{
  sdev = sqrt(var(pca_province[[class]]$x[,k]))
  perc = round(sdev^2/totvar*100,1)
  mean = pca_province[[class]]$mean
  plot(inv_clr(mean), type = "l", main = paste0("PC ",k), xaxt = "n", ylim = c(0,0.0125), col = "black", ylab = " ",xlab = paste0(perc,"% explained variance"))
  axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2021-01-01"), by="3 months"), format="%b")
  lines(inv_clr(mean + 3*sdev*pca_province[[class]]$rotation[k]), type = "l", col = "red")
  lines(inv_clr(mean - 3*sdev*pca_province[[class]]$rotation[k]), type = "l", col = "blue")
}

library(ggfortify)
scores_df = data.frame(pca_province[[class]]$x[,1:3])

scores_df$names = ordered_names_province
scores_df = tibble(scores_df)
names(scores_df) = c("PC_1", "PC_2", "PC_3", "name")
g1 = ggplot(scores_df, aes(x = PC_1, y = PC_2)) + geom_point(size = 3) + geom_text_repel(aes(label = name),size = 8)  + coord_fixed() + geom_hline(yintercept = 0, col = "red") + geom_vline(xintercept = 0, col = "red") +xlab("PC 1") + ylab("PC 2")+ theme_pubr(base_size = 25) + labs_pubr(base_size = 25)
g2 = ggplot(scores_df, aes(x = PC_1, y = PC_3)) + geom_text(aes(label = name), size = 6) + theme_pubr(base_size = 20) + labs_pubr(base_size = 20) 

x11()
plot(g1)
p_poly = st_as_sf(prov_poly)
p_poly = p_poly %>% left_join(scores_df, by = c("DEN_PCM" = "name"))
g1 = p_poly %>% ggplot() + geom_sf(aes(fill = PC_1)) + scale_fill_viridis(option = "inferno") + 
  ggtitle("PC 1")  + clean_theme()+theme_pubclean(base_size = 20) + labs_pubr(base_size = 20) + theme(legend.position="bottom") + rremove("xlab") + rremove("ylab") + rremove("xy.text") + rremove("ticks") + rremove("legend.title")
g2 = p_poly %>% ggplot() + geom_sf(aes(fill = PC_2)) + scale_fill_viridis(option = "inferno") + 
  ggtitle("PC 2")  + clean_theme()+theme_pubclean(base_size = 20) + labs_pubr(base_size = 20) + theme(legend.position="bottom") + rremove("xlab") + rremove("ylab") + rremove("xy.text") + rremove("ticks") + rremove("legend.title")
x11()
ggarrange(g1,g2)


data = data.frame(id = ordered_names_province,x = train_coords_province[,1],y = train_coords_province[,2])

for ( i in 1:select)
  data = cbind(data,pca_province[[class]]$x[,i])
names(data) = c("id","x","y", paste0("PC",1:select))
coordinates(data) = ~ x +y 

  vario1 = variogram(PC1 ~ 1, data = data,cutoff = 500,width = 500/40)
  x11()
  plot(vario1)
  fit1 = vgm(model = "Exp",psill = mean(vario1$gamma), range = 300, nugget = 0)
  fit1 = fit.variogram(vario1,fit1,fit.method = 2)
  plot(vario1,fit1, ylim = c(0,max(vario1$gamma)))
  vario2 = variogram(PC2 ~ 1, data = data,cutoff = 500,width = 500/40)
  x11()
  plot(vario2)
  fit2 = vgm(model = "Exp",psill = mean(vario2$gamma), range = 300, nugget = 0)
  fit2 = fit.variogram(vario2,fit2,fit.method = 2)
  plot(vario2,fit2, ylim = c(0,max(vario2$gamma)))

  vario3 = variogram(PC3 ~ 1, data = data,cutoff = 500,width = 500/40)
  x11()
  plot(vario3)
  fit3 = vgm(model = "Exp", nugget = 0.8*mean(vario3$gamma),psill = 0.1, range = 20)
  fit3 = fit.variogram(vario3,fit3,fit.method = 2)
  plot(vario3,fit3, ylim = c(0,max(vario3$gamma)))

  g = gstat(NULL, id = "PC1", PC1 ~ 1, data, model = fit1)
  g = gstat(g, id = "PC2", PC2 ~ 1,data , model = vgm(model = "Exp",psill = mean(vario2$gamma), range = 300, nugget = 0))
  g = gstat(g, id = "PC3", PC3 ~ 1, data, model = fit3 )
  v = variogram(g,cutoff = 500,width = 500/40)
  g = gstat(g, id = c("PC1","PC2"), model = vgm(model = "Exp",psill = 0.5, range = 200, nugget = -2))
  g = gstat(g, id = c("PC1","PC3"), model = vgm(model = "Exp",psill = 0., range = 50, nugget = -.05))
  g = gstat(g, id = c("PC2","PC3"), model = vgm(model = "Exp",psill = 0., range = 10, nugget = .05))

  g.fit = fit.lmc(v,g,fit.method = 2)
  x11()
  plot(v,g.fit)

  prov_poly = cbind(prov_poly,data)
  prov_poly = as_Spatial(prov_poly)
  com_poly = as_Spatial(com_poly)
  discr_prov1 = discretizePolygon(prov_poly,cellsize = 4000,id = "DEN_PCM",value = "PC1",showProgressBar = T)
  discr_prov2 = discretizePolygon(prov_poly,cellsize = 4000,id = "DEN_PCM",value = "PC2",showProgressBar = T)
  discr_prov3 = discretizePolygon(prov_poly,cellsize = 4000,id = "DEN_PCM",value = "PC3",showProgressBar = T)
  discr_com = discretizePolygon(com_poly,cellsize = 2000,id = "PRO_COM",showProgressBar = T)
  
  discr_prov_tot = list(PC1 = discr_prov1, PC2 = discr_prov2,PC3 = discr_prov3)
  x11()
  deconv = deconvPointVgmForCoKriging(discr_prov_tot, model = "Exp", fig = F)
  x11()
  plotDeconvVgm(deconv)
  deconv1 = deconvPointVgm(discr_prov1,model = "Exp")
  deconv2 = deconvPointVgm(discr_prov2,model = "Exp")
  deconv3 = deconvPointVgm(discr_prov3,model = "Exp")
  x11()
  plotDeconvVgm(deconv1, main = paste0("PC ",1))
  x11()
  plotDeconvVgm(deconv2, main = paste0("PC ",2))
  x11()
  plotDeconvVgm(deconv3, main = paste0("PC ",3))

  ata1 =  ataKriging(x = discr_prov1, unknown = discr_com, ptVgm = deconv1, showProgress = T)
  ata2 =  ataKriging(x = discr_prov2, unknown = discr_com, ptVgm = deconv2, showProgress = T)
  ata3 =  ataKriging(x = discr_prov3, unknown = discr_com, ptVgm = deconv3, showProgress = T)  
  save(ata1,ata2,ata3, deconv1, deconv2, deconv3, file = Rdata_path("temp_downscaling"))
  
  load(Rdata_path("temp_downscaling"))
  ### Assembling predictions
  Score_matrix = data.frame(COD_PROVCOM = ata1$areaId,score1 = ata1$pred, score2 = ata2$pred, score3 = ata3$pred) %>% arrange(COD_PROVCOM)
  load("Ordered keys")  
  which(Score_matrix$COD_PROVCOM != ordered_codes_comuni)  
  
  pred_comuni = fdata(matrix(pca_province[[class]]$mean$data,byrow = T, nrow = dim(Score_matrix)[1], ncol = length(pca_province[[class]]$fdataobj.cen$argvals)), argvals =  clist_province[[class]]$T_20$argvals)  
  perturbation = cbind(Score_matrix$score1,Score_matrix$score2,Score_matrix$score3) %*% pca_province[[class]]$rotation$data[1:3,]
  pred_comuni$data = pred_comuni$data + perturbation
  
  load("Output/Data/Regression_Province.Rdata")
  load("Ordered keys")
  load("Output/Data/modified_istat_dataset.Rdata")
  
  
  

  
  set.seed(170000)
  cluster = kmeans.fd(True_Residuals_province$`15_21`$T_20, ncl = 3, max.iter = 200, draw = F)$cluster
  support = modified_date_dataset %>% dplyr::select(COD_PROVCOM , NOME_PROVINCIA ) %>% filter(COD_PROVCOM %in% ordered_codes_comuni) %>% distinct(COD_PROVCOM,.keep_all = TRUE)
  support1 = data.frame(provnames = ordered_names_province, cluster = cluster)
  infr = support %>% left_join(support1, by = c("NOME_PROVINCIA"="provnames" )) %>% arrange(COD_PROVCOM)
  
  
  for ( i in 1:7900)
  {
    prov_ind = which(ordered_names_province == infr$NOME_PROVINCIA[i])
    pred_comuni[[class]]$data[i,] = pred_comuni[[class]]$data[i,] + predictions_province[[class]]$T_20$data[prov_ind,]
  }
  
  
  x11()
  plot(inv_clr(pred_comuni), col = "gray", ylim = c(0,0.024))
  
  predicted_comuni = list()
  predicted_comuni_density = list()
  predicted_comuni[[class]] = pred_comuni
  predicted_comuni_density[[class]] = inv_clr(pred_comuni)
  save(predicted_comuni_density,predicted_comuni,infr,file = Rdata_path("downscales_predictions"))
  

  