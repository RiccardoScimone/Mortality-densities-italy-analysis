rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/Province_geometry_polygons.Rdata")
load("Output/Data/Comuni_geometry_polygons.Rdata")
load("Output/Data/Regression_Province.Rdata")
load("Output/Data/Aggregated_Provinces.Rdata")
load("Output/Data/modified_istat_dataset.Rdata")
load("Output/Data/Aggregated_Municipalities.Rdata")

pca_province = list()
variogram_scores = list()

prov_poly = st_as_sf(prov_poly)
com_poly = st_as_sf(com_poly)

prov_poly = prov_poly %>% arrange(DEN_PCM)
com_poly = com_poly %>% arrange(PRO_COM)

year = "T_20"
select = 3 ### number of components which are used in downscaling
classe = c("0_10","11_14","15_21")
for (class in classe) pca_province[[class]] = fdata2pc(errors_province[[class]][[year]],ncomp = 107)

#### Now we focus on the elderly class
class = "15_21"

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


############# PCA VISUALIZATION ###############################################
library(ggfortify)
scores_df = data.frame(pca_province[[class]]$x[,1:3])
scores_df$names = provinces_names
scores_df = tibble(scores_df)
names(scores_df) = c("PC_1", "PC_2", "PC_3", "name")
g1 = ggplot(scores_df, aes(x = PC_1, y = PC_2)) + geom_point(size = 3) + geom_text_repel(aes(label = name),size = 8)  + coord_fixed() + geom_hline(yintercept = 0, col = "red") + geom_vline(xintercept = 0, col = "red") +xlab("PC 1") + ylab("PC 2")+ theme_pubr(base_size = 25) + labs_pubr(base_size = 25)
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


##### Spatial Downscaling on the first 3 PC ################


data = data.frame(id = provinces_names)

for ( i in 1:select)
  data = cbind(data,pca_province[[class]]$x[,i])
names(data) = c("id", paste0("PC",1:select))

prov_poly = cbind(prov_poly,data)
prov_poly = as_Spatial(prov_poly)
com_poly = as_Spatial(com_poly)
discr_prov1 = discretizePolygon(prov_poly,cellsize = 4000,id = "DEN_PCM",value = "PC1",showProgressBar = T)
discr_prov2 = discretizePolygon(prov_poly,cellsize = 4000,id = "DEN_PCM",value = "PC2",showProgressBar = T)
discr_prov3 = discretizePolygon(prov_poly,cellsize = 4000,id = "DEN_PCM",value = "PC3",showProgressBar = T)
discr_com = discretizePolygon(com_poly,cellsize = 2000,id = "PRO_COM",showProgressBar = T)

discr_prov_tot = list(PC1 = discr_prov1, PC2 = discr_prov2,PC3 = discr_prov3)
### Cross-deconvolution
deconv = deconvPointVgmForCoKriging(discr_prov_tot, model = "Exp", fig = F)
x11()
plotDeconvVgm(deconv)
### Indipendent variograms
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
save(ata1,ata2,ata3, deconv1, deconv2, deconv3, deconv, file = Rdata_path("temp_downscaling"))

#### Assemble predictions and information for plotting
Score_matrix = data.frame(COD_PROVCOM = ata1$areaId,score1 = ata1$pred, score2 = ata2$pred, score3 = ata3$pred) %>% arrange(COD_PROVCOM)
which(Score_matrix$COD_PROVCOM != municipalities_codes)  

pred_municipalities = fdata(matrix(pca_province[[class]]$mean$data,byrow = T, nrow = dim(Score_matrix)[1], ncol = length(pca_province[[class]]$fdataobj.cen$argvals)), argvals =  errors_province[[class]]$T_20$argvals)  
perturbation = cbind(Score_matrix$score1,Score_matrix$score2,Score_matrix$score3) %*% pca_province[[class]]$rotation$data[1:3,]
pred_municipalities$data = pred_municipalities$data + perturbation

set.seed(170000)
cluster = kmeans.fd(errors_province[[class]]$T_20, ncl = 3, max.iter = 200, draw = F)$cluster
support = modified_date_dataset %>% dplyr::select(COD_PROVCOM , NOME_PROVINCIA ) %>% filter(COD_PROVCOM %in% municipalities_codes) %>% distinct(COD_PROVCOM,.keep_all = TRUE)
support1 = data.frame(provnames = provinces_names, cluster = cluster)
info = support %>% left_join(support1, by = c("NOME_PROVINCIA"="provnames" )) %>% arrange(COD_PROVCOM)

for ( i in 1:7900)
{
  prov_ind = which(provinces_names == info$NOME_PROVINCIA[i])
  pred_municipalities[[class]]$data[i,] = pred_municipalities[[class]]$data[i,] + predictions_province[[class]]$T_20$data[prov_ind,]
}

x11()
plot(inv_clr(pred_municipalities), col = "gray", ylim = c(0,0.024))

predicted_municipalities = list()
predicted_municipalities_density = list()
predicted_municipalities[[class]] = pred_municipalities
predicted_municipalities_density[[class]] = inv_clr(pred_municipalities)
save(predicted_municipalities_density,predicted_municipalities,info,file = Rdata_path("downscales_predictions"))

