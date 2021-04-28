######################################################################################
rm(list = ls())
source("Scripts/Utility_Functions.R")
library(fdagstat)
load("Output/Data/Comuni_geometry_polygons.Rdata")
load("Output/Data/Province_geometry_polygons.Rdata")
load("Output/Data/Smoothing_Province 52.pdf.Rdata")
load("Output/Data/Smoothing_comuni.Rdata")
load("Output/Data/Regression_Province.Rdata")
load("Output/Data/Regression_Comuni.Rdata")

################# Kriging Analysis on residuals ##############################à
## I need residuals in functional form
classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
to_average = 4
True_Residuals_province = True_Residuals_comuni = distmat_province = distmat_comuni = list()

set.seed(10000)
comuni_sample = sample(1:7900,3000)

for ( class in classe){
  True_Residuals_province[[class]] = True_Residuals_comuni[[class]] = distmat_province[[class]]= distmat_province[[class]] = list()
  for ( year in years[-(1:(to_average))])
  {
    True_Residuals_province[[class]][[year]] = clist_province[[class]][[year]] - clr(predictions_province[[class]][[year]])
    True_Residuals_comuni[[class]][[year]] = clist_comuni[[class]][[year]] - clr(predictions_comuni[[class]][[year]])
    distmat_province[[class]][[year]] = metric.lp(True_Residuals_province[[class]][[year]])
    print("here done")
    distmat_comuni[[class]][[year]] =  metric.lp(True_Residuals_comuni[[class]][[year]][comuni_sample])
  }
}

save(True_Residuals_comuni,True_Residuals_province,distmat_comuni,distmat_province,comuni_sample, file = Rdata_path("Residuals"))


rm(clist_comuni,clist_province,flist_comuni,flist_province,regr_models_comuni,regr_models_province)

########## Variograms estimation on Provinces ############
load("Output/Data/Residuals.Rdata")
load("Ordered keys")
polygon = prov_poly
polygon = st_as_sf(polygon)
centroid_polygon = polygon
centroid_polygon$geometry = st_centroid(centroid_polygon$geometry)
n = length(centroid_polygon$geometry)

train_coords = matrix(nrow = n, ncol = 2)
for ( i in 1:n) {train_coords[i,1] = centroid_polygon$geometry[[i]][1]; train_coords[i,2] = centroid_polygon$geometry[[i]][2]}

to_order = data.frame(names = polygon$DEN_PCM , x = train_coords[,1], y = train_coords[,2])
orders = data.frame(names = ordered_names_province)
orders = orders %>% left_join(to_order, by = c("names" = "names"))

train_coords = orders[,2:3]
#train_coords = as.matrix(train_coords/1000)
year_labs = paste0("20",11:20)
names(year_labs) = years
### 
variograms = list()
plotvariograms = list()
for ( class in classe){
  variograms[[class]] = plotvariograms[[class]] = list()
#  pdf(paste0("Output/Plot/Variograms_Province_",class, ".pdf"), width = 20, height = 14)
  x11()
  for ( year in years[-(1:(to_average))])
  { 
    astep = True_Residuals_province[[class]][[year]]$argvals[2] - True_Residuals_province[[class]][[year]]$argvals[1]
    g = fstat(NULL,vName = year_labs[[year]], Coordinates = data.frame(train_coords), Functions = data.frame(t(True_Residuals_province[[class]][[year]]$data)), scalar = F)
    g = fvariogram("~x + y", g, Nlags = 50, LagMax = 800000,ArgStep = astep, useResidual = F, comments = F)
    inrange = ifelse(year == "T_20", 50000, 80000)
    nug = ifelse(year == "T_20",6/10,9.5/10)*mean(g$variogram$gamma)
    psill = ifelse(year == "T_20",4/10,0.5/10)*mean(g$variogram$gamma)
    g = fitVariograms(g, model=vgm(psill, "Exp", inrange, nug))
    variograms[[class]][[year]] = g
    plotvariograms[[class]][[year]] = plotVariogram(g,ggReturn = T) + rremove("legend") + ylim(4,16)
  }
   plot(ggarrange(plotlist = plotvariograms[[class]]))
}

train_coords_province = train_coords
variograms_province = variograms
save(variograms_province, train_coords_province, file = Rdata_path("Variograms_Province"))

########## Variograms estimation on Comuni ############
rm (list = ls())
source("Scripts/Utility_Functions.R")
load("Output/Data/Residuals.Rdata")
load("Output/Data/Comuni_geometry_polygons.Rdata")
load("Ordered keys")

classe = c("0_10","11_14","15_21")
classe = "15_21"
years = paste0("T_",11:20)
to_average = 4

polygon = com_poly
polygon = st_as_sf(polygon)
centroid_polygon = polygon
centroid_polygon$geometry = st_centroid(centroid_polygon$geometry)
n = length(centroid_polygon$geometry)

train_coords = matrix(nrow = n, ncol = 2)
for ( i in 1:n) {train_coords[i,1] = centroid_polygon$geometry[[i]][1]; train_coords[i,2] = centroid_polygon$geometry[[i]][2]}

to_order = data.frame(names = polygon$PRO_COM , x = train_coords[,1], y = train_coords[,2])
orders = data.frame(names = ordered_codes_comuni)
orders = orders %>% left_join(to_order, by = c("names" = "names"))

train_coords = orders[,2:3]
train_coords = as.matrix(train_coords)/1000
train_coords_comuni = train_coords
train_coords = train_coords[comuni_sample,]

### 
bin_variograms = variograms = list()
for ( class in classe){
  bin_variograms[[class]] =  list()
  variograms[[class]] = list()
#  pdf(paste0("Output/Plot/Variograms_Comuni_",class, ".pdf"), width = 20, height = 14)
  x11()
  par(mfrow = c(2,3))
  for ( year in years[-(1:(to_average))])
  { 
    temp = trace.variog(train_coords,distmat_comuni[[class]][[year]], bin = T, max.dist = 100, uvec = 50)
    bin_variograms[[class]][[year]] = temp
    inrange = ifelse(year == "T_20", 5, 5)
    variograms[[class]][[year]] = variofit(temp,cov.model = "gaussian",fix.nugget = F,nugget = 9/10*mean(temp$v), max.dist = 100, ini.cov.pars = c(1/10*mean(temp$v),inrange),weights = "equal" )
    if ( year == years[to_average + 1]) lim = 1.9*mean(temp$v)
    plot(temp, main = class, ylab = year, ylim = c(0,lim))
    lines(variograms[[class]][[year]])
  }
#  dev.off()
}

variograms_comuni = variograms
bin_variograms_comuni = bin_variograms
save(variograms_comuni, train_coords_comuni, bin_variograms_comuni, file = Rdata_path("Variograms_Comuni"))

#### Insert k-means in 2020 ####

rm(list = ls())
source("Scripts/Utility_Functions.R")
load("Output/Data/Variograms_Province.Rdata")
load("Output/Data/Variograms_Comuni.Rdata")
load("Output/Data/Residuals.Rdata")
rm(True_Residuals_comuni)
rm(distmat_comuni)

classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
to_average = 4
years = years[-(1:to_average)]
kmeans = list()
for (class in classe)
{
  pdf(file = paste0("Output/Plot/Residuals_province_", class, ".pdf"), width = 20,height = 10)
  kmeans[[class]] = list()
  par(mfrow = c(2,3))
  for ( year in years){
    kmeans[[class]][[year]] = rep(1,107)
    kmeans[[class]][[year]] = pam(distmat_province[[class]][[year]],k = 3, diss = T)$clustering
    plot(inv_clr( True_Residuals_province[[class]][[year]]), col = kmeans[[class]][[year]], main = year, ylim = c(0,0.02), ylab = "residual density",xaxt = "n")
    axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months"), format="%m-%Y")
  
  }  
  dev.off()
}

year = "T_20"
classe = c("11_14","15_21")

Kmeans_residuals =  list()

for ( class in classe)
  Kmeans_residuals[[class]] = center_cluster(True_Residuals_province[[class]][[year]], kmeans[[class]][[year]])

distmat_kmeans_res = list()

for ( class in classe)
  distmat_kmeans_res[[class]] = metric.lp(Kmeans_residuals[[class]]$fdata)


### 
bin_variograms_kmeans = variograms_kmeans = list()
for ( class in classe)
{ 
  x11()
  temp = trace.variog(train_coords_province,distmat_kmeans_res[[class]], bin = T, max.dist = 500, uvec = 40)
  bin_variograms_kmeans[[class]] = temp
  variograms_kmeans[[class]] = variofit(temp,cov.model = "gaussian",fix.nugget = T,nugget = 0, max.dist = 500, ini.cov.pars = c(mean(temp$v),20),weights = "cressie" )
  plot(temp, main = class, ylab = year)
  lines(variograms_kmeans[[class]])
}



###### Naive predictions ( no downscaling) ################### 
load("Ordered keys")
load("Output/Data/modified_istat_dataset.Rdata")

support = modified_date_dataset %>% filter(CL_ETA == class) %>% dplyr::select(COD_PROVCOM , NOME_PROVINCIA ) %>% filter(COD_PROVCOM %in% ordered_codes_comuni) %>% distinct(COD_PROVCOM,.keep_all = TRUE)
infr = support %>% left_join(support1, by = c("NOME_PROVINCIA"="provnames" )) %>% arrange(COD_PROVCOM)


test_coords = train_coords_comuni
predicted_comuni = predicted_comuni_density =  list()
for ( class in classe){
  support1 = data.frame(provnames = ordered_names_province, cluster = kmeans[[class]]$T_20)
  infr = support %>% left_join(support1, by = c("NOME_PROVINCIA"="provnames" )) %>% arrange(COD_PROVCOM)
  KC = krige.control(obj.model = variograms_kmeans[[class]])
  temp = list()
  temp$fdata = Krige_predict_functional(Kmeans_residuals[[class]]$fdata,train_coords_province,test_coords,KC)
  temp$means = Kmeans_residuals[[class]]$centers
  predicted_comuni[[class]] = decenter_cluster(temp,infr$cluster)


}

load("Output/Data/Regression_Province.Rdata")

for ( i in 1:dim(infr)[1])
{
  prov_ind = which(ordered_names_province == infr$NOME_PROVINCIA[i])
  predicted_comuni[[class]]$data[i,] = predicted_comuni[[class]]$data[i,] + predictions_province[[class]]$T_20$data[prov_ind,]
}
  
for (class in classe)
  predicted_comuni_density[[class]] = inv_clr(predicted_comuni[[class]])

save(predicted_comuni, predicted_comuni_density, infr, kmeans, file = Rdata_path("kriging_comuni"))






#######################################################################################################







