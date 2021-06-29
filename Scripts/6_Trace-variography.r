rm(list = ls())
source("Scripts/Utility_Functions.R")
library(fdagstat)
load("Output/Data/Province_geometry_polygons.Rdata")
load("Output/Data/Regression_Province.Rdata")
load("Output/Data/Aggregated_Provinces.Rdata")
classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
to_average = 4

polygon = prov_poly
polygon = st_as_sf(polygon)
centroid_polygon = polygon
centroid_polygon$geometry = st_centroid(centroid_polygon$geometry)
n = length(centroid_polygon$geometry)

train_coords = matrix(nrow = n, ncol = 2)
for ( i in 1:n) {train_coords[i,1] = centroid_polygon$geometry[[i]][1]; train_coords[i,2] = centroid_polygon$geometry[[i]][2]}

to_order = data.frame(names = polygon$DEN_PCM , x = train_coords[,1], y = train_coords[,2])
orders = data.frame(names = provinces_names)
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
 # x11()
  for ( year in years[-(1:(to_average+1))])
  { 
    astep = errors_province[[class]][[year]]$argvals[2] - errors_province[[class]][[year]]$argvals[1]
    g = fstat(NULL,vName = year_labs[[year]], Coordinates = data.frame(train_coords), Functions = data.frame(t(errors_province[[class]][[year]]$data)), scalar = F)
    g = fvariogram("~x + y", g, Nlags = 50, LagMax = 800000,ArgStep = astep, useResidual = F, comments = F)
    inrange = ifelse(year == "T_20", 50000, 80000)
    nug = ifelse(year == "T_20",6/10,9.5/10)*mean(g$variogram$gamma)
    psill = ifelse(year == "T_20",4/10,0.5/10)*mean(g$variogram$gamma)
    g = fitVariograms(g, model=vgm(psill, "Exp", inrange, nug))
    variograms[[class]][[year]] = g
    plotvariograms[[class]][[year]] = plotVariogram(g,ggReturn = T) + rremove("legend") + ylim(4,16)
  }
 # plot(ggarrange(plotlist = plotvariograms[[class]]))
}

train_coords_province = train_coords
variograms_province = variograms
save(variograms_province, train_coords_province, file = Rdata_path("Variograms_Province"))



