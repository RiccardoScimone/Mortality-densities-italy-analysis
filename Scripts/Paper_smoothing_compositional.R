rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/dataset_aggregato_provinciale.Rdata")

trunc_left = as.Date("2020-01-01")
trunc_right = as.Date("2020-12-31")
data_to_smooth = prov_df_aggr %>% filter(partial_date_death <= trunc_right & partial_date_death >= trunc_left)
data_to_smooth = data_to_smooth %>% tidyr::complete (NOME_PROVINCIA,partial_date_death,CL_ETA) 
data_to_smooth[is.na(data_to_smooth)] = 0
data_to_smooth$CL_ETA = factor(data_to_smooth$CL_ETA, levels = c("0_10","11_14","15_21")) 
data_to_smooth = data_to_smooth %>% group_by(NOME_PROVINCIA,CL_ETA) %>% dplyr::select(-starts_with("F")) %>%  dplyr::select(-starts_with("M"))
data_to_smooth = data_to_smooth %>% mutate_at(.vars = paste0("T_",11:20),.funs = function(x){return(x/sum(x))})
ordered_names_province = unique(data_to_smooth$NOME_PROVINCIA)

classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
alfa = 5e-3
l = 2
npoints = 1000
knots = 52
xcp = as.numeric((as.numeric(trunc_left):as.numeric(trunc_right)))
knots_vector = floor(xcp[seq(1,length(xcp), length.out = knots)])
degree = 3
add_node = 1
add_days = 7
flist_province = list()
clist_province = list()
for(class in classe)
{  
  flist_province[[class]] = list()
  clist_province[[class]] = list()
  for(year in years)
  {
    temp= data_to_smooth %>% filter(CL_ETA == class) %>% ungroup() %>% dplyr::select(NOME_PROVINCIA,partial_date_death,one_of(year)) %>% arrange(partial_date_death)
    temp$dummy = temp[[year]]
    temp = temp %>% dplyr::select(-one_of(year))
    temp = temp %>% pivot_wider(names_from = partial_date_death, values_from = dummy)
    temp[is.na(temp)] = 0
    matrix = as.matrix(temp[,-1])
    matrix[which(rowSums(matrix) < 1/2),] = 1/dim(matrix)[2]
    xcp_extended = c((xcp[1] - add_days):(xcp[1] - 1),xcp,(xcp[length(xcp)] + 1):(xcp[length(xcp)] + add_days))
    knots_vector_extended = c(xcp_extended[1],knots_vector,xcp_extended[length(xcp_extended)])
    matrix = cbind(matrix[,length(xcp):(length(xcp) - add_days + 1)], matrix, matrix[,1:add_days])
    smooth = smoothSplines(k = degree,l = l,alpha = alfa,data = matrix,knots = knots_vector_extended,num_points = npoints,xcp = xcp_extended, fast = 1)
    argvals = seq(xcp_extended[1],xcp_extended[length(xcp_extended)],length.out = npoints,)
    flist_province[[class]][[year]] = fdata(smooth$Y,argvals = argvals)
    start = xcp[1]
    stop = xcp[length(xcp)]
    flist_province[[class]][[year]] = conditioning(flist_province[[class]][[year]],start,stop)
    clist_province[[class]][[year]] = clr(flist_province[[class]][[year]])
  } 
    
}

class = "15_21"
year = "T_20"
nome =   "Bergamo" 
index = which((data_to_smooth %>% filter(CL_ETA == class)%>% distinct(NOME_PROVINCIA))$NOME_PROVINCIA == nome)
x11()
plot(flist_province[[class]][[year]][index], col = "red", ylim = c(0,0.02), lwd = 2, xaxt = "n")
points(as.numeric(trunc_left):as.numeric(trunc_right), (data_to_smooth %>% filter(CL_ETA == class,NOME_PROVINCIA == nome))[[year]], col = "black")
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months"), format="%m-%Y")

for(class in classe){
  pdf(paste0("Output/Plot/smoothed_province_",class,"knots_",knots, ".pdf"),width = 20,height = 14)
  
  par(mfrow = c(2,5))
    for (year in years)
  {
      plot(flist_province[[class]][[year]], col = "gray", main = year, ylim = c(0,0.02),xaxt = "n")
      axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months"), format="%m-%Y")
  }
  dev.off()
}

save(clist_province,flist_province, file = Rdata_path(paste0("Smoothing_Province ",knots, ".pdf")))

### Prepare for regression. We repeat the model for each year
regr_models_province = list()

to_average = 4
for(class in classe)
  {
  regr_models_province[[class]] = list()
  mean_previous_province = list()
  for(i in (to_average+1):length(years))
    {
      year = years[i]
      argvals = clist_province[[class]][[year]]$argvals
      mean_previous_province[[i]] =  clist_province[[class]][[years[i-1]]]$data
      for (y in years[(i - to_average):(i-2)])
        mean_previous_province[[i]] = clist_province[[class]][[y]]$data + mean_previous_province[[i]]
      mean_previous_province[[i]] = mean_previous_province[[i]]/to_average
      mean_previous_province[[i]] = fdata(mean_previous_province[[i]],argvals)
      basis = create.bspline.basis(rangeval = mean_previous_province[[i]]$rangeval,breaks = knots_vector)
      model = fregre.basis.fr(mean_previous_province[[i]],clist_province[[class]][[year]],basis.s = basis,basis.t = basis,lambda.s = 1e-1,lambda.t = 1e-1)
  regr_models_province[[class]][[year]] = list()
  regr_models_province[[class]][[year]]$model = model
  print(year)
  }
}

### Assemble predictions_comuni_province and residuals
predictions_province = list()
for (class in classe){
  predictions_province[[class]] = list()
  predictions_province[[class]][[years[to_average+1]]] = flist_province[[class]][[years[to_average]]]
  regr_models_province[[class]][[years[to_average+1]]]$true_res = norm.fdata(clist_province[[class]][[years[to_average]]] - clist_province[[class]][[years[to_average+1]]])
  for(i in (to_average+1):(length(years)-1))
  {
    year = years[i+1]
    predictions_province[[class]][[year]] = predict(regr_models_province[[class]][[years[i]]]$model,new.fdataobj = mean_previous_province[[i+1]])
    regr_models_province[[class]][[year]]$true_res = norm.fdata(predictions_province[[class]][[year]] - clist_province[[class]][[year]])
    predictions_province[[class]][[year]] = inv_clr( predictions_province[[class]][[year]])
  }
}

save(regr_models_province, mean_previous_province, predictions_province, file = Rdata_path("Regression_Province"))


## Visualize predictions

for ( class in classe)
  for(year in years[(to_average+1):10])
  { 
    if (year != "T_20") lims = c(0,1/365*3)
    else lims = c(0,.02)
    pdf(paste0("Output/Plot/preds_province_",year,"_", class,".pdf"),width = 20,height = 10)
    par(mfrow = c(1,2))
    plot(flist[[class]][[year]],main = paste0("observation year " ,year), col = "gray",xaxt = "n",ylab = "density",ylim = lims,xlab = "month")
    axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months"), format="%m")
    plot(predictions_province[[class]][[year]],main = paste0("prediction year " ,year), col = "gray",xaxt = "n",ylab = "density",ylim = lims,xlab = "month")
    axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months"), format="%m")
    dev.off()
  }






### visualize residual of model for each class
load("Output/Data/Province_geometry_polygons.Rdata")

for(class in classe)
{
  prov_poly_t = prov_poly
  heatlist = list()
for (year in years[-(1:to_average)]){
  cluster = kmeans(regr_models_province[[class]][[year]]$true_res,3)$cluster
  temp = data.frame(Provincia = unique(data_to_smooth$NOME_PROVINCIA), residuals =  regr_models_province[[class]][[year]]$true_res, cluster = cluster)
  temp$cluster = as.factor(temp$cluster)
  names(temp) = c("Provincia",paste0("residuals_",year), paste0("cluster ",year))
  prov_poly_t@data = prov_poly_t@data %>% left_join(temp, by = c("DEN_PCM" = "Provincia"))

}

prov_poly_t1 = st_as_sf(prov_poly_t) %>% pivot_longer(starts_with("residuals"), names_to = "year", values_to = "res") %>% group_by(year)
prov_poly_t1 = st_as_sf(prov_poly_t1)
g = ggplot(prov_poly_t1) + geom_sf(aes(fill = res)) + scale_fill_viridis(option = "inferno")  + 
  ggrepel::geom_text_repel(aes(label = DEN_PCM,geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20)+ facet_wrap(.~ year, nrow = 2) + ggtitle(class) 
pdf(paste0("Output/Plot/province_residuals_", class,".pdf"),width = 20,height = 10)
plot(g)
dev.off()
prov_poly_t2 = st_as_sf(prov_poly_t) %>% pivot_longer(starts_with("cluster"), names_to = "year", values_to = "cluster") %>% group_by(year)
prov_poly_t2 = st_as_sf(prov_poly_t2)
g = ggplot(prov_poly_t2) + geom_sf(aes(fill = cluster))  + 
  ggrepel::geom_text_repel(aes(label = DEN_PCM,geometry = geometry), size = 1.7, stat = "sf_coordinates",max.overlaps = 20)+ facet_wrap(.~ year, nrow = 2) + ggtitle(class) 
pdf(paste0("Output/Plot/province_residuals_cluster", class,".pdf"),width = 20,height = 10)
plot(g)
dev.off()
}


###################################################################################


rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/dataset_aggregato_comunale.Rdata")

trunc_left = as.Date("2020-01-01")
trunc_right = as.Date("2020-12-31")
data_to_smooth = com_df_aggr %>% filter(partial_date_death <= trunc_right & partial_date_death >= trunc_left)

data_to_smooth = data_to_smooth %>% tidyr::complete (COD_PROVCOM,partial_date_death,CL_ETA) 
data_to_smooth[is.na(data_to_smooth)] = 0

data_to_smooth$CL_ETA = factor(data_to_smooth$CL_ETA, levels = c("0_10","11_14","15_21")) 
data_to_smooth = data_to_smooth %>% group_by(COD_PROVCOM,CL_ETA) %>% dplyr::select(-starts_with("F")) %>%  dplyr::select(-starts_with("M"))
data_to_smooth = data_to_smooth %>% mutate_at(.vars = paste0("T_",11:20),.funs = function(x){return(x/sum(x))})
ordered_codes_comuni = unique(data_to_smooth$COD_PROVCOM)


classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
alfa = 1e-2
l = 2
npoints = 1000
knots = 52
xcp = as.numeric((as.numeric(trunc_left):as.numeric(trunc_right)))
knots_vector = floor(xcp[seq(1,length(xcp), length.out = knots)])
degree = 3
add_node = 1
add_days = 7
flist_comuni = list()
clist_comuni = list()
for(class in classe)
{  
  flist_comuni[[class]] = list()
  clist_comuni[[class]] = list()
  for(year in years)
  {
    temp= data_to_smooth %>% filter(CL_ETA == class) %>% ungroup %>% dplyr::select(COD_PROVCOM,partial_date_death,one_of(year))  %>% arrange(partial_date_death)
    temp$dummy = temp[[year]]
    temp = temp %>% dplyr::select(-one_of(year))
    temp = temp %>% pivot_wider(names_from = partial_date_death, values_from = dummy)
    temp[is.na(temp)] = 0
    matrix = as.matrix(temp[,-1])
    matrix[which(rowSums(matrix) < 1/2),] = 1/dim(matrix)[2]
    xcp_extended = c((xcp[1] - add_days):(xcp[1] - 1),xcp,(xcp[length(xcp)] + 1):(xcp[length(xcp)] + add_days))
    knots_vector_extended = c(xcp_extended[1],knots_vector,xcp_extended[length(xcp_extended)])
    matrix = cbind(matrix[,length(xcp):(length(xcp) - add_days + 1)], matrix, matrix[,1:add_days])
    smooth = smoothSplines(k = degree,l = l,alpha = alfa,data = matrix,knots = knots_vector_extended,num_points = npoints,xcp = xcp_extended, fast = 1)
    argvals = seq(xcp_extended[1],xcp_extended[length(xcp_extended)],length.out = npoints)
    flist_comuni[[class]][[year]] = fdata(smooth$Y,argvals = argvals)
    start = xcp[1]
    stop = xcp[length(xcp)]
    flist_comuni[[class]][[year]] = conditioning(flist_comuni[[class]][[year]],start,stop)
    clist_comuni[[class]][[year]] = clr(flist_comuni[[class]][[year]])
  } 
  
}


save(flist_comuni,clist_comuni, file = Rdata_path("Smoothing_comuni"))


load("Output/Data/Smoothing_comuni.Rdata")

for(class in classe){
  png(paste0("Output/Plot/smoothed_comuni_",class,".png"),width = 20,height = 14, units = "in", res = 1200)
  
  par(mfrow = c(2,5))
  for (year in years)
  {
    plot(flist_comuni[[class]][[year]], col = "gray", main = year, ylim = c(0,0.02))
    
  }
  dev.off()
}

x11()
code = "16024"
codes = temp$COD_PROVCOM
index = which(ordered_codes_comuni == code)
f = flist_comuni[[class]][[year]][index]
plot(f, col = "red", ylim = c(0,0.03), lwd = 2,xaxt = "n")
axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months"), format="%m-%Y")
points(as.numeric((data_to_smooth %>% filter(CL_ETA == class,COD_PROVCOM == code))$partial_date_death), temp[index,-1], col = "black")




### Prepare for regression. We repeat the model for each year
regr_models_comuni = list()

to_average = 4
for(class in classe)
{
  regr_models_comuni[[class]] = list()
  mean_previous_comuni = list()
  for(i in (to_average+1):length(years))
  {
    year = years[i]
    argvals = clist_comuni[[class]][[year]]$argvals
    mean_previous_comuni[[i]] =  clist_comuni[[class]][[years[i-1]]]$data
    for (y in years[(i - to_average):(i-2)])
      mean_previous_comuni[[i]] = clist_comuni[[class]][[y]]$data + mean_previous_comuni[[i]]
    mean_previous_comuni[[i]] = mean_previous_comuni[[i]]/to_average
    mean_previous_comuni[[i]] = fdata(mean_previous_comuni[[i]],argvals)
    basis = create.bspline.basis(rangeval = mean_previous_comuni[[i]]$rangeval,breaks = knots_vector)
    model = fregre.basis.fr(mean_previous_comuni[[i]],clist_comuni[[class]][[year]],basis.s = basis,basis.t = basis,lambda.s = 1e-1,lambda.t = 1e-1)
    regr_models_comuni[[class]][[year]] = list()
    regr_models_comuni[[class]][[year]]$model = model
    print(year)
  }
}

### Assemble predictions_comuni and residuals
predictions_comuni = list()
for (class in classe){
  predictions_comuni[[class]] = list()
  predictions_comuni[[class]][[years[to_average+1]]] = flist_comuni[[class]][[years[to_average]]]
  regr_models_comuni[[class]][[years[to_average+1]]]$true_res = norm.fdata(clist_comuni[[class]][[years[to_average]]] - clist_comuni[[class]][[years[to_average+1]]])
  for(i in (to_average+1):(length(years)-1))
  {
    year = years[i+1]
    predictions_comuni[[class]][[year]] = predict(regr_models_comuni[[class]][[years[i]]]$model,new.fdataobj = mean_previous_comuni[[i+1]])
    regr_models_comuni[[class]][[year]]$true_res = norm.fdata(predictions_comuni[[class]][[year]] - clist_comuni[[class]][[year]])
    predictions_comuni[[class]][[year]] = inv_clr( predictions_comuni[[class]][[year]])
  }
}

save(regr_models_comuni, mean_previous_comuni, predictions_comuni, file = Rdata_path("Regression_Comuni"))


for ( class in classe)
  for(year in years[(to_average+1):10])
  { 
    if (year != "T_20") lims = c(0,1/365*3)
    else lims = c(0,.02)
    png(paste0("Output/Plot/preds_comuni_",class,".png"),width = 20,height = 10, units = "in", res = 1200)
    par(mfrow = c(1,2))
    plot(flist_comuni[[class]][[year]],main = paste0("observation year " ,year), col = "gray",xaxt = "n",ylab = "density",ylim = lims,xlab = "month")
    axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months"), format="%m")
    plot(predictions_comuni[[class]][[year]],main = paste0("prediction year " ,year), col = "gray",xaxt = "n",ylab = "density",ylim = lims,xlab = "month")
    axis.Date(1, at=seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months"), format="%m")
    dev.off()
  }

### visualize residual of model for each class
load("Output/Data/Comuni_geometry_polygons.Rdata")
load("Output/Data/Regression_Comuni.Rdata")

for(class in classe)
{
  com_poly_t = com_poly
  for (year in years[-(1:to_average)]){
    cluster = kmeans(regr_models_comuni[[class]][[year]]$true_res,3)$cluster
    temp = data.frame(Comune = unique(data_to_smooth$COD_PROVCOM), residuals =  regr_models_comuni[[class]][[year]]$true_res, cluster = cluster)
    temp$cluster = as.factor(temp$cluster)
    names(temp) = c("Comune",paste0("residuals_",year), paste0("cluster ",year))
    com_poly_t@data = com_poly_t@data %>% left_join(temp, by = c("PRO_COM" = "Comune"))
  }
  com_poly_t1 = st_as_sf(com_poly_t) %>% pivot_longer(starts_with("residuals"), names_to = "year", values_to = "res") %>% group_by(year)
  com_poly_t1 = st_as_sf(com_poly_t1)
  g = ggplot(com_poly_t1) + geom_sf(aes(fill = res),colour = NA) + scale_fill_viridis(option = "inferno") + facet_wrap(.~ year, nrow = 2) + ggtitle(class) 
  png(paste0("Output/Plot/comuni_residuals_", class,".png"),width = 20,height = 14, units = "in", res = 1200)
  plot(g)
  dev.off()
  com_poly_t2 = st_as_sf(com_poly_t) %>% pivot_longer(starts_with("cluster"), names_to = "year", values_to = "cluster") %>% group_by(year)
  com_poly_t2 = st_as_sf(com_poly_t2)
  g = ggplot(com_poly_t2) + geom_sf(aes(fill = cluster),colour = NA) + facet_wrap(.~ year, nrow = 2) + ggtitle(class) 
  png(paste0("Output/Plot/comuni_residuals_cluster", class,".png"),width = 20,height = 14, units = "in", res = 1200)
  plot(g)
  dev.off()
}