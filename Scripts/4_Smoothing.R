rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/Aggregated_Provinces.Rdata") ### for smoothing on Provinces
load("Output/Data/Aggregated_Municipalities.Rdata") ### for smoothing on Municipalities

#### Prepare data format for smoothing
data_to_smooth_provinces = provinces_aggregated %>% tidyr::complete (NOME_PROVINCIA,partial_date_death,CL_ETA) 
data_to_smooth_provinces[is.na(data_to_smooth_provinces)] = 0
data_to_smooth_provinces$CL_ETA = factor(data_to_smooth_provinces$CL_ETA, levels = c("0_10","11_14","15_21")) 
data_to_smooth_provinces = data_to_smooth_provinces %>% group_by(NOME_PROVINCIA,CL_ETA) %>% dplyr::select(-starts_with("F")) %>%  dplyr::select(-starts_with("M")) %>% arrange(NOME_PROVINCIA)
data_to_smooth_provinces = data_to_smooth_provinces %>% mutate_at(.vars = paste0("T_",11:20),.funs = function(x){return(x/sum(x))})

#### Do the same on municipalities
data_to_smooth_municipalities = municipalities_aggregated %>% tidyr::complete (COD_PROVCOM,partial_date_death,CL_ETA) %>% dplyr::select(-NOME_PROVINCIA,-NOME_REGIONE,-NOME_COMUNE)
data_to_smooth_municipalities[is.na(data_to_smooth_municipalities)] = 0
data_to_smooth_municipalities = data_to_smooth_municipalities%>% left_join(municipalities_aggregated %>% distinct(COD_PROVCOM,.keep_all = T) %>% dplyr::select(COD_PROVCOM, NOME_PROVINCIA,NOME_REGIONE,NOME_COMUNE ))
data_to_smooth_municipalities$CL_ETA = factor(data_to_smooth_municipalities$CL_ETA, levels = c("0_10","11_14","15_21")) 
data_to_smooth_municipalities = data_to_smooth_municipalities %>% group_by(COD_PROVCOM,CL_ETA) %>% dplyr::select(-starts_with("F")) %>%  dplyr::select(-starts_with("M")) %>% arrange(COD_PROVCOM)
data_to_smooth_municipalities = data_to_smooth_municipalities %>% mutate_at(.vars = paste0("T_",11:20),.funs = function(x){return(x/sum(x))})


save(data_to_smooth_provinces,data_to_smooth_municipalities, file = Rdata_path("Smoothing_datasets"))

#### Smooth Provinces Data ###########
### We just set some parameters for doing stuff automatically and for the smoothing algorithm
classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
alfa = 5e-3
l = 2
npoints = 1000
knots = 52
xcp = as.numeric((as.numeric(as.Date("2020-01-01")):as.numeric(as.Date("2020-12-31"))))
knots_vector = floor(xcp[seq(1,length(xcp), length.out = knots)])
degree = 3
add_node = 1
add_days = 7
flist_province = list() ##smoothed densities
clist_province = list() ## smoothed clr
for(class in classe)
{  
  flist_province[[class]] = list()
  clist_province[[class]] = list()
  for(year in years)
  {
    temp= data_to_smooth_provinces %>% filter(CL_ETA == class) %>% ungroup() %>% dplyr::select(NOME_PROVINCIA,partial_date_death,one_of(year)) %>% arrange(partial_date_death)
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

save(clist_province,flist_province, file = Rdata_path(paste0("Smoothing_Provinces_",knots)))


### Smooth Municipalities data

alfa = 5e-3
flist_comuni = list()
clist_comuni = list()
for(class in classe)
{  
  flist_comuni[[class]] = list()
  clist_comuni[[class]] = list()
  for(year in years)
  {
    temp= data_to_smooth_municipalities %>% filter(CL_ETA == class) %>% ungroup %>% dplyr::select(COD_PROVCOM,partial_date_death,one_of(year))  %>% arrange(partial_date_death)
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

save(flist_comuni,clist_comuni, file = Rdata_path("Smoothing_municipalities"))

