library(zoo)
library(plotfunctions)
library(tidyverse)
library(lubridate)
library(fda)
library(purrr)
library(stringr)
library(ggplot2)
library(fda.usc)
library(scales)
library(cluster)
library(transport)
library(ggalt)
library(ggthemes)
library(fpc)
library(tibble)
library(viridis)
library(geofd)
library(SoDA)
library(readxl)
library(geoR)
library(sf)
library(rgdal)
library(KSgeneral)
library(ggpubr)
library(factoextra)
library(spdep)
library(viridis)
library(tmap)
library(tmaptools)
library(gridExtra)
library(lattice)
library(ggrepel)
library(robCompositions)
library(atakrig)
library(maptools)
library(ggmap)
library(Cairo)
library(dplyr)

Rdata_path= function(name_str) 
{
  return(paste0("Output/Data/",name_str,".Rdata"))
}

Plot_path= function(name_str) 
{
  return(paste0("Output/Plot/",name_str))
}



Make_covid_plot = function (data,firstkeys,firstkeyvarname,years,classes, gender = "T", plotfunction = geom_smooth, args = NULL, point = T)
{
  temp_data = data %>% filter(get(firstkeyvarname) %in% firstkeys, CL_ETA %in% classes) %>% dplyr::select(any_of(c("partial_date_death","CL_ETA",firstkeyvarname,paste0(gender,"_",years))))%>%
    pivot_longer(.,cols = paste0(gender,"_",years), names_to = "Year", values_to = "Deaths")
  if (is.null(args))
    plot = ggplot(temp_data,mapping = aes(x = partial_date_death, y = Deaths, color = Year)) + plotfunction() +facet_grid(CL_ETA~get(firstkeyvarname), scale = "free_y") 
  else
    plot = ggplot(temp_data,mapping = aes(x = partial_date_death, y = Deaths, color = Year)) + plotfunction(span = args) +facet_grid(CL_ETA~get(firstkeyvarname), scale = "free_y") 
  if (point)
    plot = plot + geom_point()
  plot = plot + scale_x_date(date_labels = "%b")
  return(plot)
}

aggregate_classes = function(data,keyname,firstvec,lastvec) #### BE VERY CAREFUL THAT THE VECTORS ARE COHERENT!
{
  n = length(firstvec)
  newdata = data
  for (i in 1:n){
    first = firstvec[i]
    last = lastvec[i]
    classes = as.character(first:last)
    newdata$CL_ETA[newdata$CL_ETA %in% classes] = paste0(first,"_",last)
    newdata = newdata %>% group_by(get(keyname), partial_date_death, CL_ETA)%>%
      summarise_at(c(paste0("M_",c(11:20)),paste0("F_",c(11:20)),paste0("T_",c(11:20))), sum) %>% ungroup()
    names(newdata)[1] = keyname
    print(paste0(i," out of ",n," aggregation completed"))
  }
  return(newdata)
}

# Smooth_data = function(data,areas,area_name,class,gender,splineorder,knots)
# {
#   to_ret = vector("list", length(areas))
#   names(to_ret) = areas
#   for (area in areas)
#   { 
#     temp_data = data %>% filter(CL_ETA == class, get(area_name) == area) %>% pivot_longer(.,cols = paste0(gender,"_",15:20), names_to = "Year", values_to = "Deaths")
#     for (i in 15:20)
#       temp_data$Year[temp_data$Year == paste0(gender,"_",i)] = paste0("20",i)
#     year(temp_data$partial_date_death) = as.numeric(temp_data$Year)
#     temp_data = drop_na(temp_data)
#     temp_data = temp_data %>% filter(!(partial_date_death %in% seq(from =as.Date("2020-11-01"), to = as.Date("2020-12-31"), by = "day"))) %>% arrange(partial_date_death)
#     basis = create.bspline.basis(rangeval = c(knots[1],knots[length(knots)]),breaks = knots,norder = splineorder)
#     basismat = try(predict(basis, temp_data$partial_date_death), silent = T)
#     if (dim(temp_data)[1] >= 1000){
#       coef = try(solve(crossprod(basismat),
#                    crossprod(basismat,temp_data$Deaths)),silent = T)
#       if (class(coef) != "try-error")
#         to_ret[[area]] = fd(coef,basis)
#     }
#   }
#   return(to_ret[!sapply(to_ret,is.null)])
# }

# extract_functional_dataset = function(fda_list,start,end,spacing = 0.01,years){
#   keys = names(fda_list)
#   for(year in years){
#     if (year == "2015")
#       strt = as.Date(paste0(year,"-","01-10"))
#     else
#       strt = as.Date(paste0(year,"-",start))
#     en =   as.Date(paste0(year,"-",end))
#     argvals = seq(as.numeric(strt),as.numeric(en), by = spacing)
#     temp = matrix(nrow = length(keys), ncol = length(argvals))
#     for (i in 1:length(keys)){
#       temp[i,] = as.vector(fdata(fda_list[[keys[i]]],argvals = argvals)$data)
#     }
#   }
#   temp = fdata(temp, argvals = argvals)
#   return(temp)
# }

convert_to_density = function(fdata)
{
  return(fdata/int.simpson(fdata))
}

clr = function(fdata)
{
  return(try(log(fdata) - 1/diff((fdata$rangeval)) * int.simpson(log(fdata))))
}

inv_clr = function(fdata)
{return(exp(fdata)/int.simpson(exp(fdata)))}

# shift_fdata_range = function(fdata,shift){
#   return(fdata$rangeval - shift)
# }
# 
# assemble_distance_fdata = function(fda_matrix,distance_function, p = 2)
# {
#   rows = dim(fda_matrix$data)[1]
#   to_ret = matrix(nrow = rows, ncol = rows)
#   for ( i in 1:(rows-1))
#   {
#     to_ret[i,i] = 0
#     for ( j in (i+1):rows)
#     {
#       to_ret[i,j] = distance_function(fda_matrix$data[i,],fda_matrix$data[j,],p)
#       to_ret[j,i] = to_ret[i,j]
#     }
#   }
#   to_ret[rows,rows] = 0
#   return(to_ret)
# }

# pam_multiple_distances = function(fdata_trans,distances_list,args_list,data_keys, distance_keys, ncl_vec, dataids)
# {
#   dists = length(distances_list)
#   to_ret = data.frame(data_keys)
#   names = "IDs"
#   for ( i in 1:dists)
#   { 
#     data = fdata_trans[[dataids[i]]]
#     dissimilarity = do.call(distances_list[[i]],args = c(list(data),args_list[[i]]))
#     clusters = pam(dissimilarity, diss = T, k = ncl_vec[i])$clustering
#     to_ret = cbind(to_ret,clusters)
#     names = c(names,paste0(distance_keys[i],"_ncl_",ncl_vec[i]))
#   }
#   names(to_ret) = names
#   return(to_ret)
# }


# Krige_predict_functional = function(traindata, coords_train, coords, krige)
# {
#   nlocs = dim(coords)[1]
#   to_ret_data = matrix(nrow = nlocs, ncol = length(traindata$argvals))
#   for ( i in 1:nlocs)
#   {
#     weigths = krweights(coords_train, coords[i,], krige)
#     to_ret_data[i,] = colSums(traindata$data*weigths)
#   }
#   to_ret_data = fdata(to_ret_data,argvals = traindata$argvals)
#   return(to_ret_data)
# }
#   
  
Distribution_Function = function(fdata_density, newargvals){
  n = length(newargvals)
  m = dim(fdata_density$data)[1]
  to_ret_data = matrix(0, nrow = m, ncol = n)
  for ( i in 2:n){
    idx = which(fdata_density$argvals <=  newargvals[i])
    avals = fdata_density$argvals[idx]
    to_ret_data[,i] = int.simpson(fdata(fdata_density$data[,idx], argvals = avals))
    
  }
  return(fdata(to_ret_data,newargvals))
}

# library(elliptic)
# 
# Kolm_density = function(vals)
# {
#   Fun = theta4(z = 0, q = exp(-2*vals^2)) 
#   return(c(0,diff(Fun)/diff(vals)))
# }


factorization = function (age)
{
  if(age < 50)
    return("0_10")
  if(age < 70)
    return("11_14")
  return("15_21")
}

age_to_fact = Vectorize(factorization)


conditioning = function(density, start, stop)
{ 
  index = which(density$argvals > start & density$argvals< stop)
  args = density$argvals[index]
  data = density$data[,index]
  to_ret = fdata(data,args)
  integral = int.simpson(to_ret)
  return(to_ret/integral)
}

# myfclr = function(density,z,z_step)
# {
#   return(fcenLR(z,z_step,density))
# }


# center_cluster = function(fdata, clustering)
# {
#   clusters = unique(clustering)
#   temp = fdata
#   means = list()
#   for ( c in clusters)
#   {
#     idx = which(clustering == c )
#     means[[c]] = mean(temp[idx])
#     for ( ind in idx)
#       temp$data[ind,] = temp$data[ind,] - means[[c]]$data
#   }
#   return(list(fdata = temp, centers = means))
# }
#   
# 
# decenter_cluster = function(to_sum, clustering)
# {
#   clusters = unique(clustering)
#   temp = to_sum$fdata
#   for ( c in clusters)
#   {
#     idx = which(clustering == c )
#     for ( ind in idx)
#       temp$data[ind,] = temp$data[ind,] + to_sum$means[[c]]$data
#   }
#   return(temp)
# }
  
# center_cluster_numeric = function(data, clustering)
# {
#   clusters = unique(clustering)
#   temp = data
#   means = list()
#   for ( c in clusters)
#   {
#     idx = which(clustering == c )
#     means[[c]] = mean(temp[idx])
#     for ( ind in idx)
#       temp$data[ind,] = temp$data[ind,] - means[[c]]$data
#   }
#   return(list(fdata = temp, centers = means))
# }


fdata_to_ggplot_df = function(fdatalist, names, years, year_labs)
{ 
  to_ret = NULL
  names(year_labs) = years
  for ( i in 1:length(years)){
    year = years[i]
    fdata = fdatalist[[year]]
    n_functions = dim(fdata$data)[1]
    avals = fdata$argvals
    temp_df = data.frame(cbind(names),fdata$data)
    names(temp_df) = c("name", paste0("val",1:length(fdata$argvals)))
    temp_df = temp_df %>% pivot_longer(cols = paste0("val",1:length(fdata$argvals)), values_to = "dens", names_to = "dummy") %>% dplyr::select(name,dens) 
    temp_df$x = rep(avals,n_functions)
    temp_df$Year = rep(year_labs[i], length(fdata$argvals) * n_functions)
    to_ret = rbind(to_ret, temp_df)
  }
  return(to_ret)
}
  
  
roll = function(aggrdate,ndays, freqs, dates)
{
  ret = list()
  r = ndays %% aggrdate
  rolledfreqs = rollapply(c(freqs, rep(0,aggrdate - r)), width = aggrdate,by = aggrdate, FUN = sum, align = "left")
  if ( r == 0) rolledfreqs = rolledfreqs[-length(rolledfreqs)]
  rolleddates = dates[seq(aggrdate/2,ndays, by = aggrdate)] + 0.5
  if(length(rolleddates) != length(rolledfreqs)) rolleddates = c(rolleddates, dates[ndays])
  ret$rolledfreqs = rolledfreqs
  ret$rolleddates = rolleddates
  return(ret)
}

discr_dens = function(dens, length){
  ret = NULL
  dates = dens$argvals[seq(1, length(dens$argvals), length.out = length+1)]
  for ( i in 1:length)
  { 
    inds = dens$argvals >= dates[i] & dens$argvals <= dates[i+1]
    tempdf = fdata(dens$data[,inds],argvals = dens$argvals[inds])
    ret = cbind(ret,int.simpson(tempdf))
  }
  return(ret)
}

  
dum_label = function(names) ## manage labels
{
  temp = rep("", length(names))
  temp[which(names %in% c("Bergamo","Cremona","Lodi","Arezzo") )] = names[which(names %in% c("Bergamo","Cremona","Lodi","Arezzo") )]
  return(temp)
}
  
  
