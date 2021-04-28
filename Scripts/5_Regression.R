rm(list = ls())
source("Scripts/Utility_Functions.R")
load("Output/Data/Smoothing_Provinces_52.Rdata")
load("Output/Data/Smoothing_municipalities.Rdata")

### Regression models on Provinces
### Create the regression models for each year and class. Set some paramters and utilities:
regr_models_provinces = list()
classe = c("0_10","11_14","15_21")
years = paste0("T_",11:20)
knots = 52
xcp = as.numeric((as.numeric(as.Date("2020-01-01")):as.numeric(as.Date("2020-12-31"))))
knots_vector = floor(xcp[seq(1,length(xcp), length.out = knots)])
to_average = 4

## Fit the models for each year and class
for(class in classe)
{
  regr_models_provinces[[class]] = list()
  mean_previous_provinces = list()
  for(i in (to_average+1):length(years))
  {
    year = years[i]
    argvals = clist_province[[class]][[year]]$argvals
    mean_previous_provinces[[i]] =  clist_province[[class]][[years[i-1]]]$data
    for (y in years[(i - to_average):(i-2)])
      mean_previous_provinces[[i]] = clist_province[[class]][[y]]$data + mean_previous_provinces[[i]]
    mean_previous_provinces[[i]] = mean_previous_provinces[[i]]/to_average
    mean_previous_provinces[[i]] = fdata(mean_previous_provinces[[i]],argvals)
    basis = create.bspline.basis(rangeval = mean_previous_provinces[[i]]$rangeval,breaks = knots_vector)
    model = fregre.basis.fr(mean_previous_provinces[[i]],clist_province[[class]][[year]],basis.s = basis,basis.t = basis,lambda.s = 1e-1,lambda.t = 1e-1)
    regr_models_provinces[[class]][[year]] = list()
    regr_models_provinces[[class]][[year]]$model = model
    print(year)
  }
}

### Assemble predictions and residuals
predictions_province = list()
errors_province = list()
for (class in classe){
  predictions_province[[class]] = list()
  predictions_province[[class]][[years[to_average+1]]] = flist_province[[class]][[years[to_average]]]
  regr_models_provinces[[class]][[years[to_average+1]]]$true_res = norm.fdata(clist_province[[class]][[years[to_average]]] - clist_province[[class]][[years[to_average+1]]])
  for(i in (to_average+1):(length(years)-1))
  {
    year = years[i+1]
    predictions_province[[class]][[year]] = predict(regr_models_provinces[[class]][[years[i]]]$model,new.fdataobj = mean_previous_provinces[[i+1]])
    regr_models_provinces[[class]][[year]]$true_res = norm.fdata(predictions_province[[class]][[year]] - clist_province[[class]][[year]])
    errors_province[[class]][[year]] = clist_province[[class]][[year]] - predictions_province[[class]][[year]]
    predictions_province[[class]][[year]] = inv_clr( predictions_province[[class]][[year]])
  }
}

save(regr_models_provinces, mean_previous_provinces, predictions_province, errors_province, file = Rdata_path("Regression_Province"))


