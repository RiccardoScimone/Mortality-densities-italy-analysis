####This script aggregates the orignal dataset at comunal, provincial, regional level, with the prescribed 
### aggregation at age level, and saves the resulting list of dataframes. It also saves the comuni, provinces,
### and region names
rm(list = ls())
source("Scripts/Utility_Functions.R") #Load packages and functions which are used in the project

########## This chunk of code removes artificial NA (e.g. 29 february for non leap years) and provides better date format #####
modified_date_dataset = read.csv("RawData/comuni_giornaliero_31dicembre.csv", header = T)
modified_date_dataset$GE = as.character(modified_date_dataset$GE)
months = str_sub(modified_date_dataset$GE,1,-3)
days = str_sub(modified_date_dataset$GE,-2,-1)
modified_date_dataset$partial_date_death = as.Date(paste0("2020-",months,"-",days))
modified_date_dataset$CL_ETA = as.character(modified_date_dataset$CL_ETA)
modified_date_dataset$COD_PROVCOM = as.character(modified_date_dataset$COD_PROVCOM)
modified_date_dataset$T_20 = as.numeric(modified_date_dataset$T_20)
modified_date_dataset$F_20 = as.numeric(modified_date_dataset$F_20)
modified_date_dataset$M_20 = as.numeric(modified_date_dataset$M_20)
modified_date_dataset = modified_date_dataset %>% replace(is.na(.),0)
modified_date_dataset = modified_date_dataset %>% dplyr::select(-GE)
save(modified_date_dataset, file = Rdata_path("modified_istat_dataset")) 
#### Note: All dates are specified at 2020 for simplicity, but the actual year is specified in each column.
rm(days)
rm(months)


#### Aggragation by age (See notation in the pdf from ISTAT) ######
firstvec = c(0,11,15)
lastvec = c(10,14,21)
#### So we are aggregating 0-49 years, 50-69 years, 70+ years #####

#### Aggregate on age basis the Municipality data and carry useful information. This will require some time ############
municipalities_codes = sort(unique(modified_date_dataset$COD_PROVCOM))
municipalities_data = modified_date_dataset %>% group_by(COD_PROVCOM,CL_ETA)
municipalities_georef = modified_date_dataset %>% arrange(COD_PROVCOM) %>% dplyr::select(COD_PROVCOM,NOME_PROVINCIA,NOME_REGIONE,NOME_COMUNE) %>% distinct(COD_PROVCOM,.keep_all = TRUE)
rm(modified_date_dataset)
municipalities_aggregated = aggregate_classes(municipalities_data,keyname = "COD_PROVCOM",firstvec,lastvec)
municipalities_aggregated = municipalities_aggregated %>% left_join(municipalities_georef, by = c("COD_PROVCOM" = "COD_PROVCOM"))

save(list = c("municipalities_aggregated","municipalities_codes"), file = Rdata_path("Aggregated_Municipalities"))



#### Same thing on regions ##################
load("Output/Data/modified_istat_dataset.Rdata")
regions_data = modified_date_dataset %>% group_by(NOME_REGIONE,CL_ETA,partial_date_death) %>%
summarise_at(c(paste0("M_",c(11:20)),paste0("F_",c(11:20)),paste0("T_",c(11:20))), sum)
regions_aggregated = aggregate_classes(regions_data,keyname = "NOME_REGIONE",firstvec,lastvec)
save(regions_aggregated, file = Rdata_path("Aggregated_Regions"))

### Same thing on provinces ##################
provinces_names = sort(unique(modified_date_dataset$NOME_PROVINCIA))
provinces_data = modified_date_dataset %>% group_by(NOME_PROVINCIA,CL_ETA,partial_date_death) %>%
summarise_at(c(paste0("M_",c(11:20)),paste0("F_",c(11:20)),paste0("T_",c(11:20))), sum) 
provinces_aggregated = aggregate_classes(provinces_data,keyname = "NOME_PROVINCIA",firstvec,lastvec)
save(list = c("provinces_aggregated","provinces_names"), file = Rdata_path("Aggregated_Provinces"))


### Here we enrich our data with additional information on the population. We have population information
### by age, for each municipality and provincia, in 2020. Let us start with municipalities:
load("Output/Data/Aggregated_Municipalities.Rdata")
source("Scripts/Utility_Functions.R")
### retrieve resident by age in each municipality. Some data field is in italian
municipality_data = read.csv("RawData/Municipality_Population_2020.csv")
municipality_data = municipality_data %>% dplyr::select(Codice.comune, EtÃ., Totale.Maschi, Totale.Femmine) %>% 
  filter(EtÃ. != 999) %>% mutate(istat = Codice.comune, num_residenti = Totale.Maschi + Totale.Femmine) %>% 
  dplyr::select(istat,num_residenti, EtÃ.,Totale.Maschi, Totale.Femmine)

### Now we perform aggregation, see factorization function in Utility_functions, which is written according to our aggregation
municipality_data = municipality_data %>% mutate(CL_ETA = age_to_fact(EtÃ.)) %>% dplyr::select(-EtÃ.)
municipality_data = municipality_data %>% group_by(istat, CL_ETA) %>% summarise_at(c("Totale.Femmine", "Totale.Maschi", "num_residenti"),sum)

### We align codes (Three Comuni are missing, God knows why)
municipality_data$istat = as.character(municipality_data$istat)
diff1 = setdiff(municipality_data$istat,municipalities_codes)
diff2 = setdiff(municipalities_codes,municipality_data$istat)
municipality_data = municipality_data %>% filter(!(istat %in% diff1))
municipalities_aggregated = municipalities_aggregated %>% filter(!(COD_PROVCOM %in% diff2))
municipalities_codes = unique(municipalities_aggregated$COD_PROVCOM)
save(list = c("municipalities_aggregated","municipalities_codes", "municipality_data"), file = Rdata_path("Aggregated_Municipalities"))


#### Now we repeat with provinces. we have to adjust same names
load("Output/Data/Aggregated_Provinces.Rdata")
source("Scripts/Utility_Functions.R")
provinces_data = read.csv("RawData/Provinces_Population_2020.csv")
provinces_data = provinces_data %>% dplyr::select(Provincia, EtÃ., Totale.Maschi, Totale.Femmine) %>% 
  filter(EtÃ. != "Totale") %>% mutate(provincia = Provincia, num_residenti = Totale.Maschi + Totale.Femmine) %>% 
  dplyr::select(provincia,num_residenti, EtÃ.,Totale.Maschi, Totale.Femmine)
provinces_data$EtÃ. = as.numeric(provinces_data$EtÃ.)
provinces_data = provinces_data %>% mutate(CL_ETA = age_to_fact(EtÃ.)) %>% dplyr::select(-EtÃ.)
provinces_data = provinces_data %>% group_by(provincia, CL_ETA) %>% summarise_at(c("Totale.Femmine", "Totale.Maschi", "num_residenti"),sum)

diff1 = setdiff(provinces_data$provincia,provinces_aggregated$NOME_PROVINCIA)
diff2 = setdiff(provinces_aggregated$NOME_PROVINCIA,provinces_data$provincia)

provinces_data$provincia[provinces_data$provincia == "Valle d'Aosta/VallÃ©e d'Aoste"] = "Valle d'Aosta/Vallée d'Aoste"
provinces_data$provincia[provinces_data$provincia == "Reggio di Calabria"] = "Reggio Calabria"
provinces_data$provincia[provinces_data$provincia == "ForlÃ¬-Cesena"] = "Forlì-Cesena"

save(list = c("provinces_aggregated","provinces_names", "provinces_data"), file = Rdata_path("Aggregated_Provinces"))


#### We now focus on the shapefiles for comuni and province. Some data field are in Italian
source("Scripts/Utility_Functions.R")

prov_poly = readOGR("RawData/Shapefile_Provinces/ProvCM01012018_g_WGS84.shp")

prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Aosta"] = "Valle d'Aosta/Vallée d'Aoste"
prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Bolzano"] = "Bolzano/Bozen" 
prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Massa Carrara"] = "Massa-Carrara" 
prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Forli'-Cesena"] = "Forlì-Cesena" 
prov_poly$DEN_PCM[prov_poly$DEN_PCM == "Reggio di Calabria"] = "Reggio Calabria"

save(prov_poly, file = Rdata_path("Province_geometry_polygons"))

com_poly = readOGR("RawData/Shapefile_Municipalities/Com01012020_WGS84.shp")
load("Output/Data/Aggregated_Municipalities.Rdata")

ind_to_drop = which(com_poly$PRO_COM %in% setdiff(com_poly$PRO_COM,unique(municipalities_aggregated$COD_PROVCOM)))
com_poly = com_poly[-ind_to_drop,]

save(com_poly, file = Rdata_path("Comuni_geometry_polygons"))

#### Lastly, we create dataset aggregated by year, to perform the scalar LISA analysis
source("Scripts/Utility_Functions.R")
load("Output/Data/Aggregated_Municipalities.Rdata")
load("Output/Data/Aggregated_Provinces.Rdata")
municipalities_aggregated_lisa = municipalities_aggregated %>% group_by(COD_PROVCOM,CL_ETA) %>% summarise_at(c(paste0("M_",11:20),paste0("F_",11:20),paste0("T_",11:20)),sum)
municipalities_aggregated_lisa = municipalities_aggregated_lisa %>% left_join(municipality_data, by = c("COD_PROVCOM" = "istat","CL_ETA" = "CL_ETA")) %>% left_join(municipalities_georef, by = c("COD_PROVCOM" = "COD_PROVCOM"))
provinces_aggregated_lisa = provinces_aggregated %>% group_by(NOME_PROVINCIA,CL_ETA) %>% summarise_at(c(paste0("M_",11:20),paste0("F_",11:20),paste0("T_",11:20)),sum)
provinces_aggregated_lisa = provinces_aggregated_lisa %>% left_join(provinces_data, by = c("NOME_PROVINCIA" = "provincia","CL_ETA" = "CL_ETA"))

#### We want to divide deaths by residents

provinces_aggregated_lisa[3:32] = provinces_aggregated_lisa[3:32] / provinces_aggregated_lisa$num_residenti
municipalities_aggregated_lisa[,3:32] = municipalities_aggregated_lisa[,3:32] / municipalities_aggregated_lisa$num_residenti


save(municipalities_aggregated_lisa, file = Rdata_path("Municipality_data_scalar"))
save(provinces_aggregated_lisa, file = Rdata_path("Province_data_scalar"))

