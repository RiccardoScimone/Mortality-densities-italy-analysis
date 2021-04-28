### Very Basic Visualization of Raw Data ####
rm(list = ls())
graphics.off()
source("Scripts/Utility_Functions.R")
load("Output/Data/Aggregated_Municipalities.Rdata")
load("Output/Data/Aggregated_Provinces.Rdata")
load("Output/Data/Aggregated_Regions.Rdata")
#### By regions, choose the one you want to visualize
Regions = c("Veneto","Piemonte","Lombardia","Sicilia","Campania")
Region_colname = "NOME_REGIONE"
years = 11:20
classes = c("0_10","11_14","15_21") ### These are 0-49 years, 50-69 years, 70+ years
gender = "T"
plotfunction = geom_line
regions_aggregated$CL_ETA = factor(regions_aggregated$CL_ETA, levels = c("0_10","11_14","15_21"))
plot_regs = Make_covid_plot(data = regions_aggregated,firstkeys = Regions,firstkeyvarname = Region_colname,years = years,classes = classes,gender = gender,plotfunction = plotfunction,point = F)
x11()
plot(plot_regs)

### By provinces
Provinces = c("Milano","Bergamo", "Brescia", "Napoli", "Messina")
Provincia_colname = "NOME_PROVINCIA"
provinces_aggregated$CL_ETA = factor(provinces_aggregated$CL_ETA, levels = c("0_10","11_14","15_21"))
plot_provs = Make_covid_plot(provinces_aggregated,Provinces,Provincia_colname,years,classes,gender,plotfunction, point = F)
x11()
plot(plot_provs)


#### By municipality
Coms = unique(municipalities_aggregated$COD_PROVCOM[municipalities_aggregated$NOME_COMUNE %in% c("Milazzo","Milano") ])
Comune_colname = "COD_PROVCOM"
municipalities_aggregated$CL_ETA = factor(municipalities_aggregated$CL_ETA, levels = c("0_10","11_14","15_21"))
plot_coms = Make_covid_plot(municipalities_aggregated,Coms,Comune_colname,years,classes,gender,plotfunction, point = F)
x11()
plot(plot_coms)





