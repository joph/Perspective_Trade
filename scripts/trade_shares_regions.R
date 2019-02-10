### Set Working Directory - works in R-Studio only
scriptDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(scriptDir,"/../"))

source("scripts/reFUEL_Functions.R")

reg_match_materials_flows<-
  read_excel("data/figure1_countries_regions.xlsx") %>% 
  as_tibble()

###read bp report
global_consumption_bp<-readGlobalEnergyConsumptionEJ()

###read material flows database and calculate trade
material_flows_tot_trade<-readMaterialFlowsEJ(reg_match_materials_flows)

material_flows_regions<-readMaterialFlowsEJRegions(reg_match_materials_flows) %>% 
  group_by(IPCC_1.5D,Year) %>% summarize(Flows=sum(Flows))

material_flows_regions %>% dplyr::filter(Year==2017) %>% na.omit() %>% 
  ggplot(aes(x=IPCC_1.5D,y=Flows)) + geom_bar(stat="identity")

country_in_file<-
  "data/figure2_countries_regions.xlsx"

bp_countries<-
  read_excel(country_in_file,3) %>% 
  filter(is.na(non_existing_country))

bp_countries_match<-
  read_excel(country_in_file,1) 

energ_cons<-readBPCountry(2,"Demand",bp_countries) %>% 
  mutate(Demand=Demand*PWh_to_TWh*constants$Value[5]*0.0036) %>% 
  full_join(bp_countries_match,by=c("Aggregate_Region"="Region")) %>% 
  group_by(IPCC_1.5D,Year) %>% summarize(Demand=sum(Demand)) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year==2017)
  
full_join(material_flows_regions,energ_cons) %>% na.omit() %>% 
  mutate(share=100*Flows/Demand) %>% 
  ggplot(aes(x=IPCC_1.5D,y=Flows)) + geom_bar(stat="identity")

  
