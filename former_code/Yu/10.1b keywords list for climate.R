
vec_climate <- "clima|climát|klima"
vec_carbon <- "carbon|kohlenstoff"
vec_heating_warming <- "calentamiento|réchauffement|rechauffement|global warm|global erwärmung|global erwarmung"
vec_greenhouse <- "invernadero|effet de serre|greenhouse|gewächshaus|gewachshaus|treibhausgas|treibhaus|treibhäus"
vec_deforestation <- "deforest|abholzung|abforst|entwald|déforest|débois|debois"

vec_climate_all <- paste(vec_climate, 
                         vec_carbon, 
                         vec_heating_warming, 
                         vec_greenhouse, 
                         vec_deforestation,
                         sep = "|")

rm(vec_climate, 
   vec_carbon, 
   vec_heating_warming, 
   vec_greenhouse,
   vec_deforestation)
vec_climate_all
