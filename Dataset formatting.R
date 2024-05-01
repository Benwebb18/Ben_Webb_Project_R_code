#Dataset formatting 
library(tidyverse)
library(readr)
library(purrr)
library(broom)
library(GGally)
setwd("C:\\Users\\Lenovo\\OneDrive\\Documents\\Uni work\\4th year project\\R stuff")
Combo_data <- read_csv("Combined dataset.csv")

#install.packages("Matrix") - this was for the geom_quantile package but is unecessary 


###############################################
#Making the variables numeric 
Combo_data$AlphaB <- as.numeric(Combo_data$AlphaB) 
Combo_data$QYmax <- as.numeric(Combo_data$QYmax) 
Combo_data$Ek <- as.numeric(Combo_data$Ek) 
Combo_data$Temp <- as.numeric(Combo_data$Temp)
Combo_data$NO3 <- as.numeric(Combo_data$NO3)
Combo_data$PO4 <- as.numeric(Combo_data$PO4)
Combo_data$`total PPP`<- as.numeric(Combo_data$`total PPP`)
Combo_data$Fpico <- as.numeric(Combo_data$Fpico)
Combo_data$Fmicro <- as.numeric(Combo_data$Fmicro)
Combo_data$`MLD` <- as.numeric(Combo_data$`MLD`)
Combo_data$`Zeu` <- as.numeric(Combo_data$`Zeu`)
Combo_data$`Fnano` <- as.numeric(Combo_data$Fnano)
Combo_data$NPP_index <- as.numeric(Combo_data$NPP_index)
Combo_data$`Fucox` <- as.numeric(Combo_data$Fucox)
Combo_data$`Perid` <- as.numeric(Combo_data$Perid)
Combo_data$`BF19` <- as.numeric(Combo_data$BF19)
Combo_data$`HF19` <- as.numeric(Combo_data$HF19)
Combo_data$`Chl b` <- as.numeric(Combo_data$`Chl b`)
Combo_data$`DVChla` <- as.numeric(Combo_data$`DVChla`)
Combo_data$`Chl a1` <- as.numeric(Combo_data$`Chl a1`)
Combo_data$`Total Chl a`<- as.numeric(Combo_data$`Total Chl a`)
Combo_data$`[Chl a]` <- as.numeric(Combo_data$`Total Chl a`)
Combo_data$Prov_name <- as.factor(Combo_data$Prov_name)


LAT_range <- range(Combo_data$LAT, na.rm = TRUE)
LONG_range <- range(Combo_data$LONG, na.rm = TRUE)


#Normalising total PPP
Combo_data$`PPP:Chl` <- Combo_data$`total PPP` / Combo_data$`[Chl a]`
Combo_data$`PPP:Chl` <- as.numeric(Combo_data$`PPP:Chl`)


#Multiplying PMB by `Chl a` conc to get Pmax 
Pm <- Combo_data$PMB*Combo_data$`[Chl a]`
Combo_data$Pm <- Pm

#Making a* variable (a/Chl a)
Combo_data <- rename(Combo_data, 'a' = 'a(ph)')
Combo_data$a <- as.numeric(Combo_data$a)
Combo_data$`a*` <- as.numeric(Combo_data$a / Combo_data$`[Chl a]`)


#Normalising total PPP
Combo_data$`PPP:Chl` <- Combo_data$`total PPP` / Combo_data$`[Chl a]`

#Making a new column for prediced Pmax 
Predicted_Pm <- Combo_data$a*Combo_data$Ek*Combo_data$QYmax * (0.01201) * 3600  #0.01201 coverts from miligrams to micromoles Carbon, 3600 converts s to hours. 
Combo_data$Predicted_Pm <- Predicted_Pm

#AlphaB * `Chl a` conc to get Alpha 
Alpha <- Combo_data$AlphaB*Combo_data$`[Chl a]`
Combo_data$Alpha <- Alpha 

#making Dataset variable categorical
Combo_data$Dataset <- as.factor(Combo_data$Dataset)

#assigning Olig data a 30 degree Lat (DONT NEED ANYMORE - but useful bit of code to know)
#Olig_lat <- 30.0
#Combo_data$LAT[Combo_data$Dataset == 'Olig'] <- Olig_lat  #very useful bit of code - basically [] means 'where dataset = Olig, make 30 the new value for LAT
Combo_data$LAT <- as.numeric(Combo_data$LAT)
Combo_data$LONG <- as.numeric(Combo_data$LONG)

Combo_data$`C_np(440)` <- as.numeric(Combo_data$`C_np(440)`)
Combo_data$`a_ph(440)` <- as.numeric(Combo_data$`a_ph(440)`)
Combo_data$`a*_ph(440)` <- as.numeric(Combo_data$`a*_ph(440)`)
Combo_data$`a_act(440)` <- as.numeric(Combo_data$`a_act(440)`)
Combo_data$`a*_act(440)` <- as.numeric(Combo_data$`a*_act(440)`)
Combo_data$`a_act(440)/a_ph(440)` <- as.numeric(Combo_data$`a_act/a_ph`)
Combo_data$`a_act` <- as.numeric(Combo_data$`a_act`)
Combo_data$`a*_act` <- as.numeric(Combo_data$`a*_act`)
Combo_data$`a_act/a_ph` <- as.numeric(Combo_data$`a_act/a_ph`)
Combo_data$`C_np(avg)` <- as.numeric(Combo_data$`C_np(avg)`)
##############################################################
#grouping the data into their respective measurements locations to represent a 'station'
#useful for stratification indexes and plotting a or chl maxima 

temp_combo_data <- Combo_data
LAT_intervals <- seq(-90, 90, by = 0.2)
LONG_intervals <- seq(-180, 180, by = 0.2)

strat_combo_data <- temp_combo_data %>% 
  mutate(
    LAT_group = cut(LAT, breaks = LAT_intervals),
    LONG_group = cut(LONG, breaks = LONG_intervals),
    Depth_group = cut_width(Depth, 20, center = 10),
    EK_group = cut_width(Ek, 50, center = 25)
  ) %>%
  group_by(LAT_group, LONG_group) %>%
  mutate(
    strat = ( mean(Temp[Depth < 10 ]) - mean(Temp[Depth > 50]) )  / mean(Temp),
    strat = abs(strat),
    max_NO3 = max(NO3[Depth<15]),   #trying to see if stratification can be quantified by using the max value for NO3 at each grid (it cant)
    max_PO4 = max(PO4[Depth<15]),
    NO3_strat = abs((mean(`NO3`[Depth < 15]) - mean(`NO3`[Depth > 50]))) / mean(`NO3`, na.rm=TRUE), 
    NO3_surface = case_when(Depth<15 ~ mean(`NO3`)),
    PO4_strat = abs((mean(`PO4`[Depth < 15]) - mean(`PO4`[Depth > 50]))) / mean(`PO4`, na.rm=TRUE), 
    PO4_surface = case_when(Depth<15 ~ mean(`PO4`)),
    chl_a_surface = mean(`[Chl a]`[Depth<5]),
    surface_temp = mean(Temp[Depth<15])
  )

Combo_data$NO3_strat <- strat_combo_data$NO3_strat
Combo_data$NO3_surface <- strat_combo_data$NO3_surface
Combo_data$PO4_strat <- strat_combo_data$PO4_strat
Combo_data$PO4_surface <- strat_combo_data$PO4_surface
Combo_data$Chl_a_surface <- strat_combo_data$chl_a_surface
Combo_data$max_PO4 <- strat_combo_data$max_PO4
Combo_data$max_NO3 <- strat_combo_data$max_NO3
Combo_data$surface_temp <- strat_combo_data$surface_temp



Combo_data$strat <- strat_combo_data$strat
Combo_data$strat[is.infinite(Combo_data$strat)] <- NA


Combo_data <- Combo_data %>% 
  mutate(Long_prov = ifelse(Prov_name == 'SPSG' & LAT < -38, 'Coastal', as.character(Long_prov))) %>% #Code to put the anomalous SPSG data in the coastal biome 
  mutate(Long_prov = ifelse(Prov_name == 'NADR', 'Polar', as.character(Long_prov))) %>%
  mutate(Long_prov = as.factor(Long_prov)) %>% 
  
  