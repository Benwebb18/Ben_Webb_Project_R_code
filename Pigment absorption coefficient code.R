#Finding C(npc) using Bricaud pigment abs coefficients 

Bric_coef <- read_csv("Bricaud_coefficients(2004).csv")
Bric_coef$lambda <- as.numeric(Bric_coef$lambda)

long_data <- gather(Bric_coef, key='variable', value='value', -lambda)
long_data$value <- as.numeric(long_data$value)
long_data$type <- character(length=(nrow(long_data)))
long_data$type <- as.character(long_data$type)
long_data$variable <- as.factor(long_data$variable)

for (j in 1:nrow(long_data)) {
  
  i <- long_data[j,1]
  i <- as.numeric(i)
  Bric_coef_i <- Bric_coef %>% 
    filter(lambda == i)
  
  pigm_a_i <- Combo_data[c(11, 12, 16:29)] #need to select specific pigment columns 
  
  
  pigm_a_i$a_Chla <- (Combo_data$`Chl a1`)*Bric_coef_i$`Chl a`
  pigm_a_i$a_DVChla <- (Combo_data$DVChla)*Bric_coef_i$`Dv-Chla`
  pigm_a_i$a_Chlb <- (Combo_data$`Chl b`)*Bric_coef_i$`Chl b`
  pigm_a_i$a_Fucox <- (Combo_data$Fucox)*Bric_coef_i$FUCO
  pigm_a_i$a_BF19 <- (Combo_data$BF19)*Bric_coef_i$`19'-BF`
  pigm_a_i$a_HF19 <- (Combo_data$HF19)*Bric_coef_i$`19'-HF`
  pigm_a_i$a_Perid <- (Combo_data$Perid)*Bric_coef_i$Peri
  pigm_a_i$a_Diadinox <- (Combo_data$Diadinox)*Bric_coef_i$Diad
  pigm_a_i$a_Zea <- (Combo_data$Zea)*Bric_coef_i$Zea
  pigm_a_i$a_Allox <- (Combo_data$Allox)*Bric_coef_i$Allox
  pigm_a_i$a_bcarotene <- (Combo_data$`Carotene-b`)*Bric_coef_i$`beta-Car`
  
  pigm_a_i[is.na(pigm_a_i)] <- 0 #reduces the number of NAs becasue it converts them to zeroes (although not 100% accurate I think it's better than just having NAs in its place)
  
  pigm_a_i$a_CHLA <- pigm_a_i$a_Chla + pigm_a_i$a_DVChla
  pigm_a_i$a_CHLB <- pigm_a_i$a_Chlb
  pigm_a_i$a_PSC <- pigm_a_i$a_Perid + pigm_a_i$a_Fucox + pigm_a_i$a_BF19 + pigm_a_i$a_HF19
  pigm_a_i$a_NPC <- pigm_a_i$a_Zea + pigm_a_i$a_Allox + pigm_a_i$a_Diadinox
  pigm_a_i$`a*_NPC` <- pigm_a_i$a_NPC / pigm_a_i$a_CHLA
  
  
  pigm_a_i$a_total <- pigm_a_i$a_CHLA + pigm_a_i$a_CHLB + pigm_a_i$a_NPC + pigm_a_i$a_PSC
  pigm_a_i$`C(npc)` <- pigm_a_i$a_NPC / pigm_a_i$a_total
  pigm_a_i$`C(ppp)` <- 1 - (pigm_a_i$`C(npc)`)
  
  pigm_a_i$Ek <- as.numeric(Combo_data$Ek)
  pigm_a_i$QYmax <- as.numeric(Combo_data$QYmax) #adding QYmax to each dataframe so i can plot it against the absorption ratios 
  pigm_a_i$Fpico <- as.numeric(Combo_data$Fpico)
  pigm_a_i$Fmicro <- as.numeric(Combo_data$Fmicro)
  pigm_a_i$Fnano <- as.numeric(Combo_data$Fnano)
  pigm_a_i$Long_prov <- as.factor(Combo_data$Long_prov)
  pigm_a_i$Prov_name <- as.factor(Combo_data$Prov_name)
  pigm_a_i$chla_a_surface <- as.numeric(Combo_data$Chl_a_surface)
  pigm_a_i$`abs(LAT)` <- as.numeric(abs(Combo_data$LAT))
  pigm_a_i$LONG <- as.numeric(Combo_data$LONG)
  pigm_a_i$Depth <- as.numeric(Combo_data$Depth)
  pigm_a_i$PMB <- as.numeric(Combo_data$PMB)
  pigm_a_i$a_ph <- as.numeric(Combo_data$a)
  pigm_a_i$AlphaB <- as.numeric(Combo_data$AlphaB)
  pigm_a_i$Temp <- as.numeric(Combo_data$Temp)
  pigm_a_i$`a*` <- as.numeric(Combo_data$`a*`)
  pigm_a_i$`[Chl a]` <- as.numeric(Combo_data$`[Chl a]`)
  pigm_a_i$LAT <- as.numeric(Combo_data$LAT)
  
  name <- sprintf('pigm_a_%d', i ) #allows the name to change dynamically depending on the wavlength 
  assign(name, pigm_a_i)
  
}

