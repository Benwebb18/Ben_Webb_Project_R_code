#Log correlation matrices (domain-specific)

#coastal
Coastal_log_corr_df  <- subset(pigm_a_440, select= c(PMB, QYmax, Ek, AlphaB,`a*`, `C(npc)`, `[Chl a]`, Fpico, Fnano, Fmicro, Temp, `abs(LAT)`, Depth, Long_prov, Prov_name, LAT)) %>% 
  mutate(Long_prov = ifelse(Prov_name == 'SPSG' & LAT < -38, 'Coastal', as.character(Long_prov))) %>% #Code to put the anomalous SPSG data in the coastal biome 
  mutate(Long_prov = ifelse(Prov_name == 'NADR', 'Polar', as.character(Long_prov))) %>%
  mutate(Long_prov = as.factor(Long_prov)) %>%  
  filter(QYmax<0.15, Depth<150, Long_prov == 'Coastal') 

Coastal_log_corr_df  <- Coastal_log_corr_df  %>% 
  select(-c(Long_prov, Prov_name, LAT)) %>% 
  mutate_all(funs(log(.)))

Coastal_log_corr_df  <- Coastal_log_corr_df  %>% 
  na.omit() %>% 
  rename(`ϕmax` = QYmax) %>%              #just copy phi from here for use in future 
  filter_all(all_vars(is.finite(.)))

Coastal_log_corr <- round(cor(Coastal_log_corr_df), 3)
p_values_coastal <- round(cor_pmat(Coastal_log_corr_df), 3)


ggcorrplot(Coastal_log_corr, hc.order = FALSE, type = "lower", lab = TRUE, p.mat=as.matrix(p_values_coastal))+
  labs(title="Coastal correlation matrix (log values)")


#polar
Polar_log_corr_df  <- subset(pigm_a_440, select= c(PMB, QYmax, Ek, AlphaB,`a*`, `[Chl a]`, Temp, `abs(LAT)`, Depth, Long_prov, Prov_name, LAT)) %>% 
  mutate(Long_prov = ifelse(Prov_name == 'SPSG' & LAT < -38, 'Polar', as.character(Long_prov))) %>% #Code to put the anomalous SPSG data in the Coastal biome 
  mutate(Long_prov = ifelse(Prov_name == 'NADR', 'Polar', as.character(Long_prov))) %>%
  mutate(Long_prov = as.factor(Long_prov)) %>%  
  filter(QYmax<0.15, Depth<150, Long_prov == 'Polar') 

Polar_log_corr_df  <- Polar_log_corr_df  %>% 
  select(-c(Long_prov, Prov_name, LAT)) %>% 
  mutate_all(funs(log(.)))

Polar_log_corr_df  <- Polar_log_corr_df  %>% 
  na.omit() %>% 
  rename(`ϕmax` = QYmax) %>%              #just copy phi from here for use in future 
  filter_all(all_vars(is.finite(.)))

Polar_log_corr <- round(cor(Polar_log_corr_df), 3)
p_values_Polar <- round(cor_pmat(Polar_log_corr_df), 3)


ggcorrplot(Polar_log_corr, hc.order = FALSE, type = "lower", lab = TRUE, p.mat=as.matrix(p_values_Polar))+
  labs(title="Polar correlation matrix (log values)")

#Trades
Trades_log_corr_df  <- subset(pigm_a_440, select= c(PMB, QYmax, Ek, AlphaB,`a*`, `C(npc)`, `[Chl a]`, Fpico, Fnano, Fmicro, Temp, `abs(LAT)`, Depth, Long_prov)) %>% 
  filter(QYmax<0.15, Depth<150, Long_prov == 'Trades')

Trades_log_corr_df  <- Trades_log_corr_df  %>% 
  select(-c(Long_prov)) %>% 
  mutate_all(funs(log(.)))

Trades_log_corr_df  <- Trades_log_corr_df  %>% 
  na.omit() %>% 
  rename(`ϕmax` = QYmax) %>%              #just copy phi from here for use in future 
  filter_all(all_vars(is.finite(.)))

Trades_log_corr <- round(cor(Trades_log_corr_df), 3)
p_values_Trades <- round(cor_pmat(Trades_log_corr_df), 3)


ggcorrplot(Trades_log_corr, hc.order = FALSE, type = "lower", lab = TRUE, p.mat =as.matrix(p_values_Trades), sig.level = 0.05 )+
  labs(title="Trades correlation matrix (log values)")

#Westerlies
Westerlies_log_corr_df  <- subset(pigm_a_440, select= c(PMB, QYmax, Ek, AlphaB,`a*`, `C(npc)`, `[Chl a]`, Fpico, Fnano, Fmicro, Temp, `abs(LAT)`, Depth, Long_prov)) %>% 
  filter(QYmax<0.15, Depth<150, Long_prov == 'Westerlies')

Westerlies_log_corr_df  <- Westerlies_log_corr_df  %>% 
  select(-c(Long_prov)) %>% 
  mutate_all(funs(log(.)))

Westerlies_log_corr_df  <- Westerlies_log_corr_df  %>% 
  na.omit() %>% 
  rename(`ϕmax` = QYmax) %>%              #just copy phi from here for use in future 
  filter_all(all_vars(is.finite(.)))

Westerlies_log_corr <- round(cor(Westerlies_log_corr_df), 3)
p_values_Westerlies <- round(cor_pmat(Westerlies_log_corr_df), 3)


ggcorrplot(Westerlies_log_corr, hc.order = FALSE, type = "lower", lab = TRUE, p.mat=as.matrix(p_values_Westerlies))+
  labs(title="Westerlies correlation matrix (log values)")


