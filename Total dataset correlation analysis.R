#Total dataset correelation analysis 
library(plotly)
library(ggcorrplot)


corr_df <- subset(pigm_a_440, select= c(PMB, QYmax, Ek, AlphaB,`a*`, `C(npc)`, `[Chl a]`, Fpico, Fnano, Fmicro, Temp, `abs(LAT)`, Depth)) %>% 
  na.omit() %>%
  rename(`ϕmax` = QYmax) %>%  #just copy phi from here for use in future 
  rename(`PmB` = PMB) %>%              #just copy phi from here for use in future 
  filter_all(all_vars(is.finite(.))) 

corr_df <- corr_df %>% 
  filter(`ϕmax` < 0.15) %>%
  filter(Depth < 150)

corr <- round(cor(corr_df), 3)
p_values <- round(cor_pmat(corr_df), 3)

ggcorrplot(corr, hc.order = FALSE, type = "lower", lab = TRUE, p.mat = as.matrix(p_values), sig.level = 0.05)
