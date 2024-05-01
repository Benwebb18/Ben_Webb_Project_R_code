library(MASS)
library(factoextra)


PCA_df <- subset(pigm_a_440, select= c(QYmax, PMB, `[Chl a]`, AlphaB, Fpico, Fnano, Fmicro, Ek, `a*`, Temp, `C(npc)`, `abs(LAT)`, Depth)) %>% 
  mutate_all(funs(as.numeric(coalesce(as.character(.), NA)))) %>% 
  na.omit() %>% 
  filter(QYmax<0.15) %>% 
  filter(Depth<150) %>% 
  filter_all(all_vars(is.finite(.))) %>% 
  rename(`ϕmax` = QYmax) 

PCA_df_2 <- subset(pigm_a_440, select= c(QYmax, PMB, AlphaB, `[Chl a]`, Fpico, Fnano, Fmicro, Ek, `a*`, Temp, `C(npc)`, `abs(LAT)`, Depth, Long_prov)) %>% 
  na.omit() %>% 
  filter(QYmax<0.15) %>% 
  filter(Depth<150) %>% 
  filter_all(all_vars(is.finite(.))) %>%     #This creates the df for Long_prov to be used for grouping the samples (cant be used in PCA bcos it's not numeric)
  rename(`ϕmax` = QYmax) 

PCA <- prcomp(PCA_df, scale = TRUE) #Scale = TRUE standardises the data which means the data can be compared without bias 


fviz_eig(PCA, addlabels = TRUE, ncp=6, title = '(b)') #scree plot

fviz_pca_biplot(PCA, title='(a)',  ylim = c(-5,5), label = "var", col.var = 'black', col.ind = 'red', addEllipses = FALSE, habillage = PCA_df_2$Long_prov, pointsize = 1.5, arrowsize = 1, labelsize = 7) #biplot with 'habillage' gicing each long prv a different colour 
