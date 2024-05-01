#Temp_PmB boxplot 
Temp_PMB %>% 
  filter(!is.na(Temp)) %>% 
  ggplot(aes(x=Temp, y=PMB))+
  geom_boxplot()+
  ylim(0, 12.5)+
  ylab("PmB")+
  scale_x_discrete(labels = as.numeric(seq(min(pigm_a_440$Temp, na.rm = TRUE), max(pigm_a_440$Temp, na.rm = TRUE), by = 1)))+ 
  geom_smooth(se = FALSE, method = "loess", color = "blue", aes(group = 1), span=0.4)+ # Adding a loess curve
  geom_function(fun=function(x) -3.27*10^(-8)*x^7 + 3.4132*10^(-6)*x^6 -1.348*10^(-4)*x^5 + 2.462*10^(-3)*x^4 - 0.0205*x^3 + 0.0617*x^2 + 0.2749*x+1.2956, colour = "red", lwd = 1)+
  ggtitle("a)")

#Temp_PmB scatter plot 
pigm_a_440 %>% 
  filter(!is.na(Temp)) %>% 
  ggplot(aes(x=Temp, y=PMB, colour = Long_prov))+
  geom_point()+
  ylim(0, 12.5)+
  ylab("PmB")+
  geom_smooth(se = FALSE, method = "loess", color = "blue", aes(group = 1), span=0.4)+ # Adding a loess curve
  ggtitle("b)")
