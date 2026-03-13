library(tidyverse)
library(MASS)
## Read in data
final_data <- read.csv("final_data.csv", stringsAsFactors = TRUE)

##Going to try only additive to start

#Start with linear model using sqrt lichen cover
lichen_model_linear_additive <- lm(sqrt_lichen_cover~GENUS_NAME+neighbourhood_name+Road, data = final_data)
summary(lichen_model_linear_additive)
par(mfrow=c(2,2))
plot(lichen_model_linear_additive) #QQ Plot shows non-normality
###Regular linear model is bad

#Now try poisson with regualr mean data
lichen_model_poisson_additive <- glm(mean_lichen_cover~GENUS_NAME+neighbourhood_name+Road, family = poisson(link = 'log'), data = final_data)
#Dispersion parameter
lichen_model_poisson_additive$deviance/lichen_model_poisson_additive$df.residual
#3.466879!!!! Too large
par(mfrow=c(2,2))
plot(lichen_model_poisson_additive) #Substansial deviation from normality, but better than regular linear model
summary(lichen_model_poisson_additive)

#Poisson with square root data
sqrtlichen_model_poisson_additive <- glm(sqrt_lichen_cover~GENUS_NAME+neighbourhood_name+Road, 
                                         family = poisson(link = 'log'), data = final_data)
#Dispersion parameter
sqrtlichen_model_poisson_additive$deviance/sqrtlichen_model_poisson_additive$df.residual
#0.5370682!!!! This may be too underdispersed
plot(sqrtlichen_model_poisson_additive) #THis looks better though
summary(sqrtlichen_model_poisson_additive)

#WIll try a negatove binomial
lichen_model_nb_additive <- glm.nb(mean_lichen_cover~GENUS_NAME+neighbourhood_name+Road, data = final_data)
plot(lichen_model_nb_additive) #Still issues at the tail end, maybe OK though??
#Dispersion parameter
lichen_model_nb_additive$deviance/lichen_model_nb_additive$df.residual #Close to 1
summary(lichen_model_nb_additive)

##Now will try an interactive model
#Straight to negative binomial
lichen_model_nb_interactive <- glm.nb(mean_lichen_cover~GENUS_NAME+neighbourhood_name+Road+GENUS_NAME:Road+neighbourhood_name*Road +GENUS_NAME*neighbourhood_name, 
                                      data = final_data) # This ignores interactions between all three variables, which don't seem that interesting and also aren't significant. I feel they kind of cloud results
plot(lichen_model_nb_interactive) #Doesn't look great, but still 
lichen_model_nb_interactive$deviance/lichen_model_nb_interactive$df.residual #Good
summary(lichen_model_nb_interactive)


#Other possibility is full interactive model - maybe makes more sense
final_data$GENUS_NAME <- fct_relevel(final_data$GENUS_NAME, "ACER")
lichen_model_nb_interactive_full_ACER <- glm.nb(mean_lichen_cover~GENUS_NAME*neighbourhood_name*Road, 
                                      data = final_data)
par(mfrow=c(1,2))
plot(lichen_model_nb_interactive_full_ACER) ## A lot better than previous model I think
lichen_model_nb_interactive_full_ACER$deviance/lichen_model_nb_interactive_full_ACER$df.residual #Good
summary(lichen_model_nb_interactive_full_ACER)
#Fagus genus is significant - P=8.73e-08
#Road type significant - P=0.00764
#Interaction between neighbourhood and fagus sig. P=1.79e-05
#Interaction between road type and fagus sig. P=3.50e-09
#Interaction between neighbourhood and road type sig. P=0.04377 (i can't think of a plausible reason for this)

###THis is probably the model we should use

## SOme plots
ggplot(final_data, aes(x=GENUS_NAME, y=mean_lichen_cover)) +
  geom_jitter(position = position_jitter(width = 0.1)) +
  geom_violin(alpha =0.5) +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "black") +
  facet_wrap(~Road)

ggplot(final_data, aes(x=GENUS_NAME, y=mean_lichen_cover)) +
  geom_jitter(position = position_jitter(width = 0.1)) +
  geom_violin(alpha =0.5) +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "black")

ggplot(final_data, aes(x=neighbourhood_name, y=mean_lichen_cover)) +
  geom_jitter(position = position_jitter(width = 0.1)) +
  geom_violin(alpha =0.5) +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "black")

ggplot(final_data, aes(x=Road, y=mean_lichen_cover)) +
  geom_jitter(position = position_jitter(width = 0.1)) +
  geom_violin(alpha =0.5) +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "black")
final_data %>% group_by(Road) %>% summarize(mean = mean(mean_lichen_cover, na.rm = TRUE))

#Run model with different order of factor levels for tree genus
final_data$GENUS_NAME <- fct_relevel(final_data$GENUS_NAME, "CARPINUS")
lichen_model_nb_interactive_full_CARPINUS <- glm.nb(mean_lichen_cover~GENUS_NAME*neighbourhood_name*Road, 
                                           data = final_data)
summary(lichen_model_nb_interactive_full_CARPINUS)

final_data$GENUS_NAME <- fct_relevel(final_data$GENUS_NAME, "FAGUS")
lichen_model_nb_interactive_full_FAGUS <- glm.nb(mean_lichen_cover~GENUS_NAME*neighbourhood_name*Road, 
                                                    data = final_data)
summary(lichen_model_nb_interactive_full_FAGUS)


