library(tidyverse)
library(MASS)
library(emmeans)
library(car)
library(broom)


## Read in data
final_data <- read.csv("final_data.csv", stringsAsFactors = TRUE) %>% 
  mutate(sqrt_lichen_cover = sqrt(mean_lichen_cover))

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
par(mfrow=c(2,2))
plot(lichen_model_nb_interactive_full_ACER) ## A lot better than previous model I think
lichen_model_nb_interactive_full_ACER$deviance/lichen_model_nb_interactive_full_ACER$df.residual #Good
summary(lichen_model_nb_interactive_full_ACER)## Not useful for out purposes

#Run a type II anova to compare broadly across factor levels
Anova(lichen_model_nb_interactive_full_ACER, type = "II")

##Want to try pairwise comparisons
comps_full <- emmeans(lichen_model_nb_interactive_full_ACER, specs = ~ GENUS_NAME:Road:neighbourhood_name, adjust = "Tukey")
contrast(comps_full, method = "pairwise")




###THis is probably the model we should use


bryo_model_nb_interactive_full <- glm.nb(mean_bryo_cover~GENUS_NAME*neighbourhood_name*Road, 
                                           data = final_data)

Anova(bryo_model_nb_interactive_full, type = "II")
comps_full_bryo <- emmeans(bryo_model_nb_interactive_full, specs = ~ GENUS_NAME:Road:neighbourhood_name, adjust = "Tukey")
contrast(comps_full_bryo, method = "pairwise", by = "GENUS_NAME")


par(mfrow=c(2,2))
plot(bryo_model_nb_interactive_full) ## A lot better than previous model I think
bryo_model_nb_interactive_full$deviance/bryo_model_nb_interactive_full$df.residual #Good
summary(bryo_model_nb_interactive_full)
tidy(bryo_model_nb_interactive_full, exponentiate=TRUE)

#bryo_model_poisson_interactive_full <- glm(mean_bryo_cover~GENUS_NAME*neighbourhood_name*Road, 
 #                                        data = final_data, family=poisson)

#ztidy(bryo_model_poisson_interactive_full, exponentiate=TRUE)
#plot(bryo_model_poisson_interactive_full)
#bryo_model_poisson_interactive_full$deviance/bryo_model_poisson_interactive_full$df.residual 

