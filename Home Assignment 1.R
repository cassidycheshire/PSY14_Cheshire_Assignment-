# Assignment 1
# Load the tidyverse, psych, lm.beta, gridExtra, sjPlot, sjmisc, and sjlabelled packages using the library function. Load the surgery data set using the read.csv function.
# Label and view the data 

library(tidyverse)
library(psych)
library(lm.beta)
library(gridExtra)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(car)
library(dplyr)
library(lmtest)
library(lme4)
library(lmerTest) 
library(cAIC4) 	
library(r2glmm) 
library(MuMIn)

surgery_data = read.csv("https://tinyurl.com/ha-dataset1")
View(surgery_data)

# Use the describe function to get information regarding the data set (specifically looking at skew and the max and mins of the variables)
# Search for the variables that might contain outliers 
surgery_data %>%
    describe()

# Noticed that the age, STAI trait, and household income variables have possibly problematic skews and outlying values. The maximum value for age, and minimum values for STAI trait and household income were concerning so I decided to visualize all three mentioned variables.
# Noticed that their is a very low value for IQ in observation 47, but decided not to pursue it because it would be a representative sample and I cannot determine if there is justifiable reason to delete it. 
surgery_data %>%
  ggplot() +
  aes(x = age, color = age) +
  geom_bar(fill = "light blue")

surgery_data %>%
  ggplot() +
  aes(x = STAI_trait, color = STAI_trait) +
  geom_histogram(fill = "light pink")

surgery_data %>%
  ggplot() +
  aes(x = household_income, color = household_income) +
  geom_histogram(fill = "light yellow")

# Found that all three variables above had problematic outliers (age, STAI trait, and household income).
# Decided that the best practice would be to remove these values from the data altogether because they were not realistic (negative number in household income, unrealistically high age, and STAI trait score below twenty(the minimum for STAI)).
# Corrected the data so that these values would be shown as NA.
surgery_data_corrected <- surgery_data %>%
  mutate(age = replace(age, age == "444", NA), STAI_trait = replace(STAI_trait, STAI_trait == "3.9", NA), household_income = replace(household_income, household_income == "-3732", NA))

# Checked to make sure that these corrections successfully occurred.
# Did so by creating old and new plots for the age, STAI trait, and household income variables. Compared them using the grid.arrange function.
surgery_data_oldplot_age <- surgery_data %>%
  ggplot() + 
  aes(x = age, color = age) +
  geom_bar(fill = "light blue")

surgery_data_newplot_age <- surgery_data_corrected %>%
  ggplot() +
  aes(x = age, color = age) +
  geom_bar(fill = "light blue")

grid.arrange(surgery_data_oldplot_age, surgery_data_newplot_age, ncol = 2)

surgery_data_oldplot_STAI <- surgery_data %>%
  ggplot() +
  aes(x = STAI_trait, color = STAI_trait) +
  geom_histogram(fill = "light pink")

surgery_data_newplot_STAI <- surgery_data_corrected %>%
  ggplot() +
  aes(x = STAI_trait, color = STAI_trait) +
  geom_histogram(fill = "light pink")

grid.arrange(surgery_data_oldplot_STAI, surgery_data_newplot_STAI, ncol = 2)

surgery_data_oldplot_household_income <- surgery_data %>%
  ggplot() +
  aes(x = household_income, color = household_income) +
  geom_histogram(fill = "light yellow")

surgery_data_newplot_household_income <- surgery_data_corrected %>%
  ggplot() +
  aes(x = household_income, color = household_income) +
  geom_histogram(fill = "light yellow")

grid.arrange(surgery_data_oldplot_household_income, surgery_data_newplot_household_income, ncol = 2)

surgery_data <- surgery_data_corrected
View(surgery_data)

# Used na.omit function to remove the NA values within the data.
NA_omit_surgery_data <- na.omit(surgery_data)

View(NA_omit_surgery_data)

surgery_data <- NA_omit_surgery_data
View(surgery_data)


# Created the first model for the hierarchical regression containing the predictor variables sex and age. 
# Checked model1 for influential outliers using Cook's distance and the slice function to view the flagged values.
# Decided to keep all flagged values, as the data was realistic and want model to be reflective of the real world.
# Checked model1 to see if the assumptions of linear regression held true.
# Decided that all assumptions of  linear regression held true.

model1 = lm(pain ~ sex + age, data = surgery_data)

model1 %>%
  plot(which = 4)

surgery_data %>%
  slice(c(100, 128, 141))

#Normality
model1 %>%
  plot(which = 2)

residuals_model1 = enframe(residuals(model1))

residuals_model1 %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(model1))

# Linearity
model1 %>%
  residualPlots()

# Homoscedasticity 
model1 %>%
  plot(which = 3)

model1 %>%
  ncvTest()

model1 %>%
  bptest()

# Multicolinearity
model1 %>%
  vif()

# Summarized efficiency of model1
# Visualized predictions of model1 using  plots
# Interpreted statistics of model1 coefficients using the confint and lm.beta functions, and coefficient tab_model function
# Found AIC of model1

model1_summary = summary(model1)
model1_summary

model1_age_plot <- surgery_data %>%
  ggplot() +
  aes(x = age, y = pain) +
  geom_point() +
  geom_point(data = model1, aes(x = age, y = pain), col = "light blue", size = 3) +
  geom_smooth(method = "lm", formula = 'y~x', se = F)
model1_age_plot

model1_sex_plot <- surgery_data %>%
  ggplot() +
  aes(x = sex, y = pain) +
  geom_point() +
  geom_point(data = model1, aes(x = sex, y = pain), col = "light blue", size = 3) +
  geom_smooth(method = "lm", formula = 'y~x', se = F)
model1_sex_plot

grid.arrange(model1_age_plot, model1_sex_plot, nrow = 1)
  
confint(model1)

lm.beta(model1)

Table1 = tab_model(model1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
Table1

AIC(model1)

# Efficiency report of model1: The multiple regression on model1 was significant, explaining 5.29% of the variance in post-surgical pain (F (2,154) = 5.36, p < .01, Adj. R^2 = .05, AIC = 582.93)

# Created the second model for the hierarchical regression containing the predictor variables sex, age, STAI, pain catastrophizing, mindfulness, and cortisol measures.
# Checked model2 for influential outliers using Cook's distance and the slice function to view the flagged values.
# Decided to keep all flagged values, as the data was realistic and want model to be reflective of the real world.
# Checked model2 to see if the assumptions of linear regression held true.
# Decided that the assumptions of normality, linearity, and homoscedasticity  held true. Multicolinearity was violated. 
# Decided to drop the variable of cortisol saliva because cortisol serum is more medically supported.

model2 = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = surgery_data)

model2 %>%
  plot(which = 4)

surgery_data %>%
  slice(c(68, 100, 114))

#Normality
model2 %>%
  plot(which = 2)

residuals_model2 = enframe(residuals(model2))

residuals_model2 %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(model2))

#Linearity
model2 %>%
  residualPlots()

#Homoscedasticity
model2 %>%
  plot(which = 3)

model2 %>%
  ncvTest()

model2 %>%
  bptest()

#Multicolinearity
model2 %>%
  vif()

# Created new model2 with the variable cortisol caliva dropped.
# Checked model2 for influential outliers using Cook's distance and the slice function to view the flagged values.
# Decided to keep all flagged values, as the data was realistic and want model to be reflective of the real world.
# Checked model2 again to see if the assumptions of linear regression held true.
# Decided that all assumptions of linear regression held true.

model2_final = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = surgery_data)

model2_final %>%
  plot(which = 4)

surgery_data %>%
  slice(c(96, 100, 114))

#Normality
model2_final %>%
  plot(which = 2)

residuals_model2_final = enframe(residuals(model2_final))

residuals_model2_final %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(model2_final))

#Linearity
model2_final %>%
  residualPlots()

#Homoscedasticity
model2_final %>%
  plot(which = 3)

model2_final %>%
  ncvTest()

model2_final %>%
  bptest()

#Multicolinearity
model2_final %>%
  vif()

# Summarized efficiency of model2_final
# Visualized predictions of model2_final using  plots
# Interpreted statistics of model2_final coefficients using the confint and lm.beta functions, and coefficient tab_model function
# Found AIC of model2_final

model2_final_summary = summary(model2_final)
model2_final_summary

model2_final_STAItrait_plot <- surgery_data %>%
  ggplot() +
  aes(x = STAI_trait, y = pain) +
  geom_point() +
  geom_point(data = model2_final, aes(x = STAI_trait, y = pain), col = "light pink", size = 3) +
  geom_smooth(method = "lm", formula = 'y~x', se = F)
model2_final_STAItrait_plot

model2_final_paincat_plot <- surgery_data %>%
  ggplot() +
  aes(x = pain_cat, y = pain) +
  geom_point() +
  geom_point(data = model2_final, aes(x = pain_cat, y = pain), col = "light green", size = 3) +
  geom_smooth(method = "lm", formula = 'y~x', se = F)
model2_final_paincat_plot

model2_final_mindfulness_plot <- surgery_data %>%
  ggplot() +
  aes(x = mindfulness, y = pain) +
  geom_point() +
  geom_point(data = model2_final, aes(x = mindfulness, y = pain), col = "orange", size = 3) +
  geom_smooth(method = "lm", formula = 'y~x', se = F)
model2_final_mindfulness_plot

model2_final_cortisol_serum_plot <- surgery_data %>%
  ggplot() +
  aes(x = cortisol_serum, y = pain) +
  geom_point() +
  geom_point(data = model2_final, aes(x = cortisol_serum, y = pain), col = "purple", size = 3) +
  geom_smooth(method = "lm", formula = 'y~x', se = F)
model2_final_cortisol_serum_plot

grid.arrange(model2_final_STAItrait_plot, model2_final_paincat_plot, model2_final_mindfulness_plot, model2_final_cortisol_serum_plot, nrow = 2)

model2_final
confint(model2_final)

lm.beta(model2_final)

Table2 = tab_model(model2_final, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
Table2

AIC(model2_final)

# Efficiency report of model2_final: The multiple regression on model2_final was significant, explaining 47.3% of the variance in post-surgical pain (F(6,150) = 24.33, p <.001, Adj. R^2 = .47, AIC = 494.78)
# Regression equation of model2: Y = 1.86 + .27 * X1 + -0.04 * X2 + -0.02 * X3 + .11 * X4 + -.28 * X5 + .56 * X6 
# Tested for statistically significant improvement in model2 compared to model 1 using adjusted R squared, AIC, and anova 

summary(model1)$adj.r.squared

summary(model2_final)$adj.r.squared

AIC(model1)

AIC(model2_final)

anova(model1, model2_final)

# Report of comparison of 2 models: The hierarchical regression on model2 was significantly better than model1, explaining 47.3% of the variance in post-surgical pain, whereas model1 explained 5.29% of the variance (F(4,150) = 31.68, p < .001, Adj. R^2 = .47, AIC = 494.78

# Assignment 2
# Created backward regression model posed by fellow researcher.
# Conducted check for extraneous outliers using Cook's distance 
# Decided to keep all flagged values, as the data was realistic and want model to be reflective of the real world.
# Conducted linear regression checks on the backward regression model
# Decided that the assumptions for backward regression model held true.
# Conducted the backward regression on the model

backward_regression_model = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + IQ + household_income + weight, data = surgery_data)

backward_regression_model %>%
  plot(which = 4)

surgery_data %>%
  slice(c(3,103,114))

#Normality
backward_regression_model %>%
  plot(which = 2)

residuals_backward_regression_model = enframe(residuals(backward_regression_model))

residuals_backward_regression_model %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(backward_regression_model))

#Linearity
backward_regression_model %>%
  residualPlots()

#Homoscedasticity
backward_regression_model %>%
  plot(which = 3)

backward_regression_model %>%
  ncvTest()

backward_regression_model %>%
  bptest()

#Multicolinearity
backward_regression_model %>%
  vif()

#Backward Regression
backward_model = step(backward_regression_model, direction = "backward")
backward_model

# Summarized efficiency of backward regression model
# Decided to keep all flagged values, as the data was realistic and want model to be reflective of the real world.
# Interpreted statistics of backward regression model coefficients using the confint and lm.beta functions, and coefficient tab_model function
# Found AIC of backward regression model
# Efficiency report of backward regression model: The multiple regression on the backward regression model was significant, explaining 47.25% of the variance in post-surgical pain (F (9,147) = 16.52, p < .001, Adj. R^2 = .47, AIC = 497.75)

summary(backward_regression_model)

confint(backward_regression_model)

lm.beta(backward_regression_model)

tab_model(backward_regression_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

AIC(backward_regression_model)

# Created new model only using the predictors obtained from the backward regression.
# Summarized efficiency of backward model
# Conducted linear regression diagnostics on the model
# Decided that the assumptions for backward regression model held true.
# Interpreted statistics of backward model coefficients using the confint and lm.beta functions, and coefficient tab_model function
# Visualized predictions of new variable in backward model using  plot
# Found AIC of backward model
# Efficiency report of backward model: The multiple regression on the backward model was significant, explaining 47.98% of the variance in post-surgical pain (F (6,150) = 24.98, p < .001, Adj. R^2 = .48, AIC = 492.72)

summary(backward_model)

backward_model %>%
  plot(which = 4)

surgery_data %>%
  slice(c(103, 114, 148))

#Normality
backward_model %>%
  plot(which = 2)

residuals_backward_model = enframe(residuals(backward_model))

residuals_backward_model %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(backward_model))

#Linearity
backward_model %>%
  residualPlots()

#Homoscedasticity
backward_model %>%
  plot(which = 3)

backward_model %>%
  ncvTest()

backward_model %>%
  bptest()

#Multicolinearity
backward_model %>%
  vif()

backward_model_household_plot <- surgery_data %>%
  ggplot() +
  aes(x = household_income, y = pain) +
  geom_point() +
  geom_point(data = backward_model, aes(x = household_income, y = pain), col = "grey", size = 3) +
  geom_smooth(method = "lm", formula = 'y~x', se = F)
backward_model_household_plot

confint(backward_model)

lm.beta(backward_model)

Table3 = tab_model(backward_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
Table3

AIC(backward_model)

# Compared the original model, backward regression model, to the backward model using AIC.
# Report of comparison of 2 models: The hierarchical regression on backward_model was significantly better than backward_regression_model, explaining 47.98% of the variance in post-surgical pain, whereas backward regression model explained 47.25% of the variance (F(6,150) = 24.98, p < .001, Adj. R^2 = .48, AIC = 492.72

summary(backward_regression_model)

summary(backward_model)

AIC(backward_regression_model)
AIC(backward_model)

# Re-named model2 as theory-based-model.
# Created table for coefficients.
# Ran AIC to compare theory_based_model to backward_model. 
# Report of comparison of 2 models: The hierarchical regression on backward_model was significantly better than the theory_based model because of overfitting, explaining 47.98% of the variance in post-surgical pain, whereas theory based model explained 47.29% of the variance (F(6,150) = 24.98, p < .001, Adj. R^2 = .48, AIC = 492.78

model2_final
theory_based_model <- model2_final

summary(theory_based_model)

confint(theory_based_model)

tab_model(theory_based_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

AIC(backward_model)

AIC(theory_based_model)


# Downloaded new data. 
# Used the describe function to get information regarding the data set (specifically looking at skew and the max and mins of the variables)
# Noticed that the max value for mindfulness was an outlier because the scale only goes up to 7 so I decided to visualize the variable.
# Decided that the best practice would be to remove this value from the data altogether because it is not realistic .
# Corrected the data so that this value would be shown as NA.
# Checked to make sure that these corrections successfully occurred.
# Did so by creating old and new plots for the mindfulness variable. Compared them using the grid.arrange function.
# Used na.omit function to remove the NA values within the data.

surgery_data_2 = read.csv("https://tinyurl.com/ha-dataset2")
View(surgery_data_2)

describe(surgery_data_2)

surgery_data_2 %>%
  ggplot() +
  aes(x = mindfulness, color = mindfulness) +
  geom_histogram(fill = "light blue")

surgery_data2_corr <- surgery_data_2 %>%
  mutate(mindfulness = replace(mindfulness, mindfulness == "7.17", NA))

View(surgery_data2_corr)

surgery_data_2_oldplot_mind <- surgery_data_2 %>%
    ggplot() + 
    aes(x = mindfulness, color = mindfulness) +
    geom_bar(fill = "light blue")   

surgery_data_2_newplot_mind <- surgery_data2_corr %>%
  ggplot() + 
  aes(x = mindfulness, color = mindfulness) +
  geom_bar(fill = "light blue")

grid.arrange(surgery_data_2_oldplot_mind, surgery_data_2_newplot_mind)         

NA_omit_surgery_data_2 <- na.omit(surgery_data2_corr)

View(NA_omit_surgery_data_2)

surgery_data2_corr <- NA_omit_surgery_data_2
View(surgery_data2_corr)

# Made predictions on pain using previous regression models theory_based model, backward_model, and new data set.
# Calculated sum of squared residuals.
# Found that the backward regression model has more error than theory-based model by 10.15.

predict_test_theory = predict(theory_based_model, surgery_data2_corr)

predict_test_backward = predict(backward_model, surgery_data2_corr)

RSS_test_theory = sum((surgery_data2_corr[, "pain"] - predict_test_theory)^2)
RSS_test_theory

RSS_test_backward = sum((surgery_data2_corr[, "pain"] - predict_test_backward)^2)
RSS_test_backward

RSS_test_backward - RSS_test_theory

# Assignment 3

# Downloaded data files 3 and 4 
# Used the describe function to get information regarding the data sets (specifically looking at skew and the max and mins of the variables)
# Noticed there was a problematic minimum value in  surgery data 3 and 4 in household income. Also noticed that mindfulness value in 4 was over 6. Decided to visualize using a graph.
# Omitted problematic values from surgery data 3 and 4 and saved as final version of data set.
# Created graph to make sure it worked 

surgery_data_3 = read.csv("https://tinyurl.com/ha-dataset3")
View(surgery_data_3)

surgery_data_4 = read.csv("https://tinyurl.com/ha-dataset4")
View(surgery_data_4)

describe(surgery_data_3)

describe(surgery_data_4)

surgery_data_3 %>%
  ggplot() +
  aes(x = household_income, color = household_income) +
  geom_histogram(fill = "blue")

surgery_data_4 %>%
  ggplot() +
  aes(x = household_income, color = household_income) +
  geom_histogram(fill = "yellow")

surgery_data_4 %>%
  ggplot() +
  aes(x = mindfulness, color = mindfulness) +
  geom_histogram(fill = "pink")

surgery_data_3_corr = surgery_data_3 %>%
  slice(c(-77)) %>%
  mutate(sex = replace(sex, sex == "femlae", "female"))
View(surgery_data_3_corr)

surgery_data_4_corr = surgery_data_4 %>%
  slice(c(-5,-80,-87))
View(surgery_data_4_corr)

surgery_data_4_oldplot_house <- surgery_data_4 %>%
  ggplot() + 
  aes(x = household_income, color = household_income) +
  geom_histogram(fill = "yellow")

surgery_data_4_newplot_house <- surgery_data_4_corr %>%
  ggplot() +
  aes(x = household_income, color = household_income) +
  geom_histogram(fill = "yellow")

grid.arrange(surgery_data_4_oldplot_house, surgery_data_4_newplot_house, ncol = 2)
 
surgery_data_3_oldplot_house <- surgery_data_3 %>%
  ggplot() + 
  aes(x = household_income, color = household_income) +
  geom_histogram(fill = "blue")

surgery_data_3_newplot_house <- surgery_data_3_corr %>%
  ggplot() + 
  aes(x = household_income, color = household_income) +
  geom_histogram(fill = "blue")

grid.arrange(surgery_data_3_oldplot_house, surgery_data_3_newplot_house, ncol = 2)

surgery_data_4_oldplot_mind <- surgery_data_4 %>%
  ggplot() + 
  aes(x = mindfulness, color = mindfulness) +
  geom_histogram(fill = "pink")

surgery_data_4_newplot_mind <- surgery_data_4_corr %>%
  ggplot() + 
  aes(x = mindfulness, color = mindfulness) +
  geom_histogram(fill = "pink")

grid.arrange(surgery_data_4_oldplot_mind, surgery_data_4_newplot_mind)

# Built linear mixed random intercept model on data 3 including predictors from theory based model
# Found coefficients and confidence intervals for predictors in model
# Noted model coefficients for all predictors in comparison to theory based model: 
  # Theory based model:                 Rand_int model: 
    # sex: .27, -.12 - .66                .17, -.08 - .63
    # age: -.04, -.08 - 0                 -.08, -.12 - -.04
    # STAI_trait: -.02, -.07 - .03        .01, -.03 - .05
    # pain_cat: .00, .06 - .17            .04, -.01 - .09
    # mindfulness: -.28, -.52 - -.05      -.24, -.45 - -.02
    # cortisol_serum: .56, .34 - .78      .51, .31 - .70
# Computed marginal variance for fixed effects: .356 (36%)
  # sex: .012
  # age: .073
  # STAI_trait: .002
  # pain_cat: .012
  # mindfulness: .017
  # cortisol_serum: .104
# Computed conditional variance for fixed and random effects: .482 (48%)

surg_rand_int_mod = lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = surgery_data_3_corr)

summary(surg_rand_int_mod)

confint(surg_rand_int_mod)

Table4 = tab_model(surg_rand_int_mod, theory_based_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
Table4

r2beta(surg_rand_int_mod, method = "nsj", data = surgery_data_3_corr)

r.squaredGLMM(surg_rand_int_mod)

# Used surg_rand_int_mod from surgery data 3 to predict pain in surgery data 4
# Computed the variance explained by surg_rand_int on surgery data
# Comparison of model on surg data 3 vs 4: explained marginal variance 37% and conditional variance 49% on surg data 3, and 23% on surg data 4 
predict_surg_int = predict(surg_rand_int_mod, surgery_data_4_corr, allow.new.levels = TRUE)
predict_surg_int

RSS_surg_int = sum((surgery_data_4_corr$pain - predict_surg_int)^2)
RSS_surg_int

surg_int_mod_mean <- lm(pain ~ 1, data = surgery_data_4_corr)	
surg_int_mod_mean

TSS_surg_int = sum((surgery_data_4_corr$pain - predict(surg_int_mod_mean))^2)
TSS_surg_int

variance_surg_int = 1-(RSS_surg_int/TSS_surg_int)
variance_surg_int

# Built new linear mixed model on surgery data 3 predicting pain with only predictor of cortisol serum
# Made random intercept and random intercept slope model
# Used optimizer for slope model
# Compared models to make sure slope model is better. Slope model explains 24.62% marginal variance and 37.87% conditional variance whereas int model explains 24.26% marginal variance and 35.10% conditional variance.

mod_rnd_slope_cort = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = surgery_data_3_corr)

mod_rnd_slope_cort_opt = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = surgery_data_3_corr)

r.squaredGLMM(mod_rnd_slope_cort_opt)

# Visualized fitted regression lines for each hospital by saving both predictions into a new variable 

surgery_data_3_corr = surgery_data_3_corr %>% 		
  mutate(surg_pred_int = predict(surg_rand_int_mod),		
         surg_pred_slope = predict(mod_rnd_slope_cort_opt))

surg_int_plot = surgery_data_3_corr %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 2) +		
  geom_line(color='black', aes(y=surg_pred_int, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)


Figure1 = surg_slope_plot = surgery_data_3_corr %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 2) +		
  geom_line(color='black', aes(y=surg_pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)
Figure1








