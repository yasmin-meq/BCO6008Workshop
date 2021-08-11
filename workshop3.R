library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)

muffin_cupcake_data_original <- read.csv("https://raw.githubusercontent.com/adashofdata/muffin-cupcake/master/recipes_muffins_cupcakes.csv")

muffin_cupcake_data_original%>%
  skim()

#clean variable names using janiror package function 
#make the variables in small later and no spacing!
muffin_cupcake<- muffin_cupcake_data_original %>%
  clean_names()

#splitting our dataset into testing and training 
muffin_cupcake_split <- initial_split(muffin_cupcake)

#training and testing datasets separately 
training_data <- training(muffin_cupcake_split) 
testing_data <- testing(muffin_cupcake_split)

#step1: create a recipe
model_recipe <- recipe(type~ flour+milk+sugar+egg, data=training_data)

#step2: define steps to clean
model_recipe_steps <- model_recipe %>%
  step_impute_mean(all_numeric())%>%
  step_center(all_numeric())%>%
  step_scale(all_numeric())

#step3: prep()
prepped_recipe <- prep(model_recipe_steps, training= training_data)

#step4: bake()
bake_recipe <- bake(prepped_recipe, training_data)


preprocessed_testing_data <- bake(prepped_recipe, testing_data)
#DON'T panic if the numbers are in -ve! this is normal because we scaled our data

  