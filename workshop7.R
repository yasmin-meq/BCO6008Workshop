library(tidyverse)
library(tidymodels)
library(skimr)

volcano_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

volcano_raw%>%filter(last_eruption_year!="Unknown") 

volcano%>%count(primary_volcano_type, sort=TRUE) # we have 26 categories but some of them are repeated so we have to fix things!

volcano_df <- volcano_raw %>%
  transmute(volcano_type = case_when(str_detect(primary_volcano_type, "Stratovolcano") ~ "Stratovolcano",
                                     str_detect(primary_volcano_type, "Shield") ~ "Shield",
                                     TRUE ~ "Other"),
            volcano_number, latitude, longitude, elevation, 
            tectonic_settings, major_rock_1) %>%
  mutate_if(is.character, factor)

volcano_df%>%count(volcano_type)

#splitting the data
set.seed(123)

volcano_df_split <- initial_split(volcano_df)
volcano_train <- training(volcano_df_split)
volcano_test <- testing(volcano_df_split)


volcano_df%>%count(volcano_number) #unique variable!
# bootstrapping | we did that because the data is unbalance! 
volcano_boot <- bootstraps(volcano_df)

# recipe 
volcano_rec <- recipe(volcano_type ~. , data = volcano_df)%>%
  update_role(volcano_number, new_role = "Id")%>% # we rename volcano number to Id coz its unique
  step_other(tectonic_settings) %>%
  step_other(major_rock_1) %>%
  step_dummy(tectonic_settings, major_rock_1) %>%
  step_zv(all_predictors()) %>% # zero variance,Keep it in every model 
  step_normalize(all_predictors()) # make sure to normalize all predictors!

volcano_prep <- prep(volcano_rec) # every recipe must finish with prep()

# extract from the one you use in the recipe then you use juice()
juice(volcano_prep)
  
# Note: if you need to apply the recipe to a new data which is not in the recipe data, you have to used bake()


# setting up the model:

install.packages("ranger")
library(ranger)

rf_spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")







