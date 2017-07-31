
invisible( 
  lapply(
    c( 'magrittr' , 'tidyverse' , 'lubridate' , 'data.table' , 'stringr' , 
       'tidyr' , 'broom' , 'purrr' , 'tibble' , 'nlme' , 'ISLR' , 'leaps' , 
       'glmnet' , 'pls' , 'splines', 'gam' , 'e1071' , 'LiblineaR' , 'xtable' , 
       'tree' , 'randomForest' , 'cluster' , 'readr' , 'MASS' , 'purrr' , 'rlang' ,
       'class' , 'gbm' , 'ggthemes' ) ,
    require , character.only = T ) )

# Helper Functions 
density_plots <- function( df , main = 'Density Plots') {
  df %>% 
  gather(
    key = measure ,
    value = value 
  ) %>% 
  ggplot( 
    aes(
      value ,
      color = measure ,
      group = measure
  )) +
  geom_density() +
  facet_wrap( ~measure  , scales = 'free' ) +
  ggtitle( label = main ) +
  theme_gdocs()
}

# Configurations
cfg.seed <- 897

##Load Data ###################################################################

data <-
  read_csv("./charity.csv") 
## Visuals ####################################################################
p <- list()

# raw data density plots
  p$density_raw <-
    data %>% 
    dplyr::select( avhv , npro , tgif , lgif , rgif , agif , tdon , tlag , incm , inca ) %>% 
    density_plots( 'Density Plots of Raw Data' )
  p$density_raw
  ggsave( 'plot-density-raw-data.png' , plot = p$density_raw)  

# log-transformed density plots
  p$density_logtransformed <-
  data %>% 
    dplyr::select( avhv , npro , tgif , lgif , rgif , agif , tdon , tlag , incm , inca ) %>% 
    mutate_all( log ) %>% 
    density_plots( 'Density Plots of Log-Transformed Data' )
  p$density_logtransformed
  ggsave( 'plot-density-log-transformed.png' , plot = p$density_logtransformed)  

# Prep Data ###########################################################################
# Visual Inspection of density plots indicates strong skew for each of these predictors
# mutating using log transform
data %<>%
  mutate_at( 
    vars( avhv , tgif , lgif , rgif , agif , tdon , tlag , incm , inca ) , 
    log )


