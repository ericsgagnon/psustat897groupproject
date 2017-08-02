
invisible( 
  lapply(
    c( 'magrittr' , 'tidyverse' , 'lubridate' , 'data.table' , 'stringr' , 
       'tidyr' , 'broom' , 'purrr' , 'tibble' , 'nlme' , 'ISLR' , 'leaps' , 
       'glmnet' , 'pls' , 'splines', 'gam' , 'e1071' , 'LiblineaR' , 'xtable' , 
       'tree' , 'randomForest' , 'cluster' , 'readr' , 'MASS' , 'purrr' , 'rlang' ,
       'class' , 'gbm' , 'ggthemes' , 'lars' , 'corrgram' , 'ggcorrplot') ,
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
  theme_gdocs() +
  theme( plot.title = element_text( face = 'bold' , size = rel(1.5) , hjust = .5 ) )
}

# Configurations
cfg.seed <- 897
cfg <- list()
##Load Data ###################################################################
data <-
  read_csv("./charity.csv") 

##Initial Formula #############################################################
cfg$predictors <-     
    data %>% 
    names %>% 
    str_subset( '[^ID]' ) %>% 
    str_subset( '[^part]' ) %>% 
    str_subset( '[^donr]' ) %>% 
    str_subset( '[^damt]' )

f.c <- 
    cfg$predictors %>% 
    paste0( collapse = ' + ' ) %>% 
    { paste0( 'donr ~ ' , . ) }
  
f <-
    f.c %>% 
    as.formula
## EDA Visuals ################################################################
p <- list()

# raw data density plots
p$density_raw <- {
    data %>% 
    dplyr::select( avhv , npro , tgif , lgif , rgif , agif , tdon , tlag , incm , inca ) %>% 
    density_plots( 'Density Plots of Raw Data' )
  p$density_raw
}

# log-transformed density plots
p$density_logtransformed <- {
  data %>% 
    dplyr::select( avhv , npro , tgif , lgif , rgif , agif , tdon , tlag , incm , inca ) %>% 
    mutate_all( log ) %>% 
    density_plots( 'Density Plots of Log-Transformed Data' )
  p$density_logtransformed
}

# correlation plot
p$correlations <- {
  data %>% 
  select( cfg$predictors ) %>% 
  select( -matches('reg') ) %>% 
  cor() %>% 
  round( 3 ) %>% 
  ggcorrplot(
    ggtheme = theme_gdocs ,
    legend.title = 'r' ,
    hc.order = T ,
    colors = c('red' , 'white' , 'green') ,
    show.diag = F ,
    outline.color = 'black' ,
    lab = T ,
    lab_col = 'black' ,
    lab_size = rel(2.5) ,
    tl.cex = 13 
  ) + 
  ggtitle( 'Predictor Correlations' ) +
  theme(
    axis.ticks = element_blank() ,
    panel.grid.major = element_blank() ,
    panel.grid.minor = element_blank() ,
    axis.text = element_text(face = 'bold', size = rel(3.5) , hjust = 0 ) ,
    plot.title = element_text( face = 'bold' , size = rel(1.5) , hjust = .5 )
  )
}

# save plots to png
  p %>% 
  names %>% 
  lapply( function(x){  
    ggsave( 
      filename = paste0( 'plot-' ,  x , '.png' ) , 
      plot = p[[x]]  ,
      path = './output' 
    ) 
  })

# Prep Data ###########################################################################

# Visual Inspection of density plots indicates strong skew for each of these predictors
# mutating using log transform
data %<>%
  mutate_at( 
    vars( avhv , tgif , lgif , rgif , agif , tlag , incm , inca ) , 
    log )

# Dimension Reduction #################################################################
  
# Correlation plots indicate several groups of highly correlated predictors:
# perform pca & reg subsets


    
  
# Classification ######################################################################

  
  