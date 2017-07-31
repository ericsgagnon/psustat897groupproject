#dev
data.train <-
  data %>% 
  filter( part == 'train' )

data.validate <-
  data %>% 
  filter( part == 'valid' )

data.test <-
  data %>% 
  filter( part == 'test' )

f.c <- 
  data %>% 
  names %>% 
  str_subset( '[^donr]' ) %>% 
  str_subset( '[^damt]' ) %>% 
  str_subset( '[^part]' ) %>% 
  str_subset( '[^ID]' ) %>% 
  paste0( collapse = ' + ' ) %>% 
  { paste0( 'donr ~ ' , . ) }

f <-
  f.c %>% 
  as.formula

##1. Classification ###########################################################
m <- list()

m$binomial <-
  data %>% 
  filter( part == 'train' ) %$%
  glm( f , . , family = binomial('logit') )

m$lda <-
  data %>% 
  filter( part == 'train' ) %$%
  lda( f , . )

m$qda <-
  data %>% 
  filter( part == 'train' ) %$%
  qda( f , . )




# XXX #####################################################
regsubsets()

set.seed(cfg.seed)
m$knn <-
  data.train %$% 
  knn( . , . , donr , )

map

#m$kmeans <-
#data %>% 

kn <-
  tibble( k = 1:15 ) %>% 
  group_by( k ) %>% 
  do( 
    kn = knn( 
      select( data.train , -ID , -damt , -part ), 
      select( data.validate , -ID , -damt , -part ) ,  
      data.train$donr , 
      .$k ) 
  )

clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], select( data.train , -ID , -damt , -part )))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))




kclusts <- 
  data.frame(k=1:9) %>% 
  group_by(k) %>% 
  do(kclust=kmeans(select( data.train , -ID , -damt , -part ), .$k))  
ggplot(assignments, aes(x1, x2)) + geom_point(aes(color=.cluster)) + facet_wrap(~ k)  

m$hclust <-
  
  
  
  
  ###############################################################################
data %>% 
  filter( part == 'train' ) %>% 
  ggplot( aes( agif , rgif ) ) +
  geom_point()

data %>% 
  filter( part == 'train' ) %>% 
  ggplot( aes( agif , lgif ) ) +
  geom_point()

data %>% 
  summarise( min( rgif ) , max( rgif ))

m.lda <- 
  data %>% 
  filter( part == 'train' ) %$% 
  lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
        avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif ) # include additional terms on the fly using I()



m.lda.predict <-
  m.lda %>% 
  predict() %>% 
  {.$posterior[,2]}

m.lda.predict 


##Sample Script######################################
# Classification modeling

# linear discriminant analysis


model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5

# logistic regression

model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1291.0 11642.5

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 310+981 = 1291
# check profit = 14.5*981-2*1291 = 11642.5

# Results

# n.mail Profit  Model
# 1329   11624.5 LDA1
# 1291   11642.5 Log1

# select model.log1 since it has maximum profit in the validation sample

post.test <- predict(model.log1, data.test.std, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.log1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1676  331
# based on this model we'll mail to the 331 highest posterior probabilities

# See below for saving chat.test into a file for submission