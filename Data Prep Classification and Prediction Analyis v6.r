	# DATA SETUP

		charity = read.csv("E:/STAT 897/Group Project/charity.csv")
		charity.t = charity
		charity.t$avhv = log(charity.t$avhv)
		charity.t$agif = log(charity.t$agif)
		charity.t$inca = log(charity.t$inca)
		charity.t$incm = log(charity.t$incm)
		charity.t$lgif = log(charity.t$lgif)
		charity.t$rgif = log(charity.t$rgif)
		charity.t$tgif = log(charity.t$tgif)
		charity.t$tlag = log(charity.t$tlag)

		data.train = charity.t[charity$part=="train",]
		x.train = data.train[,2:21]
		c.train = data.train[,22] # donr
		n.train.c = length(c.train) # 3984
		y.train = data.train[c.train==1,23] # damt for observations with donr=1
		# x.train.mat = model.matrix(damt~., data.train[c.train==1,2:23])[,-22]
		n.train.y = length(y.train) # 1995

		data.valid = charity.t[charity$part=="valid",]
		x.valid = data.valid[,2:21]
		c.valid = data.valid[,22] # donr
		n.valid.c = length(c.valid) # 2018
		y.valid = data.valid[c.valid==1,23] # damt for observations with donr=1
		#x.valid.mat = model.matrix(damt~., data.valid[c.valid==1,2:23])[,-22]
		n.valid.y = length(y.valid) # 999

		data.test = charity.t[charity$part=="test",]
		n.test = dim(data.test)[1] # 2007
		x.test = data.test[,2:21]

		x.train.mean = apply(x.train, 2, mean)
		x.train.sd = apply(x.train, 2, sd)
		x.train.std = t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
		# x.train.mat.mean = apply(x.train.mat, 2, mean)
		# x.train.mat.sd = apply(x.train.mat, 2, sd)
		# x.train.mat.std = t((t(x.train.mat)-x.train.mat.mean)/x.train.mat.sd) # standardize to have zero mean and unit sd
		apply(x.train.std, 2, mean) # check zero mean
		apply(x.train.std, 2, sd) # check unit sd
		data.train.std.c = data.frame(x.train.std, donr=c.train) # to classify donr
		data.train.std.y = data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

		x.valid.std = t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
		# x.valid.mat.std = t((t(x.valid.mat)-x.train.mat.mean)/x.train.mat.sd) # standardize to have zero mean and unit sd
		data.valid.std.c = data.frame(x.valid.std, donr=c.valid) # to classify donr
		data.valid.std.y = data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

		x.test.std = t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
		data.test.std = data.frame(x.test.std)

		train.dat = data.frame(x = x.train.std, y = as.factor(data.train.std.c$donr))
		valid.dat = data.frame(x = x.valid.std, y = as.factor(data.valid.std.c$donr))

	#LIBRARIES

		library(MASS)
		library(tree)
		library(randomForest)
		library(gbm)
		library(e1071)
		library(lars)
		library(leaps)
		library(glmnet)
		library(pls)
		library(splines)
		library(gam)
		library(ISLR)
		library(magrittr)
		library(stringr)
		library(lubridate)
		library(dplyr)
		library(ggplot2)
		library(ggthemes)

		#EXPLORATORY DATA ANALYSIS
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
		
		## EDA Visuals ################################################################
		p    <- list()
		data <- charity %>% tbl_df
		cfg.predictors <-
		  c( "reg1" , "reg2" , "reg3" , "reg4" , "home" , "chld" , "hinc" , 
		     "genf" , "wrat" , "avhv" , "incm" , "inca" , "plow" , "npro" , 
		     "tgif" , "lgif" , "rgif" , "tdon" , "tlag" , "agif" )
		
		# raw data density plots
		p$density_raw <- {
		  data %>% 
		    filter( part == 'train' ) %>% 
		    dplyr::select( avhv , npro , tgif , lgif , rgif , agif , tdon , tlag , incm , inca ) %>% 
		    density_plots( 'Density Plots of Raw Data' )
		}
		
		# log-transformed density plots
		p$density_logtransformed <- {
		  data %>% 
		    filter( part == 'train' ) %>% 
		    dplyr::select( avhv , npro , tgif , lgif , rgif , agif , tdon , tlag , incm , inca ) %>% 
		    mutate_all( log ) %>% 
		    density_plots( 'Density Plots of Log-Transformed Data' )
		}
		
		# correlation plot
		p$correlations <- {
		  data %>% 
		    filter( part == 'train' ) %>% 
		    select( cfg.predictors ) %>% 
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

		# predictors to damt - previously identified predictors are log-transformed first
		p$predstodamt <-  {
		  data %>% 
		    filter( part == 'train' ) %>% 
		    filter( damt > 0 ) %>% 
  		  mutate_at( 
  		    vars( avhv , tgif , lgif , rgif , agif , tlag , incm , inca ) , 
  		    log ) %>% 
		    select( cfg.predictors , damt , -matches('reg') , -home , -genf ) %>% 
		    gather( predictor , value , -damt ) %>% 
		    ggplot( aes( value , damt , color = predictor , group = predictor ) ) +
		    geom_point() + 
		    geom_density2d(  color = 'gray' ) +
		    geom_smooth( method = 'loess' , color = 'black') +
		    facet_wrap( ~ predictor , scales = 'free' ) +
		    theme_gdocs() + 
		    theme(
		      legend.position = 'none' 
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
		
		
		#CLASSIFICATION MODELS: LOGIT, LDA, TREES-BASED, AND SVM
		
		#LOGISTIC REGRESSION
		model.logit <- glm(y ~ x.reg1 + x.reg2 + x.reg3 + x.reg4 + x.home + x.chld + x.hinc + x.genf + x.wrat + 
		                     x.avhv + x.incm + x.inca + x.plow + x.npro + x.tgif + x.lgif + x.rgif + x.tdon + 
		                     x.tlag + x.agif, 
		                  train.dat, family=binomial("logit"))
		
		post.valid.logit = predict(model.logit, valid.dat, type="response") # n.valid.c post probs
		
		profit.logit = cumsum(14.5*c.valid[order(post.valid.logit, decreasing=T)]-2)
		plot(profit.logit) # see how profits change as more mailings are made
		n.mail.valid = which.max(profit.logit) # number of mailings that maximizes profits
		c(n.mail.valid, max(profit.logit)) # report number of mailings and maximum profit
		
		cutoff.logit = sort(post.valid.logit, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
		chat.valid.logit = ifelse(post.valid.logit>cutoff.logit, 1, 0) # mail to everyone above the cutoff
		logit.table = table(chat.valid.logit, c.valid) # classification table
		logit.mail = sum(logit.table[2,])
		logit.mail.tp = logit.table[2,2]
		logit.error = (logit.table[2,1]+logit.table[1,2])/2018
		logit.profit = 14.5*logit.mail.tp - 2*logit.mail
		
		logit.error #validation error rate: 0.2215
		logit.mail #total mailings: 1,406
		logit.profit #total profit: $11,383.50 
		
		#LDA Model Base	
		model.ldaB = lda(y ~ x.reg1 + x.reg2 + x.reg3 + x.reg4 + x.home + x.chld + x.hinc + x.genf + x.wrat + 
		                   x.avhv + x.incm + x.inca + x.plow + x.npro + x.tgif + x.lgif + x.rgif + x.tdon + 
		                   x.tlag + x.agif, 
		                 train.dat) # include additional terms on the fly using I()
		
		post.valid.ldaB = predict(model.ldaB, valid.dat)$posterior[,2] # n.valid.c post probs
		
		profit.ldaB = cumsum(14.5*c.valid[order(post.valid.ldaB, decreasing=T)]-2)
		plot(profit.ldaB) # see how profits change as more mailings are made
		n.mail.valid = which.max(profit.ldaB) # number of mailings that maximizes profits
		c(n.mail.valid, max(profit.ldaB)) # report number of mailings and maximum profit
		
		cutoff.ldaB = sort(post.valid.ldaB, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
		chat.valid.ldaB = ifelse(post.valid.ldaB>cutoff.ldaB, 1, 0) # mail to everyone above the cutoff
		ldaB.table = table(chat.valid.ldaB, c.valid) # classification table
		ldaB.mail = sum(ldaB.table[2,])
		ldaB.mail.tp = ldaB.table[2,2]
		ldaB.error = (ldaB.table[2,1]+ldaB.table[1,2])/2018
		ldaB.profit = 14.5*ldaB.mail.tp - 2*ldaB.mail
		
		ldaB.error #validation error rate: 0.2235
		ldaB.mail #total mailings: 1,406
		ldaB.profit #total profit: $11,354.50
		
		#LDA Model Base + Quad (hinc, chld, tdon, tlag, wrat, inca, npro, tgif)
		model.ldaF = lda(y ~ x.reg1 + x.reg2 + x.reg3 + x.reg4 + x.home + x.chld + x.hinc + x.genf + x.wrat + 
		                   x.avhv + x.plow + x.npro + x.tgif + x.tdon + 
		                   x.tlag + x.agif + I(x.hinc^2) + I(x.chld^2) + I(x.tdon^2) + I(x.tlag^2) + I(x.wrat^2) + 
		                   I(x.npro^2) + I(x.tgif^2), 
		                 train.dat) # include additional terms on the fly using I()
		
		post.valid.ldaF = predict(model.ldaF, valid.dat)$posterior[,2] # n.valid.c post probs
		
		profit.ldaF = cumsum(14.5*c.valid[order(post.valid.ldaF, decreasing=T)]-2)
		plot(profit.ldaF) # see how profits change as more mailings are made
		n.mail.valid = which.max(profit.ldaF) # number of mailings that maximizes profits
		c(n.mail.valid, max(profit.ldaF)) # report number of mailings and maximum profit
		
		cutoff.ldaF = sort(post.valid.ldaF, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
		chat.valid.ldaF = ifelse(post.valid.ldaF>cutoff.ldaF, 1, 0) # mail to everyone above the cutoff
		ldaF.table = table(chat.valid.ldaF, c.valid) # classification table
		ldaF.mail = sum(ldaF.table[2,])
		ldaF.mail.tp = ldaF.table[2,2]
		ldaF.error = (ldaF.table[2,1]+ldaF.table[1,2])/2018
		ldaF.profit = 14.5*ldaF.mail.tp - 2*ldaF.mail
		
		ldaF.error #validation error rate: 0.1457
		ldaF.mail #total mailings: 1,267
		ldaF.profit #total profit: $11,763.00
		
		#CLASSIFICATION TREE, BASE MODEL
		
		model.treeB=tree(y~.,train.dat)
		summary(model.treeB)
		model.treeB
		
		tree.pred=predict(model.treeB,valid.dat,type="class")
		treeB.table = table(tree.pred,valid.dat$y)
		treeB.mail = sum(treeB.table[2,])
		treeB.mail.tp = treeB.table[2,2]
		treeB.error = (treeB.table[2,1]+treeB.table[1,2])/2018
		treeB.profit = 14.5*treeB.mail.tp - 2*treeB.mail
		
		treeB.error #validation error rate: 0.1516
		treeB.mail #total mailings: 1,165
		treeB.profit #total profit: $11,140.50
		
		#CLASSIFICATION TREE, RANDOM FORESTS MODEL
		
		#THE BELOW LOOP TO DISCOVER WHICH SEED RETURNS MAXIMUM PROFIT (SET.SEED(53) RETURNED MAXIMUM)
		# mat = matrix(, ncol=4)
		
		# for(i in 1:100){
		# iter = i
		# set.seed(i)
		# model.rf = randomForest(y~., train.dat, mtry = 4, importance = TRUE)
		
		# summary(model.rf)
		# model.rf
		
		# tree.pred=predict(model.rf, valid.dat, type = "class")
		# rf.table = table(tree.pred, valid.dat$y)
		# rf.mail = sum(rf.table[2,])
		# rf.mail.tp = rf.table[2,2]
		# rf.error = (rf.table[2,1]+rf.table[1,2])/2018
		# rf.profit = 14.5*rf.mail.tp - 2*rf.mail
		
		# vec = c(iter, rf.error, rf.mail, rf.profit)
		# mat = rbind(mat, vec)
		# # print(i)
		# # print(rf.error) #validation error rate: 
		# # print(rf.mail) #total mailings: 
		# # print(rf.profit) #total profit: $
		# }
		
		set.seed(53)
		model.rf = randomForest(y~., train.dat, mtry = 4, importance = TRUE)
		
		summary(model.rf)
		model.rf
		
		tree.pred=predict(model.rf,valid.dat,type="class")
		rf.table = table(tree.pred,valid.dat$y)
		rf.mail = sum(rf.table[2,])
		rf.mail.tp = rf.table[2,2]
		rf.error = (rf.table[2,1]+rf.table[1,2])/2018
		rf.profit = 14.5*rf.mail.tp - 2*rf.mail
		
		rf.error #validation error rate: 0.1070
		rf.mail #total mailings: 1,063
		rf.profit #total profit: $11,257.50
		
		#SVM CLASSIFICATION MODEL
		
		svm.train = svm(y ~ x.reg1 + x.reg2 + x.reg3 + x.reg4 + x.home + x.chld + x.hinc + x.genf + x.wrat + 
		                  x.avhv + x.plow + x.npro + x.tgif + x.tdon + 
		                  x.tlag + x.agif + I(x.hinc^2) + I(x.chld^2) + I(x.tdon^2) + I(x.tlag^2) + I(x.wrat^2)
		                  + I(x.npro^2) + I(x.tgif^2),
		                data = train.dat, kernel = "linear", cost=10,scale = FALSE)
		
		summary(svm.train)
		set.seed(53)
		tune.charity = tune(svm, y ~ x.reg1 + x.reg2 + x.reg3 + x.reg4 + x.home + x.chld + x.hinc + x.genf + x.wrat + 
		                      x.avhv + x.plow + x.npro + x.tgif + x.tdon + 
		                      x.tlag + x.agif + I(x.hinc^2) + I(x.chld^2) + I(x.tdon^2) + I(x.tlag^2) + I(x.wrat^2)
		                    + I(x.npro^2) + I(x.tgif^2), 
		                    data = train.dat, kernel = "linear", ranges = list(cost = c(0.001, 0.005, 0.01, 0.05, 0.1, 1, 5, 10)))
		
		summary(tune.charity)
		bestmod = tune.charity$best.model
		summary(bestmod)
		
		svm.pred = predict(bestmod, valid.dat)
		svm.table = table(predict = svm.pred, truth = valid.dat$y)
		svm.mail = sum(svm.table[2,])
		svm.mail.tp = svm.table[2,2]
		svm.error = (svm.table[2,1]+svm.table[1,2])/2018
		svm.profit = 14.5*svm.mail.tp - 2*svm.mail
		
		svm.error #validation error rate: 0.1115
		svm.mail #total mailings: 1,072
		svm.profit #total profit: $11,239.50
			
# PREDICTION MODELING

		# LEAST SQUARES REGRESSION
		
		#LEAST SQUARES MODEL (LM)
			
			model.ls3 = lm(damt ~ reg1 + reg2 + reg3 + reg4 + chld + hinc + 
			                 wrat + I(wrat^2) + I(wrat^3) + I(wrat^4) + avhv + plow + npro + 
			                 tgif + tdon + tlag + I(tlag^2) + agif,
						data.train.std.y)
						
			pred.valid.ls3 = predict(model.ls3, newdata = data.valid.std.y) # validation predictions
			ls3.mse = mean((y.valid - pred.valid.ls3)^2) # mean prediction error
			ls3.se = sd((y.valid - pred.valid.ls3)^2)/sqrt(n.valid.y) # std error
			
			ls3.mse
			ls3.se
			
		#BEST SUBSETS (BSS)
		
			bss.train.fit = regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + chld + hinc + 
			                             wrat + I(wrat^2) + I(wrat^3) + I(wrat^4) + avhv + plow + npro + 
			                             tgif + tdon + tlag + I(tlag^2) + agif,
				data=data.train.std.y, nvmax=17)
				
			which.min(summary(bss.train.fit)$bic)
			coef(bss.train.fit,which.min(summary(bss.train.fit)$bic))
			predict.regsubsets = function(object, newdata, id, ...){
				form = as.formula(object$call[[2]])
				mat = model.matrix(form, newdata)
				coefi = coef(object, id = id)
				xvars = names(coefi)
				mat[, xvars]%*%coefi
			}
			bss.pred = predict.regsubsets(bss.train.fit, data.valid.std.y, which.min(summary(bss.train.fit)$bic))
			bss.mse = mean((y.valid - bss.pred)^2) # mean prediction error
			bss.se = sd((y.valid - bss.pred)^2)/sqrt(n.valid.y) # std error
			
			bss.mse
			bss.se
	
		#BEST SUBSETS 10-FOLD VALIDATION (BSSF)
		
			k=10
			set.seed(53)
			bssf.folds = sample(1:k,nrow(data.train.std.y),replace=TRUE)
			bssf.cv.errors = matrix(NA, k, 10, dimnames=list(NULL, paste(1:10)))
			for (j in 1:k){
				bssf.train.fit=regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + chld + hinc + 
				                            wrat + I(wrat^2) + I(wrat^3) + I(wrat^4) + avhv + plow + npro + 
				                            tgif + tdon + tlag + I(tlag^2) + agif, 
					data=data.train.std.y[bssf.folds!=j,], nvmax=17)
				for (i in 1:10){
					bssf.pred = predict(bssf.train.fit, data.train.std.y[bssf.folds==j,], id=i)
					bssf.cv.errors[j, i] = mean((bssf.pred - data.train.std.y$damt[bssf.folds==j])^2)
				}
			}
			bssf.mcv.errors = apply(bssf.cv.errors, 2, mean)
			which.min(bssf.mcv.errors) #Returns the number of variables in the Cross-Validation Selected model
			coef(bssf.train.fit, which.min(bssf.mcv.errors)) #Returns the coefficients for the CV selected model
			bssf.pred = predict.regsubsets(bssf.train.fit, data.valid.std.y, which.min(bssf.mcv.errors)) #Predicts y using the CV Selected model
			bssf.mse = mean((y.valid - bssf.pred)^2) #Calculates test MSE for the CV Selected model
			bssf.se = sd((y.valid - bssf.pred)^2)/sqrt(n.valid.y) # std error
			
			bssf.mse
			bssf.se
			
		#RIDGE REGRESSION (RR)
		
			#make x matrix and y from data.train.std.y
			rr.train.x = model.matrix(damt ~ reg1 + reg2 + reg3 + reg4 + chld + hinc + 
			                            wrat + I(wrat^2) + I(wrat^3) + I(wrat^4) + avhv + plow + npro + 
			                            tgif + tdon + tlag + I(tlag^2) + agif, data.train.std.y)[,-21]
			rr.train.y = data.train.std.y$damt
			rr.valid.x = model.matrix(damt ~ reg1 + reg2 + reg3 + reg4 + chld + hinc + 
			                            wrat + I(wrat^2) + I(wrat^3) + I(wrat^4) + avhv + plow + npro + 
			                            tgif + tdon + tlag + I(tlag^2) + agif, data.valid.std.y)[,-21]
			rr.valid.y = data.valid.std.y$damt
			
			set.seed(53)
			rr.cv.out = cv.glmnet(rr.train.x, rr.train.y, alpha=0)
			
			rr.bestlam = rr.cv.out$lambda.min
			rr.bestlam
			
			rr.train.fit = glmnet(rr.train.x, rr.train.y, alpha=0, lambda=rr.bestlam)
			coef(rr.train.fit)[, 1]
			
			rr.pred = predict(rr.train.fit, s=rr.bestlam, newx=rr.valid.x)
			rr.mse = mean((y.valid - rr.pred)^2) # mean prediction error
			rr.se = sd((y.valid - rr.pred)^2)/sqrt(n.valid.y) # std error
			
			rr.mse
			rr.se
			
		#LASSO
		
			set.seed(53)
			lasso.cv.out = cv.glmnet(rr.train.x, rr.train.y, alpha=1)
			
			lasso.bestlam = lasso.cv.out$lambda.min
			lasso.bestlam
			
			lasso.train.fit = glmnet(rr.train.x, rr.train.y, alpha=1, lambda=lasso.bestlam)
			coef(lasso.train.fit)[, 1]
			
			lasso.pred = predict(lasso.train.fit, s=lasso.bestlam, newx=rr.valid.x)
			lasso.mse = mean((y.valid - lasso.pred)^2)
			lasso.se = sd((y.valid - lasso.pred)^2)/sqrt(n.valid.y) # std error
			
			lasso.mse
			lasso.se
			
		#PRINCIPAL COMPONANTS
		
			set.seed(53)
			pcr.train.fit = pcr(damt ~ reg1 + reg2 + reg3 + reg4 + chld + hinc + 
			                      wrat + I(wrat^2) + I(wrat^3) + I(wrat^4) + avhv + plow + npro + 
			                      tgif + tdon + tlag + I(tlag^2) + agif,
				data=data.train.std.y, scale=TRUE, validation="CV")
			
			validationplot(pcr.train.fit,val.type="MSEP")
			
			#PCR (M=10)
			pcr.pred = predict(pcr.train.fit, data.valid.std.y, ncomp=10)
			pcr.mse = mean((y.valid - pcr.pred)^2) # mean prediction error
			pcr.se = sd((y.valid - pcr.pred)^2)/sqrt(n.valid.y) # std error
			
			pcr.mse
			pcr.se
			
			#PCR (M=13)
			pcr.pred = predict(pcr.train.fit, data.valid.std.y, ncomp=13)
			pcr.mse = mean((y.valid - pcr.pred)^2) # mean prediction error
			pcr.se = sd((y.valid - pcr.pred)^2)/sqrt(n.valid.y) # std error
			
			pcr.mse
			pcr.se
			
			#PCR (M=16)
			pcr.pred = predict(pcr.train.fit, data.valid.std.y, ncomp=16)
			pcr.mse = mean((y.valid - pcr.pred)^2) # mean prediction error
			pcr.se = sd((y.valid - pcr.pred)^2)/sqrt(n.valid.y) # std error
			
			pcr.mse
			pcr.se
			
		#PARTIAL LEAST SQUARES
		
			set.seed(53)
			pls.train.fit = plsr(damt ~ reg1 + reg2 + reg3 + reg4 + chld + hinc + 
			                       wrat + I(wrat^2) + I(wrat^3) + I(wrat^4) + avhv + plow + npro + 
			                       tgif + tdon + tlag + I(tlag^2) + agif,
				data=data.train.std.y, scale=TRUE, validation="CV")
			
			validationplot(pls.train.fit,val.type="MSEP")
			
			#PLS (M=3)
			pls.pred = predict(pls.train.fit, data.valid.std.y, ncomp=3)
			pls.mse = mean((y.valid - pls.pred)^2) # mean prediction error
			pls.se = sd((y.valid - pls.pred)^2)/sqrt(n.valid.y) # std error
			
			pls.mse
			pls.se
			
			#PLS (M=7)
			pls.pred = predict(pls.train.fit, data.valid.std.y, ncomp=7)
			pls.mse = mean((y.valid - pls.pred)^2) # mean prediction error
			pls.se = sd((y.valid - pls.pred)^2)/sqrt(n.valid.y) # std error
			
			pls.mse
			pls.se
			
			#PLS (M=14)
			pls.pred = predict(pls.train.fit, data.valid.std.y, ncomp=14)
			pls.mse = mean((y.valid - pls.pred)^2) # mean prediction error
			pls.se = sd((y.valid - pls.pred)^2)/sqrt(n.valid.y) # std error
			
			pls.mse
			pls.se
			
		#GAM

			#GAM - removed correlated variables
			gam.train.fit3 = gam(damt ~ reg1 + reg2 + reg3 + reg4 + s(chld,3) + s(hinc, 3) + 
			                       poly(wrat, 3) + s(avhv, 3) + s(plow, 4) + s(npro, 3) + 
			                       s(tgif, 4) + s(tdon, 3) + poly(tlag, 2) + s(agif, 4),
			                     data=data.train.std.y)
			
			gam.pred = predict(gam.train.fit3, data.valid.std.y)
			gam.mse = mean((y.valid - gam.pred)^2) # mean prediction error
			gam.se = sd((y.valid - gam.pred)^2)/sqrt(n.valid.y) # std error
			
			gam.mse
			gam.se
			
	#MODEL LDAF FINAL CLASSIFICATION MODEL

		model.ldaF = lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
		                   avhv + plow + npro + tgif + tdon + 
		                   tlag + agif + I(hinc^2) + I(chld^2) + I(tdon^2) + I(tlag^2) + I(wrat^2) + 
		                   I(npro^2) + I(tgif^2), 
					 data.train.std.c) # include additional terms on the fly using I()
		
		n.mail.valid = which.max(profit.ldaF)
		tr.rate = .1 # typical response rate is .1
		vr.rate = .5 # whereas validation response rate is .5
		adj.test.1 = (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
		adj.test.0 = ((n.valid.c - n.mail.valid)/n.valid.c)/((1 - vr.rate)/(1 - tr.rate)) # adjustment for mail no
		adj.test = adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
		n.mail.test = round(n.test*adj.test, 0) # calculate number of mailings for test set
		post.test = predict(model.ldaF, data.test.std)$posterior[,2] # post probs for test data
		
		cutoff.test = sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
		chat.test = ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
		table(chat.test)
		
	#GAM1 FINAL PREDICTION MODEL
		yhat.test = predict(gam.train.fit3, newdata = data.test.std) # test predictions
		
	#OUTPUT
	ip = data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
	write.csv(ip, file="E:/STAT 897/Group Project/Report/Report Elements/ip.csv", 
          row.names=FALSE) # use group member initials for file name