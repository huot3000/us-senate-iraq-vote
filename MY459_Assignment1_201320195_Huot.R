
#######################
#
# MY459
# Problem Set 1 
#
# Philippe Huot - 201320195
#
#######################

## Cleaning

	#Clears environment
	rm(list=ls())
      
	#Clears plots
	dev.off()
      
	#Clears console
	cat("\014")

## Set-up 

	#Loading dataset for problem set
	install.packages("pscl",dependencies=TRUE) 

	#Loading needed package
	library(pscl) 

	#Access relevant variables in dataset
	attach(iraqVote)

	#Having a look at variables in dataset
	help(iraqVote)

#############
## Question 1
#############

	#Fitting a linear reg model for Senator's vote as a function of whether the 
	#senator was a Rep and the vote in that Senator's state in the previous 
	#presidential elections

	lm(y~rep + gorevote)

	#Saving this linear regression model
	linreg <- lm(y ~ rep + gorevote) 

	#Saving my coefs
	coeflinreg <- coef(linreg) 

##a)

	#Two ways to get my coefficient values
	summary(linreg) 
	print(coeflinreg)

	# y = 1.74458 + 0.316933repTRUE - 0.012376gorevote

##b)

	#Interpreting the intercept

	#The intercept is the probability that one Senator votes "Yay" when he is 
	# not Republican and when the vote share for Gore in the previous elections
	# is 0. At 1.17446 (over 100%) this interpretation doesn't make much sense.
	# Another model should perhaps be used (probit).

##c)

	#Calculating the RMSE:

	#Generating fitted values
	linearfittedvalues <- predict(linreg,type="response")

	#Computing the RMSE
	sqrt(mean((y - linearfittedvalues)^2))

	#Defining a RMSE function
	rmse <- function(fitted, observed) sqrt(mean((observed - fitted)^2))
	rmse(linearfittedvalues, y)

##d) 

	#Over what range of the explanatory variables does the linear model give a 
	#prediction greater than or less than zero?

	# I am struggling with this one.

#############
## Question 2
#############

	#Fit logistic regression of Iraq vote as a function of party and vote:
	glm(y ~ rep + gorevote, data=iraqVote, family=binomial)

	#Saving this logistic regression model
	logreg <- glm(y ~ rep + gorevote, data=iraqVote, family=binomial)

	#Saving my coefs
	coeflogreg <- coef(logreg) 

##a)

	#Printing my coefficient values
	print(coeflogreg)	
	summary(logreg)
	
	# prob(y=1) =  5.87859 + 3.01881repTRUE - 0.11322gorevote

##b)

	#Calculating the RMSE:

	#Generating fitted values
	logfittedvalues <- predict(logreg,type="response")

	#Computing the RMSE
	sqrt(mean((y - logfittedvalues)^2))

	#Defining a RMSE function
	rmselog <- function(fitted, observed) sqrt(mean((observed - fitted)^2))
	rmselog(logfittedvalues, y)

	#Let's compare the two RMSEs:
	#Linear model RMSE: 
	rmse(linearfittedvalues, y)
	#Log model RMSE:
	rmselog(logfittedvalues, y)

	#They are almost identical!

#############
## Question 3
#############

	##Plotting the predicted probabilities/fitted values for the log regression

	#I am interested in creating two plots: one for Republicans (REP) and one for Democrats (DEM).
	#I will hence create one fake dataset where rep=TRUE and rep=FALSE

	fakedataREP <- with(iraqVote, data.frame(gorevote = seq(0,100,1), rep=TRUE))
	fakedataDEM <- with(iraqVote, data.frame(gorevote = seq(0,100,1), rep=FALSE))

	#Generating predicted probabilities/fitted values for both fake datasets,
	#using the logreg model saved above.

	logfittedREP <- predict(logreg, newdata = fakedataREP, type = "response")
	logfittedDEM <- predict(logreg, newdata = fakedataDEM, type = "response")

	#Plotting the data and inserting the line for both Republican (red) and 
	#Democrat fitted values (blue)

	plot( xlim=c(0,100), ylim=c(0,1), jitter(gorevote, amount=10), y , main="Probability of Senator Voting YES, by Party", xlab="State's Share of Democratic Vote in Previous Elections", ylab="Probability of YES Vote on Iraq", pch="|", cex=0.75, col=rgb(0,0,0,0.5))
	lines( fakedataREP$gorevote, logfittedREP, lwd=2, col ="#E91D0E")
	lines( fakedataDEM$gorevote, logfittedDEM, lwd=2, col ="#232066")
	legend("bottomleft", legend=c("if Republican", "if Democrat"), col=c("#E91D0E", "#232066"), lty=c(1,1), lwd=c(2,2))

##a)

	#The coefficient on the "gorevote" explanatory variable would need to be bigger 
	#for the Democrat senators. In other words, for the two lines to be the same,
	#the coefficient estimates for the democrats would need to be higher - senators in states with 
	#a high proportion of democratic vote share in the previous elections should be more prone
	#to vote yes than it is the case.

##b)

	#The senators who voted to authorize the Iraq War had constituency who historically
	#didn't vote for Democrates (at least based on previous election results).
	#Without surprise, Republican senators had a higher probability of voting for the Iraq War.

#############
## Question 4
#############

	#Manually calculting the fitted values

	#Getting the X values for each observation, and assigning them to a matrix

	X <- model.matrix(logreg)
	print(head(X))

	#Getting the coefficient values (beta) for each observation, assign them to a vector
	beta <-coef(logreg)
	print(beta)

	#Get the fitted values through matrix multiplication
	manualfitted <- exp(X %*% beta)/(1+exp(X %*% beta))
	head(manualfitted)

	#Comparing the obtained fitted values
	tail(cbind(logfittedvalues, manualfitted))

	#They are the same. 

##############################
# End of Problem Set
##############################

