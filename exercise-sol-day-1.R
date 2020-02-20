# R dataframe warpbreaks
data("warpbreaks")
head(warpbreaks)
warpbreaks$wool
warpbreaks$tension

# Using a linear model, 
# establish whether there is evidence that the effect of tension on break rate 
# is dependent on the type of wool. If there is, 
# use interaction.plot() function to examine the nature of the dependence

# cat to numeric
M <- warpbreaks
must_convert<-sapply(M,is.factor)
must_convert

# logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(M[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
M2

out<-cbind(M[,!must_convert],M2)  
out

lm.mod <- lm( tension ~ V1+wool, data=data.frame(out))
summary(lm.mod)

lm.mod1 <- lm( tension ~ wool, data=data.frame(out))
summary(lm.mod1)
# Generate the model diagnostic plots
par(mfrow=c(2,2))
plot(lm.mod1)

out <- data.frame(out)
# Plot the predicted heart attack levels (proportions)
plot(out$wool,out$tension,xlab="wool T", ylab="tension T")
lines(out$wool,fitted(lm.mod))

#------------------------------
#2
#----------------------------
data(cars)
cars

lm.modq2 <-lm(dist ~ speed+0,data=cars)
summary(lm.modq2)

plot(fitted(lm.modq2),residuals(lm.modq2),xlab="fitted values",ylab="residuals")

res<-residuals(lm.modq2)
del1<-which.max(res)
del2<-which.min(res)

newdata<-cars[-c(del1, del2),]
lm.modq22 <-lm(dist ~ speed+0,data=newdata,)
summary(lm.modq22)

plot(fitted(lm.modq22),residuals(lm.modq22),xlab="fitted values",ylab="residuals")

lm.modq222 <-lm(dist ~ speed+I(speed^2)+0,data=newdata)
summary(lm.modq222)

lm.modq223 <-lm(dist ~ speed+I(speed^2),data=newdata)
summary(lm.modq223)


#3
conc <- c (0.1, 0.5, 1, 10, 20, 30, 50, 70, 80, 100, 150)
no <- c(7, 1, 10, 9, 2, 9, 13, 1, 1, 4, 3) 
yes <- c(0, 0, 3, 4, 0, 6, 7, 0, 0, 1, 7)

data_cons<-data.frame(cbind(conc, no, yes)) 

# GLM call using cbind() to fit the heart attack model
mod.0 <- glm(cbind(yes,no)~conc, family=binomial(link=logit), data=data_cons)
mod.0

# Generate the model diagnostic plots
par(mfrow=c(2,2))
plot(mod.0)


# Fit a second model with a cubic linear predictor
mod.2 <- glm(cbind(yes,no)~conc+I(conc^2)+I(conc^3), family=binomial, data=data_cons)
mod.2

# Plot predicted heart attack levels (proportions) with the cubic model
par(mfrow=c(1,1))
plot(heart$ck,p,xlab="Creatinine kinase level", ylab="Proportion Heart Attack")
lines(heart$ck,fitted(mod.2))

par(mfrow=c(2,2))
plot(mod.2)
# Calculate an 'analysis of deviance' table
anova(mod.0,mod.2,test="Chisq")


