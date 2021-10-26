rm(list=ls())

# Loading our data
ds=read.csv(file.choose())
ds$colonial=as.factor(ds$colonial)

# Now we look at the summary of the dataset

summary(ds)

# plotting the histogram of the varibles price and sqft.

hist(ds$price,col = 'red',main = 'Histogram of Price',xlab='Price')

hist(ds$sqrft,col = 'blue',main = 'Histogram of Sqfit', xlab='square_fit')


# plot relationship between the two price and squarefit

plot(ds$sqrft,ds$price,pch=16, col=c('red','blue'),ylab='Price',xlab='Sqrfit')
abline(lm(ds$price~ds$sqrft))


# Making a simple linear regression model

Model1 <- lm(ds$price~ds$sqrft)
summary(Model1)

## Now calculating residuals 

pred1 <- predict(Model1)
# calculating residuals

res1 <- ds$price-pred1

##d)

plot(ds$id,res1)
plot(ds$sqrft,res1)

##e)

plot(log(ds$lotsize),ds$price)

##f) making new model2

Model2 <- lm(ds$price~ds$sqrft+log(ds$lotsize)+ds$colonial)
summary(Model2)

#g)

# # two sided t.test
#Ho: theta_2=31
#Ha: theta_2 not equal to 31

theta2=31
standarderror=11.25
t_value=theta2/standarderror
t_value
df=nrow(ds)-1
p_value=2*(1-pt(t_value,df))

summari
