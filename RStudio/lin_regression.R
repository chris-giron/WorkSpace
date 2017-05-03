library(ggplot2)
data(diamonds)

# Sample of Data Set
diamSmall = diamonds[sample(1:dim(diamonds)[1],500),]

# Build Model
M1 = lm(price~carat,diamSmall)
theta = coef(M1)
theta
ggplot(diamSmall, aes(carat,price)) + geom_point() + 
  geom_abline(intercept = theta[1],slope=theta[2], size=2, color=I("red"))

# Predict Model
predict(M1,data.frame(carat=c(3,4,5)))

# Summary of Model
summary(M1)
summary(M1)$r.squared
residuals(M1)
mean(residuals(M1)^2)

# Non-linear Model
M2=lm(price~carat+I(carat^2)+I(carat^3),diamSmall)
theta=coef(M2)
theta
# Create Domain
X=seq(0,3,length = 100)
# Calculate Y Values
Y=theta[1]+theta[2]*X+theta[3]*X^2+theta[4]*X^3
D=data.frame(x=X,y=Y)
ggplot(D,aes(X,Y)) + 
  geom_line(size=2,color='red') + 
  geom_point(data = diamSmall, aes(carat,price))

# Summary of New Model
summary(M2)$r.squared
mean(residuals(M2)^2)


# Adding Explanatory Variable
M3=lm(price~carat+color,diamSmall)
summary(M3)$r.squared
mean(residuals(M3)^2)

# Log Model
M4=lm(log(price)~log(carat),diamSmall)

