x<- read.csv(file.choose(),header = T)
head(x)
summary(x)
names(x)
par(mfrow=c(3,3))
boxplot(x$carat,
        main = "carat",
        ylab = "Carat (ct)")

boxplot(x$depth_percent,
        main = "depth_percent",
        ylab = "(%)")



boxplot(x$table_percent,
        main = "table_percent",
        ylab = "(%)")


boxplot(x$price,
        main = "price",
        ylab = "(Rs.)")

boxplot(x$length_mm,
        main = "length_mm",
        ylab = "(mm)")



boxplot(x$width_mm,
        main = "width_mm",
        ylab = "(mm)")

boxplot(x$depth_mm,
        main = "depth_mm",
        ylab = "(mm)")

table(x$cut)
table(x$color)
table(x$clarity)

par(mfrow=c(1,3))
barplot(table(x$cut),
        main="cut",
        ylab= "condition")
barplot(table(x$color),
        main="color",
        ylab="grade")
barplot(table(x$clarity),
        main="clarity",
        ylab="Condition")

par(mfrow=c(3,3))
hist(x$"carat", freq=FALSE)
lines(density(x$"carat"),col="red")

hist(x$"depth_percent", freq=FALSE)
lines(density(x$"depth_percent"),col="red")



hist(x$"price", freq=FALSE)
lines(density(x$"price"),col="red")

hist(x$"table_percent", freq=FALSE)
lines(density(x$"table_percent"),col="red")

hist(x$"length_mm", freq=FALSE)
lines(density(x$"length_mm"),col="red")



hist(x$"width_mm", freq=FALSE)
lines(density(x$"width_mm"),col="red")

hist(x$"depth_mm", freq=FALSE)
lines(density(x$"depth_mm"),col="red")

u<- subset(x,select = c(depth_mm,width_mm,length_mm,table_percent,price,depth_percent,carat))
plot(u,
     pch=16,
     cex=0.3)
par(mfrow=c(2,3))

plot(x$price, x$carat,
     main="Price vs Carat",
     xlab="Price(Rs)",
     ylab="Carat")

plot(x$price, x$width_mm,
     main="Price vs width_mm",
     xlab="Price(Rs)",
     ylab="width_mm")
plot(x$price, x$depth_mm,
     main="Price vs depth_mm",
     xlab="Price(Rs)",
     ylab="depth_mm")


plot(x$price, x$length_mm,
     main="Price vs length_mm",
     xlab="Price(Rs)",
     ylab="length_mm")



model= lm(price~., data=x)


numeric_data <- x[, c("price",
                      "carat",
                      "depth_percent",
                      "table_percent",
                      "length_mm",
                      "width_mm",
                      "depth_mm")]

cor(numeric_data)

round(cor(numeric_data),2)

install.packages("corrplot")   # First time only

library(corrplot)

corrplot(cor(numeric_data),
         method="color",
         addCoef.col="black")


plot(model)
summary(model)

coef(model)
resid(model)
predict(model)
confint(model, level = 0.90)
vcov(model)
df.residual(model)
summary(model)$r.squared
summary(model)$adj.r.squared
anova(model)
par(mfrow=c(2,2))
plot(model)


