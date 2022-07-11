d<-read.csv("C:/Users/Sravanthi/Desktop/murder.csv")
d

x<-c(d[,2])
y<-c(d[,4])

# Apply the lm() function.
relation <- lm(x~y)

print(relation)
summary(relation)

a <- data.frame(y = 30)
result <-  predict(relation,a)
print(result)

plot(y,x,col = "blue",main = "murder & literacy Regression",
     abline(lm(y~x)),cex = 1.3,pch = 16,xlab = "POVERTY",ylab = "murders")


