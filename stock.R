T = read.csv("YahooStock.csv", header=TRUE)
names(T)
head(T)
P = T[ , 2:11]
names(P)
plot(P[,1],type='l',ylim=c(0,100)) 
# plot several time series
points(P[,3],type='l',col=3)
points(P[,4],type='l',col=4)
points(P[,5],type='l',col=5)



p= prcomp(P,scale=TRUE)
summary(p)
p$rotation

plot(p, type="l")

cor(P, p$x[,1:2])
p1 = p$x[ ,1]
p2 = p$x[ ,2]
hist(p1)
hist(p2)
qqnorm(p1)
qqnorm(p2)


plot(1:5000, p1)
points(1:5000, p2, col="blue")

plot(p1, p2)

