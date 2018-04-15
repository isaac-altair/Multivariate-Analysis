turtle<-read.table("Turtle.txt", header = TRUE)
turtle
#names(turtle)=scan(what="")

#sex Length Width Height

pairs(turtle[,2:4])

turtle.f=turtle[turtle[ , 1]=="1", 2:4]
turtle.f=turtle[turtle[ , 1]=="2", 2:4]
turtle.f = log(turtle.f)
turtlef.pc<-prcomp(turtle.f)
turtlef.pc

summary(turtlef.pc)

turtlef.pc$rotation

cor(turtle.f, turtlef.pc$x[ ,1:2])
screeplot(turtlef.pc,type="lines")
plot(turtlef.pc$x[,1],turtlef.pc$x[,2],xlab="PC1",ylab="PC2",pch=20)

turtlefs.pc<-prcomp(turtle.f, scale=TRUE)
turtlefs.pc

type = c(rep("F",24),rep("M",24))

turtle.a <- log(turtle[])

