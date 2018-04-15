### q4

q4 = read.table("PsyTests.txt", header = T)

### a
Male_matrix=q4[q4$G==1,]
Female_matrix=q4[q4$G==2,]
Male_matrix = Male_matrix[, 2:5]
Female_matrix = Female_matrix[, 2:5]
Male_var = var(Male_matrix)
Female_var = var(Female_matrix)
Master_var = (31*Male_var+31*Female_var)/62
Pooled = Master_var
Pooled

### b
# Ho: Sigma1=Sigma2=...=Sigmak
# Ha: Sigma1=/=Sigma2=/=...=/=Sigmak
Pooled = Master_var
M = 62*log(det(Pooled))-31*log(det(Male_var))-31*log(det(Female_var))
C = 1 - 43/30*(2/32-1/62)
Chi_squared = qchisq(0.95, 6)
RHS = M * C^(-1)
RHS > Chi_squared
# If true, reject the hypothesis that Ho: Sigma1=Sigma2=...=Sigmak

### c
# Ho: mu_men=mu_women
# Ha: mu_men=/=mu_women
Male_matrix=q4[q4$G==1,]
Female_matrix=q4[q4$G==2,]
Male_matrix = Male_matrix[, 2:5]
Female_matrix = Female_matrix[, 2:5]
Male_var = var(Male_matrix)
Female_var = var(Female_matrix)
Master_var = (31*Male_var+31*Female_var)/62
Male_mean = colMeans(Male_matrix)
Female_mean = colMeans(Female_matrix)
Mean_dif=t(Male_mean-Female_mean)
T_squared = 64*(Male_mean-Female_mean)%*%(Master_var)^(-1)%*%cbind(Male_mean-Female_mean)
LHS = 60/(4*63)*T_squared
RHS = df(4,60,0.05)
LHS > RHS
# Do not reject Ho since True

### d
plot(c(0:16), c(12:28),type="n",xlab = "Order of inputs", ylab="Gender Means", main="Profile plot of two mean vectors")
lines(Male_mean,lty = 1)
lines(Female_mean,lty = 2)
legend(7.5,25,c("Male mean","Female mean"),lty=c(1,2))

### e
Observation
## Parallelism:
# Graph is not parallel at any point as can be seen on the graph. 
# Thus, we can't test Coincidental Profile. It's been mentioned in lecture that we can
# just write our observation regarding the graph and make necessary conclusions.
# Thus, no code is necessary for profile analysis, graph speaks for itself.