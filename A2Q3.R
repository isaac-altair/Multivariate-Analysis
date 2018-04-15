### Question 3

### a
#Ho: mu*d = 0
#Ha: mu*d =/= 0
q3 = read.table("BronchusCancer.txt", header = T)
alpha = 0.05
d1 = q3[,1]-q3[,3]
d2 = q3[,2]-q3[,4]
d_master = matrix(c(d1,d2),nrow=16,ncol=2)
mean_1 = sum(d_master[,1])/16
mean_2 = sum(d_master[,2])/16
mean = matrix(c(mean_1,mean_2),nrow=1,ncol=2)
variance = var(d_master)
T2=16*mean%*%variance^(-1)%*%t(mean)
Critical_value = 2*15/14*qf(0.95, df1=2, df2=14)
T2 < Critical_value
# Reject. T2 < Critical_value


### b
### i
#install.packages("ellipse")
# library(ellipse)
# library(car)
source("Ellipse.R")
c = 2.4478
column_mean = c(mean_1,mean_2)
variance = var(d_master)
draw.ellipse(column_mean,variance,c)

### ii
# Ho: mu*d = 0
# Ha: mu*d =/= 0

d1_bar = mean(d1)
d1_var = var(d1)^0.5
t_test_d1 = d1_bar/(d1_var/4)
t_test = dt(c(0.025,0.975), df=15)
t_test_d1 > t_test
# Reject. t_test_d1 > t_test


d2_bar = mean(d2)
d2_var = var(d2)^0.5
t_test_d1 = d2_bar/(d2_var/4)
t_test = dt(c(0.025,0.975), df=15)
t_test_d1 > t_test
# Reject. t_test_d1 > t_test


### iii

vector_1 = matrix(c(1,0),1,2)
vector_2 = matrix(c(0,1),1,2)
mean_sim = matrix(c(mean_1,mean_2),nrow=1,ncol=2)
variance_sim = var(d_master)
crit_value = (2*15/14*df(0.05,2,14))^(0.5)
vector_1_plus = vector_1%*%t(mean_sim)+crit_value*(vector_1%*%variance_sim%*%t(vector_1))^(0.5)
vector_1_minus = vector_1%*%t(mean_sim)-crit_value*(vector_1%*%variance_sim%*%t(vector_1))^(0.5)
component_vector_1_interval = matrix(c(vector_1_minus,vector_1_plus),1,2)
component_vector_1_interval
vector_2_plus = vector_2%*%t(mean_sim)+crit_value*(vector_2%*%variance_sim%*%t(vector_2))^(0.5)
vector_2_minus = vector_2%*%t(mean_sim)-crit_value*(vector_2%*%variance_sim%*%t(vector_2))^(0.5)
component_vector_2_interval = matrix(c(vector_2_minus,vector_2_plus),1,2)
component_vector_2_interval

### iv

mean_bon = matrix(c(mean_1,mean_2),nrow=1,ncol=2)
variance_bon = var(d_master)
xbar_1_plus = mean_bon[,1]+dt(0.05/4,15)*(variance_bon[1,1]/16)^0.5
xbar_1_minus = mean_bon[,1]-dt(0.05/4,15)*(variance_bon[1,1]/16)^0.5
component_1_interval = matrix(c(xbar_1_minus,xbar_1_plus),1,2)
component_1_interval
xbar_2_plus = mean_bon[,2]+dt(0.05/4,15)*(variance_bon[2,2]/16)^0.5
xbar_2_minus = mean_bon[,2]-dt(0.05/4,15)*(variance_bon[2,2]/16)^0.5
component_2_interval = matrix(c(xbar_2_minus,xbar_2_plus),1,2)
component_2_interval

### v
inv_var = (variance)^(-1)
Max_T2 = inv_var%*%t(mean)
Max_T2
# Conclusion:
# By looking at 95% confidence region, we see that it contains 0 mean which suggests
# that our treatment is ineffective. Therefore, I don't want to receive the treatment.

