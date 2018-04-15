install.packages("robustbase")
library("robustbase")
fit = lm(cbind(Y1,Y2,Y3,Y4) ~ X1 + X2 + X3 + X4, data=pulpfiber)

# Individual regression models on Y1, Y2, Y3, and Y4
summary(fit)

# Coefficent Matrix for multivariate regression model on all four response variables
fit$coefficients

# Residuals
fit$residuals

# Reordering the pulpfiber matrix
pulpfiber2 = cbind(pulpfiber[,5:8],pulpfiber[,1:4])
pulpfiber2
Y_matrix = pulpfiber2[,1:4]
Y_matrix
Y = as.matrix(Y_matrix)
Y
Z_matrix_no1 = pulpfiber2[,5:8]
ones = matrix(c(1),nrow=62,ncol=1)
Z_matrix = cbind(ones,Z_matrix_no1)
Z = as.matrix(Z_matrix)
Z
Error_matrix = fit$residuals # Residual matrix
Error = Error_matrix
b_hat = solve(t(Z)%*%Z)%*%t(Z)%*%Y
b_hat
Y_hat = Z%*%b_hat

# To run the hypotheses testing, we can refer to Ch. 7.7 -
# - Likelihood Ratio Tests for Regression Parameters
# In the following tests, we assume alpha = 0.05

# Test 1: Responses do not depend on Z1,Z2,Z3, and Z4
# Ho : b1=b2=b3=b4=0
# Ha : b1=/=0
# q = 0
Sigma_hat1 = t(Y-Y_hat)%*%(Y-Y_hat)
Sigma_hat = Sigma_hat1/62
Sigma_hat
b_hat_1 = solve(t(Z[,1])%*%Z[,1])%*%t(Z[,1])%*%Y
b_hat_1
Sigma_hat_1 = (1/62)*t(Y-Z[,1]%*%b_hat_1)%*%(Y-Z[,1]%*%b_hat_1)
Sigma_hat_1
LHS = -(62-4-1-0.5*(4-4+0+1))*log(det(Sigma_hat)/det(Sigma_hat_1))
LHS
df = 62*(4-0)
RHS = qchisq(0.95,df)
RHS
# 170.8647 < 285.7339
# Since LHS < RHS, we do not reject the hypotheses

# Test 2: Responses do not depend on Z2,Z3, and Z4
# Ho : b2=b3=b4=0
# Ha : b2=/=0
Sigma_hat1 = t(Y-Y_hat)%*%(Y-Y_hat)
Sigma_hat = Sigma_hat1/62 # Residual Sum of Squares
Sigma_hat
b_hat_1 = solve(t(Z[,1:2])%*%Z[,1:2])%*%t(Z[,1:2])%*%Y
b_hat_1
Sigma_hat_1 = (1/62)*t(Y-Z[,1:2]%*%b_hat_1)%*%(Y-Z[,1:2]%*%b_hat_1)
Sigma_hat_1
LHS = -(62-4-1-0.5*(4-4+1+1))*log(det(Sigma_hat)/det(Sigma_hat_1))
LHS
df = 62*(4-1)
RHS = qchisq(0.95,df)
RHS
# 109.7515< 218.8205
# Since LHS < RHS, we do not reject the hypotheses

# Test 3: Responses do not depend on Z3, and Z4
# Ho : b3=b4=0
# Ha : b3=/=0
Sigma_hat1 = t(Y-Y_hat)%*%(Y-Y_hat)
Sigma_hat = Sigma_hat1/62 # Residual Sum of Squares
Sigma_hat
b_hat_1 = solve(t(Z[,1:3])%*%Z[,1:3])%*%t(Z[,1:3])%*%Y
b_hat_1
Sigma_hat_1 = (1/62)*t(Y-Z[,1:3]%*%b_hat_1)%*%(Y-Z[,1:3]%*%b_hat_1)
Sigma_hat_1
LHS = -(62-4-1-0.5*(4-4+2+1))*log(det(Sigma_hat)/det(Sigma_hat_1))
LHS
df = 62*(4-2)
RHS = qchisq(0.95,df)
RHS
# 80.32595 < 150.9894
# Since LHS < RHS, we do not reject the hypotheses

# Test 4: Responses do not depend on Z4
# Ho : b4=0
# Ha : b4=/=0
Sigma_hat1 = t(Y-Y_hat)%*%(Y-Y_hat)
Sigma_hat = Sigma_hat1/62 # Residual Sum of Squares
Sigma_hat
b_hat_1 = solve(t(Z[,1:4])%*%Z[,1:4])%*%t(Z[,1:4])%*%Y
b_hat_1
Sigma_hat_1 = (1/62)*t(Y-Z[,1:4]%*%b_hat_1)%*%(Y-Z[,1:4]%*%b_hat_1)
Sigma_hat_1
LHS = -(62-4-1-0.5*(4-4+3+1))*log(det(Sigma_hat)/det(Sigma_hat_1))
LHS
df = 62*(4-3)
RHS = qchisq(0.95,df)
RHS
# 65.35109 < 81.38102
# Since LHS < RHS, we do not reject the hypotheses

