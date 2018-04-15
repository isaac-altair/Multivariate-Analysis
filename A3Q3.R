q3 = read.table("T9-12.DAT", header=FALSE)
q3
# a.
# Need to standardize the matrix
#
# p3 = scale(q3,center = TRUE,scale = TRUE)
# p3

fac.q3_no_rot = factanal(q3,3,rotation="none")
fac.q3_no_rot
print(loadings(fac.q3_no_rot),cutoff=0,lower = 0.00001)

# b.
fac.q3_rot = factanal(z_q3,3,method="mle",rotation="varimax")
fac.q3_rot
L = print(loadings(fac.q3_rot),cutoff=0,lower = 0.00001)
L

# Interpretation
# To see which factors play greater role, we can use factors with values > 0.5
# Factor 1 describes person's mechanical reasoning and mathematical ability that
# contribute growth of sales, profitability of sales, and new account sales.
# As we can see, Mathematical reasoning plays a greater role in sales performance.
# Factor 2 describes creativity of a person and its contribution to new-account sales.
# Factor 3 describes persons abstract reasoning. Interestingly, it has little
# contribution to contribution in sales.
# It would be a good point to conclude that mechanical reasoning and mathematical ability
# play the greatest role in performance of sales.

# c.
R = cor(p3)
eigenR = eigen(R)
eigenR
#L = cbind(sqrt(eigenR$val[1])*eigenR$vec[,1],sqrt(eigenR$val[2])*eigenR$vec[,2],sqrt(eigenR$val[3])*eigenR$vec[,3])
#L
h = apply(L^2,1,sum)
h
Psi = diag(R)-h
Psi
L%*%t(L)+diag(Psi)

# d.
n = dim(p3)[1]
p = dim(p3)[2]
m = dim(L)[2]
df = p*(p+1)/2-(p*(m+1)-m*(m-1)/2)
df
X2.Barlett = (n-1-(2*p+4*m+5)/6)*log(det(L%*%t(L)+diag(Psi))*(det(R)^(-1)))
X2.Barlett
qchisq(0.95, df)
# Reject the null hypothesis Ho: Sigma = L^2+Psi, u=x, since X2.Barlett > dchisq

# e.
new = matrix(c(110,98,105,15,18,12,35),ncol=7)
new
new_m = new - colMeans(q3)
new_m
new_mv = (solve(cov(q3)))%*%t(new_m)
new_mv
F = t(L)%*%(solve(R))%*%(new_mv-colMeans(p3))
t(F)


