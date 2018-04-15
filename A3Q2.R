q2 = read.table("spotowls.txt", header=FALSE)
q2
q2 = q2[, 2:8]
q2
names(q2)<-scan(what='')
'0.91' '1.18' '1.40' '1.60' '1.77' '2.41' '3.38'

R = cor(q2)
R
eigenR=eigen(R)
eigenR
evalueR = (eigenR$values)
evalueR
eigen_sum = sum(evalueR)
evectorR = eigenR$vectors
evectorR
PCmatrix = prcomp(q2, scale = TRUE)
PCmatrix
PCs = PCmatrix$rotation
PCs
PC1 = PCs[,1] # Column
PC1
PC2 = PCs[,2] # Column
PC2

# PC1 explanation:
# PC1 describes the percentage of mature forests withing given 7 diameters.
# PC2 explnation:
# PC2 describes the change in maturity of trees. As we go away from the centre,
# the amount of mature trees decreases.

# 2b. Relationship between PC1 & PC2 and R
# - PC1 and R:
# The entries in R are all positive, just as in PC1. There is an inverse correlation 
# relationship between outer and inner rings. 
# - PC2 and R:
# In PC2, we see that values are increasing for young trees. This argument is supported 
# by the decreasing variance between inner and outer rings which suggests that there is a greater
# amout of older trees in the centre as opposed to outer rings.

# 2c.
# Proportion = (lambda1+lambda2)/(sum of all eigenvalues)*100%
Proportion = ((evalueR[1]+evalueR[2])/sum(evalueR))
Proportion
# 81.01%

# 2d.Combining Kaiser's Scree Rule and Cattell's Scree test, we can see that it is sufficient for us to
# use first 2 PCs since they describe ~80% of the data. Note, for Kaiser's test to work, all of the eigenvalues
# must be greater than one. But, to make sure that >= 80% of the data is explained, we need to round to second
# eigenvalue 0.9125307 to 1.

fit <- princomp(R, cor=TRUE)
plot(fit,type="lines") # scree plot 

# 2e.

pc.q2 = princomp(q2, cor=TRUE)
plot(pc.q2$score[1:30,1],pc.q2$score[1:30,2], xlab="PC1",ylab="PC2",pch=20)
points(pc.q2$score[31:60,1],pc.q2$score[31:60,2], xlab="PC1",ylab="PC2",pch=2)
legend(4,2,c("Dot - R","Triangle - N"))

# Explanation
# As we can see, the values for the principal components in random sites seem
# to be more scattered whereas the values for the principal components in nesting
# sites seem be clustered together indicating that the percentage of mature
# trees is higher in the centres of nesting sites.

# 2f
Rmean = colMeans(Rmatrix, na.rm = FALSE, dims = 1)
Rmean
Nmean = colMeans(Nmatrix, na.rm = FALSE, dims = 1)
Nmean
Sp = ((30-1)*var(Rmatrix)+(30-1)*var(Nmatrix))/(60-2)
T2 = t(Rmean-Nmean)%*%solve((2/30)*Sp)%*%(Rmean-Nmean)
LFS = T2
LFS
RHS = (60-2)*7/(60-7-1)*qf(0.95,7,60-7-1)
RHS
# LHS > RHS
# Reject Ho under Ho: u1=u2
# Therefore, Nesting Sites and Random sites have a different mean suggesting 
# a different distribution of mature trees.

df1 = 7
df2 = 52
p_value = 1 - pf(LFS*(((60-2)*7)/(60-7-1))^(-1),7,52)
p_value
