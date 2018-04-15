### Question 1

q2 = read.table("A2Q1.txt", header = TRUE)
q2 = q2[, 2:4]
q2
xbar_MLE=colMeans(q2)
xbar_MLE
S_MLE = 0.9*var(q2)
S_MLE
Unbiased_S = var(q2)
det(Unbiased_S)
sum(diag(Unbiased_S))
