###Question 2

q1 = matrix(c(6, 10, 8, 9, 6, 3), nrow=3, ncol=2)
Original_matrix = q1
Original_matrix
s = var(Original_matrix)
s
mean_vectors = colMeans(Original_matrix)
mean_vectors
C = matrix(c(1, 1, -1, 1), nrow=2, ncol=2)
install.packages("ICSNP")
library(ICSNP)
mu_1 = matrix(c(9, 5), nrow = 1, ncol = 2)
mu_1
q1
HotellingsT2(q1, mu_1) 			# Run T2 on original matrix
new_row_1 = C%*%Original_matrix[1,] # Substitute the first of original by
new_row_1
q1[1, ] = new_row_1			# the product of C by row 1
q1[1, ]
new_mu_1 = C%*%t(mu_1)			# Update mu
new_mu_1
HotellingsT2(q1, t(new_mu_1))		# Run T2 on new matrix 1
q1[1,]=Original_matrix[1,]          # Bring q1 to its original form
new_row_2 = C%*%Original_matrix[2,] # Substitute the second row of q1 with
q1[2,]=new_row_2				# C by row 2
HotellingsT2(q1, t(new_mu_1))			# Run T2 on matrix 2
q1[2,]=Original_matrix[2,]		# Bring q1 to its original form
new_row_3 = C%*%Original_matrix[3,] # Substitute the second row of q1 with
q1[3,]=new_row_3				# C by row 3
q1[3,]
q1_new = rbind(t(new_row_1),t(new_row_2), t(new_row_3))
q1_new
s_new = var(q1_new)
s_new
q1_new

t(new_mu_1)
HotellingsT2(q1_new, t(new_mu_1))			# Run T2 on matrix 3

