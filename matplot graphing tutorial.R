x <- sample(50:100,50, T) # SRSWR
                          # help(sample)
                          # sample(x, size, replace = FALSE, prob = NULL)
x
                          # convert to a 5x10 matrix
A <- matrix(x,5,10)       # by column/column-wise/filling one column first (default)
                          # help(matrix)
                          # matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, 
                          #  dimnames = NULL)
A
                          # name and assignments vectors
names <- c("Sara", "Jill", "Jared", "Kim", "Don")
assignments <- c("Quiz 1", "Quiz 2", "Test 1", "Quiz 3", "Quiz 4", "Midterm", "Quiz 5", 
                 "Quiz 6", "Test 2","Final")  # assign labels to columns and rows
row.names(A) <- names
                          # rownames(A,names) AND row.names(A) give same result.
colnames(A) <- assignments
A
matplot(A)  # (1)
                          # Note our X axis goes to 5
                          # we are graphing students against grades.
matplot(t(A)) # (2)
                          # to graph assignments against grades
matplot(t(A), type="b")
# replace numbers with shapes
matplot(t(A), type="b", pch=15:19) # (3)
matplot(t(A), type="b", pch=15:18, col=c(1:5)) # (4)

#LEGEND ADDING
legend("bottomleft", inset=0.01, legend=names, col=c(1:5),pch=15:19,
       bg= ("white"), horiz=F) # (5)


