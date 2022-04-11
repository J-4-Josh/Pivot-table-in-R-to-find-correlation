## Enter number of elements in variable (all variables must be same length)
var_len <- 4
## Enter row names
Enter_rwn <- c("18-24", "25-34", "35-54", "55+")
## Creating variables: Enter values of your variables 
## Change variable names (if needed)
Apple <- c(991, 807, 593, 437)
Android <- c(883, 754, 618, 336)
Blackberry <- c(307, 324, 307, 200)
Windows <- c(102, 159, 151, 171)
Other <- c(146, 119, 74, 50)
Unsure <- c(337, 367, 512, 460)

Apple[5] <- c(sum(Apple[1:var_len]))
Android[5] <- c(sum(Android[1:var_len]))
Blackberry[5] <- c(sum(Blackberry[1:var_len]))
Windows[5] <- c(sum(Windows[1:var_len]))
Other[5] <- c(sum(Other[1:var_len]))
Unsure[5] <- c(sum(Unsure[1:var_len]))



tbl <- data.frame(Apple, Android, Blackberry, Windows, Other, Unsure)
rown <- c(Enter_rwn,  "Total")
?row.names
row.names(tbl) <- rown[1:length(rown)]
Total <- c()
n <- c()
for (i in 1:(var_len+1)) {
  tbl$Total[i] <- c(Apple[i] + Android[i] + Blackberry[i] + Windows[i] + Other[i] + Unsure[i])
  }



## Creating Expected table from original
tbl_e <- data.frame()
lrow <-  nrow(tbl)
lcol <- ncol(tbl)
n <- tbl[lrow, lcol]
n
drl <- lrow - 1
dcl <- lcol - 1

for (i in 1:drl) {
  for (j in 1:dcl) {
   tbl_e[i, j] <- (tbl[i, lcol]*tbl[lrow, j])/n
  }
}

pre_sum_oe <- data.frame()
for (i in 1:drl) {
  for (j in 1:dcl) {
    pre_sum_oe[i, j] <- (tbl[i, j]-tbl_e[i, j])/tbl_e[i, j]
  }
}

sum(pre_sum_oe)


