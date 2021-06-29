# Problem n.2
# A jeweler is considering changing diamond supplier in order to purchase diamonds of superior quality. The jeweler
# performed identical and independent measurements on 8 diamonds provided by the current supplier and on 8
# diamonds provided by the new supplier in order to compare the quality of the diamonds. The file purity.txt
# contains the values of 10 purity parameters measured by the jeweler on the 16 diamonds. The higher the purity
# parameter, the higher the quality of the diamond.
# a) For each purity parameter, perform a permutation one-sided test to look for possible statistical superiority of
# the diamonds of the new supplier. In detail, for each purity parameter, use the difference of the sample means as
# test statistic and use 5000 random permutations with random seed equal to 123 to estimate the permutational
# distribution. Report the value of the 10 test statistics and their corresponding p-values.
# b) For which purity parameters the new supplier can be considered superior to the current supplier if the jeweler
# wants to limit the false discovery rate to a maximum value of 10%?
#   c) For which purity parameters the new supplier can be considered superior to the current supplier if the jeweler
# wants to impose a probability at most 1% that at least one of the non-superior purity parameter is judged as
# superior?
#   


data <- read.table('purity.txt', header=TRUE)
head(data)
dim(data)
















