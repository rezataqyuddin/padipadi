library(RoughSets)

## Create a Decision Table from Dataset ##

decision.table <- SF.asDecisionTable(read.csv("dataset/padi_cluster.csv"), 
                                     decision.attr = 14, 
                                     indx.nominal = NULL)

## Dataset conversion to Discretization using unsupervised.quantiles ##

cut.values1 <- D.discretization.RST(decision.table,
                                    type.method = "unsupervised.quantiles",
                                    nOfIntervals = 3)

## Computation of a decision-relative discernibility matrix based on the rough set theory ##
res.2 <- BC.discernibility.mat.RST(decision.table, range.object = NULL)

## Transform Discretized Data to new Decision Table ##
rice.discretized1 <- SF.applyDecTable(decision.table, cut.values1)
dim(rice.discretized1)
lapply(rice.discretized1, unique)

## evaluate single reduct
single.reduct <- FS.quickreduct.RST(rice.discretized1)

## Feature selection using Heuristic Permutation ##
res.1 <- FS.permutation.heuristic.reduct.RST(rice.discretized1,  permutation = NULL)

## Feature selection using Heuristic Permutation ##
res.3 <- FS.DAAR.heuristic.reduct.RST(rice.discretized1)

## Feature selection using Heuristic Permutation ##
res.4 <- FS.greedy.heuristic.reduct.RST(decision.table, qualityF = X.entropy,epsilon = 0.0)

new.decTable <- SF.applyDecTable(rice.discretized1, single.reduct)
new.decTable1 <- SF.applyDecTable(rice.discretized1, res.2)

print(res.1)
print(res.2)
print(new.decTable)

