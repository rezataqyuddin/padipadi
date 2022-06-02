library(RoughSets)

###### Feature Selection Using RST ######
decision.table <- SF.asDecisionTable(read.csv("dataset/rice_dataset.csv"), decision.attr = 18, indx.nominal = NULL)
cut.values1 <- D.discretization.RST(decision.table,type.method = "unsupervised.quantiles",nOfIntervals = 3)
rice.discretized1 <- SF.applyDecTable(decision.table, cut.values1)

splitdata <- floor(0.25 * nrow(rice.discretized1))

## set the seed to make your partition reproducible
set.seed(123)
new.decision.table <- sample(seq_len(nrow(rice.discretized1)), size = splitdata)

control <- list(type.aggregation = c("t.tnorm", "lukasiewicz"),
                t.implicator = "lukasiewicz", type.relation = c("tolerance", "eq.1"))

## Quick Reduct ##
reduct.2 <- FS.quickreduct.FRST(rice.discretized1, type.method = "fuzzy.dependency",
                                type.QR = "fuzzy.QR", control = control)
reduct.3 <- FS.quickreduct.FRST(rice.discretized1, type.method = "min.positive.reg",
                                type.QR = "fuzzy.QR", control = control)
reduct.4 <- FS.quickreduct.FRST(rice.discretized1, type.method = "fuzzy.discernibility",
                                type.QR = "fuzzy.QR", control = control)
reduct.5 <- FS.quickreduct.FRST(rice.discretized1, type.method = "owa",
                                type.QR = "fuzzy.QR", control = control)


## generate a single reduct using FRST
reduct.2 <- FS.reduct.computation(rice.discretized1, method = "nearOpt.fvprs")

## generate a single reduct using RST
reduct.1 <- FS.reduct.computation(rice.discretized1, method = "greedy.heuristic")


## generate a new decision table using reduct.1
new.decTable.1 <- SF.applyDecTable(rice.discretized1, reduct.1)

## generate new decision table using reduct.2
new.decTable.2 <- SF.applyDecTable(decision.table, reduct.2)