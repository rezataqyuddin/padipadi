library(Rcpp)
library(RoughSets)

dc2 <- SF.asDecisionTable(read.csv("dataset/padi_cluster.csv"),decision.attr = 12)
slicedata <- dc2[1:100,]

#Discretize Dataframe
cut.values1 <- D.discretization.RST(slicedata,
                                    type.method = "unsupervised.quantiles",
                                    nOfIntervals = 3)

slicedisc <- SF.applyDecTable(slicedata, cut.values1)
dim(slicedisc)
lapply(slicedisc, unique)

#Select FeatureSet using Reduction Computation
B <- FS.reduct.computation(slicedisc)
IND.B <- BC.IND.relation.RST(slicedisc, feature.set = B)

IND.B

#QuickReduct using FRST and Lower Approximation
control <- list(t.implicator = "lukasiewicz", type.relation = c("tolerance", "eq.1"),
                type.aggregation = c("t.tnorm", "lukasiewicz"))
reduct.1 <- FS.quickreduct.FRST(slicedisc, type.method = "fuzzy.dependency",
                                type.QR = "fuzzy.QR", control = control)
new.decTable <- SF.applyDecTable(slicedisc, reduct.1)

#QuickReduct using RST Single Reduct
res.1 <- FS.quickreduct.RST(slicedisc)
res.1
## generate new decision table according to the reduct
new.decTable2 <- SF.applyDecTable(slicedisc, res.1)
