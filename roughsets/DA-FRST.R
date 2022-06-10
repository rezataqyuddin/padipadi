#Motif Selection using FSRT
#Referensi : https://cran.r-project.org/web/packages/RoughSets/RoughSets.pdf

library(RoughSets)

dataset = read.csv("dataset/motifs_cluster.csv", sep =";", header = TRUE)
decision.table <- SF.asDecisionTable(dataset,
                                     decision.attr = 1,  indx.nominal = 2)
reduct <- FS.feature.subset.computation(decision.table,method = "quickreduct.frst")

ds.fs <- SF.applyDecTable(decision.table, reduct)

indx <- IS.FRIS.FRST(ds.fs, control = list(threshold.tau = 0.2, alpha = 1))

selected <- SF.applyDecTable(ds.fs, indx)
