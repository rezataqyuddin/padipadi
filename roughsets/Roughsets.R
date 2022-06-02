library(RoughSets)

decision.table <- SF.asDecisionTable(read.csv("dataset/rice_dataset.csv"), decision.attr = 18, indx.nominal = NULL)

conditional.attr <- c(1, 2)
control.ind <- list(type.aggregation = c("t.tnorm", "lukasiewicz"),
                    type.relation = c("tolerance", "eq.1"))
IND.condAttr <- BC.IND.relation.FRST(decision.table, attributes = conditional.attr,
                                     control = control.ind)
decision.attr = c(18)
control.dec <- list(type.aggregation = c("crisp"), type.relation = "crisp")
IND.decAttr <- BC.IND.relation.FRST(decision.table, attributes = decision.attr,
                                    control = control.dec)
control <- list(t.implicator = "lukasiewicz", t.tnorm = "lukasiewicz")
FRST.LU <- BC.LU.approximation.FRST(decision.table, IND.condAttr, IND.decAttr,
                                    type.LU = "implicator.tnorm", control = control)
fuzzy.region <- BC.positive.reg.FRST(decision.table, FRST.LU)

fuzzy.region
