###################################################
library(RoughSets)
## Read Dataset
dataset = read.csv("dataset/motifs_cluster.csv")


## Data Prepocessing 
## 1. Check and Transform Missing Value
## 2. Encoding Categorical 

dataset$npb = factor(dataset$npb,
                         levels=c('Present','Absent'),
                         labels=c('1','0'))
dataset$cult_93_11 = factor(dataset$cult_93_11,
                       levels=c('Present','Absent'),
                       labels=c('1','0'))

dataset$tadukan = factor(dataset$tadukan,
                         levels=c('Present','Absent'),
                         labels=c('1','0'))
dataset$tetep = factor(dataset$tetep,
                         levels=c('Present','Absent'),
                         labels=c('1','0'))

## 3. Normalize Start and End, hr12_pro, and ref_prod using Z-Score
dataset$start <- (dataset$start - mean(dataset$start)) / sd(dataset$start)
dataset$end <- (dataset$end - mean(dataset$end)) / sd(dataset$end)
dataset$hr12_prod_size <- (dataset$hr12_prod_size - mean(dataset$hr12_prod_size)) / sd(dataset$hr12_prod_size)
dataset$ref_prod_size <- (dataset$ref_prod_size - mean(dataset$ref_prod_size)) / sd(dataset$ref_prod_size)

## 3. Convert dataset into Decision Table

decision.table <- SF.asDecisionTable(dataset,
                                     decision.attr = 13, 
                                     indx.nominal = NULL)

cut.values1 <- D.discretization.RST(decision.table,
                                    type.method = "unsupervised.quantiles",
                                    nOfIntervals = 3)
rice.discretized1 <- SF.applyDecTable(decision.table, cut.values1)
dim(rice.discretized1)
lapply(rice.discretized1, unique)

control <- list(t.implicator = "lukasiewicz", type.relation = c("tolerance", "eq.1"),
                m.owa = 3, type.aggregation = c("t.tnorm","lukasiewicz"))

summary(decision.table)
conditional.attr <- c(4,5,6,7,8,9);

IND.A <- BC.IND.relation.RST(rice.discretized1, feature.set = conditional.attr)

reduct <- FS.quickreduct.RST(rice.discretized1, control = control)



## build the decision-relation discernibility matrix
res.2 <- BC.discernibility.mat.RST(rice.discretized1, range.object = NULL)

## generate all reducts
reduct2 <- FS.all.reducts.computation(res.2)

## generate new decision table
new.decTable <- SF.applyDecTable(rice.discretized1, reduct, control = list(indx.reduct = 1))


control.1 <- list(type.relation = c("crisp"),
                  type.aggregation = c("crisp"),
                  t.implicator = "lukasiewicz", type.LU = "implicator.tnorm")
res.1 <- BC.discernibility.mat.FRST(rice.discretized1, type.discernibility = "standard.red",
                                    control = control.1)

## generate single reduct
reduct <- FS.all.reducts.computation(res.1)

## generate new decision table
new.decTable <- SF.applyDecTable(decision.table, reduct)
