## Load dataset

## This is the openly available dataset on which I ran the STATA script 
data <- data.frame(haven::read_dta("data_Deming_2008_0217_MF.dta"))
names(data)

## This is the openly available dataset
data2 <- haven::read_dta("data_Deming_2008_0217.dta")
names(data2)

names(data)[!names(data) %in% names(data2)]


#############################
##
## Sample Eligibility
##

## First create rules that establish sample eligibility
## Rule is 5 years old by 1990, so that they will be 19 by 2004
## Next restrict the sample to families with at least 2 age-eligible children
## Finally restrict to families where at least one (but not all) children were in Head Start

############################################################################################
# I create 3 sets of variables that define preschool participation. The first simply uses   
# the age variable and excludes those with any missing data. The second substitutes the PPVT
# age (ie age at test) when the original variable is unavailable, and also substitutes past 
# or future age variables from other survey years (plus or minus 24 months). The third is   
# the most restrictive definition. It codes inconsistent or missing responses across years  
# as zeros, as well as for children for whom mothers report "3 months or less" participation
# in the program. The variables are coded HS1, HS2, HS3 and Pre1, Pre2, Pre3. In general    
# different rules have very little impact on the estimates. I use #2 in the paper.          
############################################################################################

# Load the dataset (assuming it's in Stata format)
data <- data.frame(haven::read_dta("data_Deming_2008_0217_MF.dta"))

## Keep only eligible children in HS or no preschool
data <- data[data$Elig2_90 == 1, ]
data <- data[data$None2_90 == 1 | data$HS2_90 == 1, ]
data <- data[data$MotherID %in% names(table(data$MotherID) >= 2), ]
data$program <- factor(ifelse(data$None2_90 == 1, "None", 
                              ifelse(data$Pre2_90 == 1, "Pre", "HS")))

## Keep only families in which children differ in HS participation
selected_family <- rowSums(table(data$MotherID, data$program) > 0) == 2
selected_family <- names(selected_family)[selected_family]
data <- data[data$MotherID %in% selected_family, ]
  


####################################
##
## Node-specific predictor
##
table(data$None2_90, data$HS2_90, data$Pre2_90)
data$program <- factor(ifelse(data$None2_90 == 1, "None", 
                              ifelse(data$Pre2_90 == 1, "Pre", "HS")),
                       levels = c("None", "HS"))




#################################
##
## Partitioning variables
##

## kids with same mother must have same score for age-adjusted AFQT
table(tapply(data$AgeAFQT, data$MotherID, sd, na.rm = TRUE))

## do kids with same mother have same race 
## Hispanic=Race_Child==1;
## Black=Race_Child==2;
## White=Race_Child==3;
table(tapply(data$Race_Child, data$MotherID, sd, na.rm = TRUE))
data$Race_Child <- factor(ifelse(data$Race_Child == 1, "Hispanic", 
                          ifelse(data$Race_Child == 2, "Black", "White")))

## do kids with same mother have same family income
table(tapply(data$PermInc, data$MotherID, sd, na.rm = TRUE))

## do kids with same mother have same family income
table(tapply(data$MomDropout, data$MotherID, sd, na.rm = TRUE))
data$MomDropout <- factor(data$MomDropout)

## mom's education
table(tapply(data$MomHS, data$MotherID, sd, na.rm = TRUE))
table(tapply(data$MomSomeColl, data$MotherID, sd, na.rm = TRUE))
table(tapply(data$Height_Mom81, data$MotherID, sd, na.rm = TRUE))
table(tapply(data$MothED, data$MotherID, sd, na.rm = TRUE))






## Outcome variables
##
## I construct summary indices for the three test scores, an index of the nontest 
## score school-age outcomes (grade retention and learning disability diagnosis), 
## and an indexof the six long-term outcomes. Later, the test score index is 
## separated further into agecategories to look at the initial effect of Head Start 
## and fade-out over time.

part_vars <- c("AgeAFQT", "Race_Child", "PermInc", "MomDropout", "Height_Mom81", "MothED")
id_vars <- c("ChildID", "MotherID")

## "three tests analyzed are the Peabody Picture Vocabulary Test (PPVT), 
## the Peabody Individual Achievement Math (PIATMT) subtest, 
## and Reading Recognition (PIATRR) subtest.
## BPI is behaviour problems index

testscore_vars <- c(paste0("PPVT_Raw", 2*(43:52)),
                    paste0("PIATMT_Raw", 2*(43:52)),
                    paste0("PIATRR_Raw", 2*(43:52)),
                    paste0("Test_Pct", 2*(43:52)),
                    paste0("BPI_Raw", 2*(43:52)),
                    paste0("BPIAS_Raw", 2*(43:52)),
                    paste0("AgeTest_Yr", 2*(43:52)))
pred_vars <- "program"

id_vars %in% names(data)
part_vars %in% names(data)
testscore_vars %in% names(data)
pred_vars %in% names(data)

long_dat <- reshape(data[ , c(part_vars, id_vars, testscore_vars, pred_vars)], 
                    varying = list(paste0("PPVT_Raw", 2*(43:52)),
                                   paste0("PIATMT_Raw", 2*(43:52)),
                                   paste0("PIATRR_Raw", 2*(43:52)),
                                   paste0("Test_Pct", 2*(43:52)),
                                   paste0("BPI_Raw", 2*(43:52)),
                                   paste0("BPIAS_Raw", 2*(43:52)),
                                   paste0("AgeTest_Yr", 2*(43:52))),
                    direction = "long", 
                    v.names = c("PPVT_Raw", "PIATMT_Raw", "PIATRR_Raw", "Test_Pct", "BPI_Raw", 
                                "BPIAS_Raw", "AgeTest_Yr"))
dim(data)
dim(long_dat)




##########################
##
## Fit trees
##

## PPVT
PPVT_dat <- long_dat[rowSums(sapply(long_dat[ , c("PPVT_Raw", pred_vars, part_vars)], is.na)) == 0, ]
dim(PPVT_dat)
PPVT_tree <- lmertree(PPVT_Raw ~ AgeTest_Yr*program | ChildID | AgeAFQT + Race_Child +
                        PermInc + MomDropout + Height_Mom81 + MothED, 
                      cluster = ChildID, data = PPVT_dat, minsize = 250)
plot(PPVT_tree, type = "simple", which = "tree", gp = gpar(cex = .5), nodesize_level = 2)
coef(PPVT_tree)


## PIATMT
PIATMT_dat <- long_dat[rowSums(sapply(long_dat[ , c("PIATMT_Raw", pred_vars, part_vars)], is.na)) == 0, ]
dim(PIATMT_dat)
PIATMT_tree <- lmertree(PIATMT_Raw ~ AgeTest_Yr*program | ChildID | AgeAFQT + Race_Child +
                        PermInc + MomDropout + Height_Mom81 + MothED, 
                      cluster = ChildID, data = PIATMT_dat, minsize = 250)
plot(PIATMT_tree, type = "simple", which = "tree", gp = gpar(cex = .5), nodesize_level = 2)
coef(PIATMT_tree)

## PIATRR
PIATRR_dat <- long_dat[rowSums(sapply(long_dat[ , c("PIATRR_Raw", pred_vars, part_vars)], is.na)) == 0, ]
dim(PIATRR_dat)
PIATRR_tree <- lmertree(PIATRR_Raw ~ AgeTest_Yr*program | ChildID | AgeAFQT + Race_Child +
                          PermInc + MomDropout + Height_Mom81 + MothED, 
                        cluster = ChildID, data = PIATRR_dat, minsize = 250)
plot(PIATRR_tree, type = "simple", which = "tree", gp = gpar(cex = .5), nodesize_level = 2)
coef(PIATRR_tree)


## Composite test score
test_dat <- long_dat[rowSums(sapply(long_dat[ , c("Test_Pct", pred_vars, part_vars)], is.na)) == 0, ]
dim(test_dat)
test_tree <- lmertree(Test_Pct ~ sqrt(AgeTest_Yr)*program | ChildID | AgeAFQT + Race_Child +
                          PermInc + Height_Mom81 + MothED, 
                        cluster = ChildID, data = test_dat, minsize = 250)
plot(test_tree, type = "simple", which = "tree", gp = gpar(cex = .5), nodesize_level = 2)
coef(test_tree)





##################################
##
## Cross-validated accuracy
## 

nreps <- 10
MSEs_t <- data.frame(PPVT = rep(NA, times = 10),
                   PIATRR = rep(NA, times = 10),
                   PIATMT = rep(NA, times = 10))
sizes <- MSEs_l <- MSEs_t

## PPVT
set.seed(42)
fold_ids <- sample(nreps, size = length(unique(PPVT_dat$MotherID)), replace = TRUE)
PPVT_dat$fold_id <- NA
for (mom in unique(PPVT_dat$MotherID)) {
  PPVT_dat$fold_id[PPVT_dat$MotherID == mom] <- fold_ids[unique(PPVT_dat$MotherID) == mom]
}
table(PPVT_dat$fold_id, useNA = "ifany")
for (i in 1:nreps) {
  ## Fit tree on training data
  PPVT_t <- lmertree(PPVT_Raw ~ AgeTest_Yr*program | ChildID | AgeAFQT + Race_Child +
                          PermInc + MomDropout + Height_Mom81 + MothED, 
                        cluster = ChildID, data = PPVT_dat[PPVT_dat$fold_id != i, ], minsize = 225)
  PPVT_l <- lmer(PPVT_Raw ~ AgeTest_Yr*program + (1|ChildID), 
                     data = PPVT_dat[PPVT_dat$fold_id != i, ])

  ## Evaluate on test data
  sizes[i, "PPVT"] <- length(PPVT_t$tree)
  preds <- predict(PPVT_t, newdata = PPVT_dat[PPVT_dat$fold_id == i, ], re.form = ~0)
  MSEs_t[i, "PPVT"] <- mean((preds - PPVT_dat[PPVT_dat$fold_id == i, "PPVT_Raw"])^2)
  preds <- predict(PPVT_l, newdata = PPVT_dat[PPVT_dat$fold_id == i, ], re.form = ~0)
  MSEs_l[i, "PPVT"] <- mean((preds - PPVT_dat[PPVT_dat$fold_id == i, "PPVT_Raw"])^2)
  
}


## PIATMT
set.seed(42)
fold_ids <- sample(nreps, size = length(unique(PIATMT_dat$MotherID)), replace = TRUE)
PIATMT_dat$fold_id <- NA
for (mom in unique(PIATMT_dat$MotherID)) {
  PIATMT_dat$fold_id[PIATMT_dat$MotherID == mom] <- fold_ids[unique(PIATMT_dat$MotherID) == mom]
}
table(PIATMT_dat$fold_id, useNA = "ifany")
for (i in 1:nreps) {
  ## Fit tree on training data
  PIATMT_t <- lmertree(PIATMT_Raw ~ AgeTest_Yr*program | ChildID | AgeAFQT + Race_Child +
                       PermInc + MomDropout + Height_Mom81 + MothED, 
                     cluster = ChildID, data = PIATMT_dat[PIATMT_dat$fold_id != i, ], minsize = 225)
  PIATMT_l <- lmer(PIATMT_Raw ~ AgeTest_Yr*program + (1|ChildID), 
                       data = PIATMT_dat[PIATMT_dat$fold_id != i, ])
  
  ## Evaluate on test data
  sizes[i, "PIATMT"] <- length(PIATMT_t$tree)
  preds <- predict(PIATMT_t, newdata = PIATMT_dat[PIATMT_dat$fold_id == i, ], re.form = ~0)
  MSEs_t[i, "PIATMT"] <- mean((preds - PIATMT_dat[PIATMT_dat$fold_id == i, "PIATMT_Raw"])^2)
  preds <- predict(PIATMT_l, newdata = PIATMT_dat[PIATMT_dat$fold_id == i, ], re.form = ~0)
  MSEs_l[i, "PIATMT"] <- mean((preds - PIATMT_dat[PIATMT_dat$fold_id == i, "PIATMT_Raw"])^2)
}


## PIATRR
set.seed(42)
fold_ids <- sample(nreps, size = length(unique(PIATRR_dat$MotherID)), replace = TRUE)
PIATRR_dat$fold_id <- NA
for (mom in unique(PIATRR_dat$MotherID)) {
  PIATRR_dat$fold_id[PIATRR_dat$MotherID == mom] <- fold_ids[unique(PIATRR_dat$MotherID) == mom]
}
table(PIATRR_dat$fold_id, useNA = "ifany")
for (i in 1:nreps) {
  ## Fit tree and benchmark LMM on training data
  PIATRR_t <- lmertree(PIATRR_Raw ~ AgeTest_Yr*program | ChildID | AgeAFQT + Race_Child +
                         PermInc + MomDropout + Height_Mom81 + MothED, 
                       cluster = ChildID, data = PIATRR_dat[PIATRR_dat$fold_id != i, ], minsize = 225)
  PIATRR_l <- lmer(PIATRR_Raw ~ AgeTest_Yr*program + (1|ChildID), 
                   data = PIATRR_dat[PIATRR_dat$fold_id != i, ])
  
  ## Evaluate on test data
  sizes[i, "PIATRR"] <- length(PIATRR_t$tree)
  preds <- predict(PIATRR_t, newdata = PIATRR_dat[PIATRR_dat$fold_id == i, ], re.form = ~0)
  MSEs_t[i, "PIATRR"] <- mean((preds - PIATRR_dat[PIATRR_dat$fold_id == i, "PIATRR_Raw"])^2)
  preds <- predict(PIATRR_l, newdata = PIATRR_dat[PIATRR_dat$fold_id == i, ], re.form = ~0)
  MSEs_l[i, "PIATRR"] <- mean((preds - PIATRR_dat[PIATRR_dat$fold_id == i, "PIATRR_Raw"])^2)
}

## Tree size 
colMeans(sizes)

## Performance of trees
colMeans(1 - MSEs_t / c(var(PPVT_dat$PPVT_Raw), var(PIATRR_dat$PIATRR_Raw), var(PIATMT_dat$PIATMT_Raw)))

## Performance of LMMS
colMeans(1 - MSEs_l / c(var(PPVT_dat$PPVT_Raw), var(PIATRR_dat$PIATRR_Raw), var(PIATMT_dat$PIATMT_Raw)))

