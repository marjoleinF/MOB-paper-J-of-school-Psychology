### R code from vignette source '_MOB_paper.Rnw'

## suppress printing of > and + before code, it is annoying for copy-pasting
options(prompt=" ", continue = " ") 


set.seed(3)
x1 <- runif(250, min = 5, max = 11) ## age
x2 <- runif(250, min = 2, max = 8) ## gender
x3 <- runif(250, min = 4, max = 10) ## reading comprehension
error <- rnorm(250, sd = 1.5)
y <- 7 + ifelse(x1 > 8 & x2 > 5, yes = 2.5, no = -2.5) + error ## total SDQ score (0-40)
toy_data <- round(data.frame(x1, x2, x3, y), digits = 2L)
toy_data$x2 <- factor(ifelse(x2 > 5, yes = ifelse(x2 > 6.5, "A", "C"), no = "B"))


library("colorspace")
boxplot(toy_data$y, cex = .7)


library("partykit")
tree <- lmtree(y ~ 1 | x1 + x2 + x3, data = toy_data)
plot(tree, gp = gpar(cex = .7), ylim = c(1,14))
term_node_means <- round(tapply(toy_data$y, predict(tree, type = "node"), mean), digits= 2L)


library("strucchange")
gefp_x1 <- gefp(y ~ 1, fit = lm, data = toy_data, order.by = ~ x1)
gefp_x3 <- gefp(y ~ 1, fit = lm, data = toy_data, order.by = ~ x3)
lmod <- lm(y ~ 1, data = toy_data)
resids <- residuals(lmod)
par(mfrow = c(1, 3))
plot(toy_data$x1, resids, xlab = "x1", ylab = "residuals", 
     cex = 1.2, cex.lab = 1.2, cex.axis = 1.2)
plot(toy_data$x2, resids, xlab = "x2", ylab = "residuals", 
     cex = 1.2, cex.lab = 1.2, cex.axis = 1.2)
plot(toy_data$x3, resids, xlab = "x3", ylab = "residuals", 
     cex = 1.2, cex.lab = 1.2, cex.axis = 1.2)


par(mfrow = c(1, 3))
plot(toy_data$x1[order(toy_data$x1)], cumsum(resids[order(toy_data$x1)]), 
     xlab = "x1", ylab = "cumulative sum", type = "l",
     cex = 1.2, cex.lab = 1.2, cex.axis = 1.2)
boxplot(t(replicate(100, tapply(resids, toy_data$x2, sum))), 
        ylab = "sum", cex = 1.2, cex.lab = 1.2, cex.axis = 1.2)
plot(toy_data$x3[order(toy_data$x3)], cumsum(resids[order(toy_data$x3)]), 
     xlab = "x3", ylab = "cumulative sum", type = "l",
     cex = 1.2, cex.lab = 1.2, cex.axis = 1.2)


## Load data
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

## Node-specific predictor
data$program <- factor(ifelse(data$None2_90 == 1, "None", 
                              ifelse(data$Pre2_90 == 1, "Pre", "HS")),
                       levels = c("None", "HS"))

## Partitioning variables

## ageAFTQ: age-adjusted AFQT
## Race_child: Hispanic=Race_Child==1; Black=Race_Child==2; White=Race_Child==3
data$Race_Child <- factor(ifelse(data$Race_Child == 1, "Hispanic", 
                                 ifelse(data$Race_Child == 2, "Black", "White")))
## family income: PermInc
## Mom's years of education: MothED
part_vars <- c("AgeAFQT", "Race_Child", "PermInc", "Height_Mom81", "MothED")
id_vars <- c("ChildID", "MotherID")

## Outcome: Peabody Picture Vocabulary Test (PPVT)
testscore_vars <- c(paste0("PPVT_Raw", 2*(43:52)), paste0("AgeTest_Yr", 2*(43:52)))

## Create long dataset
pred_vars <- "program"
HS_dat <- reshape(data[ , c(part_vars, id_vars, testscore_vars, pred_vars)], 
                  varying = list(paste0("PPVT_Raw", 2*(43:52)),
                                 paste0("AgeTest_Yr", 2*(43:52))),
                  direction = "long", 
                  v.names = c("PPVT_Raw", "AgeTest_Yr"))
HS_dat <- HS_dat[ , -which(names(HS_dat) %in% c("time", "id"))]
names(HS_dat) <- c("AFTQ", "Race", "Income", "Mom_height", "Mom_edu_yrs", 
                   "ChildID", "MotherID", "Program", "PPVT", "Age")
HS_dat <- HS_dat[complete.cases(HS_dat), ]
HS_dat$Age_orig <- HS_dat$Age
#HS_dat$Age <- sqrt(HS_dat$Age)
rownames(HS_dat) <- NULL
save(HS_dat, file = "HS_dat.Rda")


load("HS_dat.Rda")
head(HS_dat, 3)

hist(tapply(HS_dat$Mom_height, HS_dat$MotherID, mean))


library("lme4")
lmm <- lmer(PPVT ~ Program*Age + (1|MotherID/ChildID), data = HS_dat)


beta <- fixef(lmm)
plot(jitter(HS_dat$Age), HS_dat$PPVT, col = HS_dat$Program,
     cex = .5, #cex.lab = .7, cex.axis=.7, cex.main = .7,
     xlab = "Age (years)", ylab = "PPVT score", xaxt = "n",
     main = paste0("Global Model (", length(unique(HS_dat$ChildID)), " children)"))
abline(a = beta["(Intercept)"], b = beta["Age"], col = "black")
abline(a = beta["(Intercept)"]+beta["ProgramHS"], 
       b = beta["Age"]+beta["ProgramHS:Age"], col = "red")
axis(1, #at=sqrt(c(3, 5, 8, 12, 17)), labels=c(3, 5, 8, 12, 17), 
     cex.axis = .8)


library("glmertree")
HS_tree <- lmertree(PPVT ~ Program*Age | (1|MotherID/ChildID) | AFTQ + Race + 
                      Income + Mom_edu_yrs + Mom_height, 
                    data = HS_dat, cluster = MotherID, minsize = 250)


coefs <- round(coef(HS_tree), digits = 2)
design_df <- data.frame(.tree = rep(sort(unique(HS_tree$data$.tree)), each = 2),
                        Program = factor(rep(c("None", "HS"))))
design_df$`PPVT at age 6` <- predict(HS_tree$lmer, newdata = cbind(design_df, Age = 6), 
                                     re.form = ~0)
design_df$`PPVT at age 12` <- predict(HS_tree$lmer, newdata = cbind(design_df, Age = 12), 
                                      re.form = ~0)
names(design_df)[1] <- "Node"

library("kableExtra")
add_footnote(
knitr::kable(design_df, format = "latex", digits = 2, 
             caption = "Node-specific predicted PPVT scores at different ages.", 
             label = "predictions", centering = FALSE,
             linesep = "", # linesep command suppressess addlinesep every 5 rows
             row.names = FALSE, escape=TRUE, align = c("cccc"), booktabs = TRUE),
#latex_options = "HOLD_position")
"\textit{Note.} For computing predictions, random effects were assumed zero. HS = Head Start; PPVT = Peabody Picture Vocabulary Test.", notation = "none")


plot(HS_tree, type = "simple", which = "tree", gp = gpar(cex = .5), nodesize_level = 2)


nodes <- predict(HS_tree, type = "node")
beta <- coef(HS_tree)
par(mfrow = c(1, 4))
for (node in sort(unique(nodes))) {
  plot(HS_dat$Age[nodes == node], HS_dat$PPVT[nodes == node],
       col = HS_dat$Program[nodes == node],
       cex = .5, cex.lab = .7, cex.axis=.7, cex.main = .7,
       xlab = "Age", ylab = "PPVT", #xlim = c(sqrt(3), sqrt(20)), 
       ylim = c(0, 150))
  abline(a = beta[as.character(node), "(Intercept)"], b = beta[as.character(node), "Age"], col = "black")
  abline(a = beta[as.character(node), "(Intercept)"] + beta[as.character(node), "ProgramHS"], 
         b = beta[as.character(node), "Age"] + beta[as.character(node), "ProgramHS:Age"], col = "red")
}


load("dat_SPISA.Rda")


load("dat_SPISA.Rda")
head(dat_SPISA, 3)


library("psychotree")
if(!file.exists("SPISA_example/Raschtree.rda")){
  # relabel factors of the area covariate so that the Raschtree is not too big: 
  levels(dat_SPISA$Area) <- c("Lang & Cult", "\nLaw and Econ",
                              "\nMed & Health", "\nEngin",
                              "\nSci, Phar, Geo",
                              "\nAgri & Nutri",
                              "\nSports", "Arts",
                              "\nno Info")
  Raschtree_culture <- raschtree(culture ~  Gender + Age + Area,
                                 data = dat_SPISA)
  save(Raschtree_culture, file = "SPISA_example/Raschtree.rda")
}
load("SPISA_example/Raschtree.rda")


plot(Raschtree_culture)


library(devtools)
install_github("mirka-henninger/raschtreeMH")
library("raschtreeMH")


if(!file.exists("SPISA_example/Raschtree_MH_culture.rda")){
  Raschtree_MH_culture <- raschtree(culture ~  Gender + Age + Area,
                                    data = dat_SPISA,
                                    stopfun= stopfun_mantelhaenszel(purification = "iterative"))
  save(Raschtree_MH_culture, file = "SPISA_example/Raschtree_MH_culture.rda")
}
load("SPISA_example/Raschtree_MH_culture.rda")


Raschtree_MH_culture <- add_mantelhaenszel(Raschtree_MH_culture,
                                           purification = "iterative")


plot(Raschtree_MH_culture)


plot(Raschtree_MH_culture, color_by_node = 1)


plot(Raschtree_MH_culture, color_by_node = 4)


Raschtree_MH_culture$info$mantelhaenszel


Stangle(file='_MOB_paper.Rnw', output = '_MOB_paper.R', ,encoding='utf8',
        drop.evalFALSE = TRUE, annotate = FALSE)


