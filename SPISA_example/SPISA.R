library(psychotree)
library(raschtreeMH)
load("/home/mhenninger/Arbeit/Forschung/Data/SPISA/spisa_ges.RData")

spisa <- spisa[spisa$Bogen == 20, ]

# translate covariates into english
covar_dat <- spisa[, c("Alter", "Geschlecht", "Student", "Richtung")]
colnames(covar_dat) <- c("Age", "Gender", "Student", "Area")
levels(covar_dat$Gender) <- c("Female", "Male", "Missing")
levels(covar_dat$Student) <- c("No", "Yes", "Missing")
covar_dat[!(covar_dat$Gender %in% c("Female", "Male")),]$Gender <- "Missing"
covar_dat$Area <- as.factor(covar_dat$Area)
levels(covar_dat$Area) <- c("Language & Culture", "Law and Economics", "Medicine & Health", "Engineering",
                            "Sciences, Pharmacy, Geography", "Agriculture & Nutrition", "Sports", "Arts",
                            "no Information")

culture_scale <- apply(spisa[,28:36],2,as.numeric)

dat <- covar_dat
dat$culture <- culture_scale

# only students
dat <- dat[dat$Student == "Yes", ]
dat$Student <- NULL
dat <- dat[complete.cases(dat),]

dat_SPISA <- dat

save(dat_SPISA, file = "../dat_SPISA.Rda")



# dat_SPISA <- covar_data
# dat_SPISA$culture <- culture_scale
# tree_culture <- raschtree(culture ~  Gender + Age + Occupation + Area, 
#                           data = dat_SPISA)
# plot(tree_culture)
# 
# tree_MH_culture <- raschtree(culture ~  Gender + Age + Occupation + Area, 
#                              data = dat_SPISA, 
#                              stopfun= stopfun_mantelhaenszel(purification = "iterative"))
# plot(tree_MH_culture)
# 
# 
# tree_culture <- add_mantelhaenszel(tree_culture, purification = "iterative")
# tree_MH_culture <- add_mantelhaenszel(tree_MH_culture, purification = "iterative")
# 
# plot(tree_culture)
# plot(tree_MH_culture)
# 
# 
# pdf(file = "SPISA_example/tree_culture.pdf", width = 16,  height = 10)
# plot(tree_culture, show_classification = FALSE)
# dev.off()
# 
# pdf(file = "SPISA_example/tree_culture_MH.pdf", width = 20,  height = 12)
# plot(tree_culture)
# dev.off()
# 
# pdf(file = "SPISA_example/tree_culture_MH_stopped_1.pdf", width = 10,  height = 10)
# plot(tree_MH_culture, color_by_node = 1)
# dev.off()
# 
# pdf(file = "SPISA_example/tree_culture_MH_stopped_2.pdf", width = 10,  height = 10)
# plot(tree_MH_culture, color_by_node = 2)
# dev.off()
# 
# pdf(file = "SPISA_example/tree_culture_MH_stopped_4.pdf", width = 10,  height = 10)
# plot(tree_MH_culture, color_by_node = 4)
# dev.off()
# 
