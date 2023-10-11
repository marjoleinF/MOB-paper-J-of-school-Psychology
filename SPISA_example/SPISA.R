library(psychotree)
library(raschtreeMH)
load("/home/mhenninger/Arbeit/Forschung/Data/SPISA/spisa_ges.RData")

spisa <- spisa[spisa$Bogen == 20, ]

# translate covariates into english
covar_dat <- spisa[, c("Alter", "Geschlecht", "Student", "Tat", "Richtung")]
colnames(covar_dat) <- c("Age", "Gender", "Student", "Occupation", "Area")
levels(covar_dat$Gender) <- c("Female", "Male", "Missing")
levels(covar_dat$Student) <- c("No", "Yes", "Missing")

# combine factor levels of occupation
covar_dat$Occupation <- forcats::fct_collapse(covar_dat$Occupation, 
                                              Arbeitnehmer = c("Vollzeit", "Teilzeit"),
                                              nicht_berufstaetig = c("arbeitlos", "nicht berufstaetig"),
                                              Ruhestand = c("Ruhestand", "Ruhe+Tat")
)
levels(covar_dat$Occupation) <- c("\n\n\nPupil", "\n\n\nStudent", "\nPhD",
                                  "\nin Education", "\nEmployed",
                                  "\nUnemployed", "\nRetired",
                                  "\nOthers")
covar_dat[!(covar_dat$Gender %in% c("Female", "Male")),]$Gender <- "Missing"
levels(covar_dat$Area) <- c("Language & Culture", "\nLaw and Economics", "\nMedicine & Health", "\nEngineering",
                            "\nSciences, Pharmacy, Geography", "\nAgriculture & Nutrition", "\nSports", "\nArts",
                            "\nno Information")

culture_scale <- apply(spisa[,28:36],2,as.numeric)

dat <- covar_dat
dat$culture_scale <- culture_scale

# only students
dat <- dat[dat$Student == "Yes", ]
dat <- dat[complete.cases(dat),]
# dat <- dat[dat$Area != "Missing", ]

# set.seed(13081990)
# dat_SPISA <- dat[sample(nrow(dat), 5000),]
dat_SPISA <- dat

culture_scale <- dat_SPISA$culture_scale
covar_data <- dat_SPISA[,1:5]
save(culture_scale, covar_data, file = "SPISA_example/dat_SPISA.rda")


dat_SPISA <- covar_data
dat_SPISA$culture <- culture_scale
tree_culture <- raschtree(culture ~  Gender + Age + Occupation + Area, 
                          data = dat_SPISA)
plot(tree_culture)

tree_MH_culture <- raschtree(culture ~  Gender + Age + Occupation + Area, 
                             data = dat_SPISA, 
                             stopfun= stopfun_mantelhaenszel(purification = "iterative"))
plot(tree_MH_culture)


tree_culture <- add_mantelhaenszel(tree_culture, purification = "iterative")
tree_MH_culture <- add_mantelhaenszel(tree_MH_culture, purification = "iterative")

plot(tree_culture)
plot(tree_MH_culture)


pdf(file = "SPISA_example/tree_culture.pdf", width = 16,  height = 10)
plot(tree_culture, show_classification = FALSE)
dev.off()

pdf(file = "SPISA_example/tree_culture_MH.pdf", width = 20,  height = 12)
plot(tree_culture)
dev.off()

pdf(file = "SPISA_example/tree_culture_MH_stopped_1.pdf", width = 10,  height = 10)
plot(tree_MH_culture, color_by_node = 1)
dev.off()

pdf(file = "SPISA_example/tree_culture_MH_stopped_2.pdf", width = 10,  height = 10)
plot(tree_MH_culture, color_by_node = 2)
dev.off()

pdf(file = "SPISA_example/tree_culture_MH_stopped_4.pdf", width = 10,  height = 10)
plot(tree_MH_culture, color_by_node = 4)
dev.off()

