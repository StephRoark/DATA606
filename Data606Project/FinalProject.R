library(tidyverse)
library(psych)
library(corrplot)
library(bestglm)
library(caret)
library(rsq)

bc_data <- read.csv("wisc_bc_data.csv")

#create bucket variable for radius mean which is either top half or bottom half
#bc_data <- bc_data %>% 
#    mutate(radius_mean_size = ifelse(radius_mean < 19.7,"bottom half", "top half") )

str(bc_data)
head(bc_data)
summary(bc_data)

describe(bc_data %>% select(-id, -diagnosis))

table(bc_data$diagnosis)

colnames(bc_data)
    
bc_model <- glm(diagnosis ~ radius_mean + texture_mean + perimeter_mean + 
                 area_mean + smoothness_mean + compactness_mean + 
                 concavity_mean + concave.points_mean + symmetry_mean + 
                 fractal_dimension_mean + radius_se + texture_se + perimeter_se +
                 area_se + smoothness_se + compactness_se + concavity_se + 
                 concave.points_se + symmetry_se + fractal_dimension_se + 
                 radius_worst + texture_worst + perimeter_worst + area_worst +
                 smoothness_worst + compactness_worst + concavity_worst + 
                 concave.points_worst + symmetry_worst + fractal_dimension_worst, 
                data = bc_data, family=binomial(link="logit"), control = list(maxit = 50))

corMatMy <- cor(bc_data[,3:32])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)

highlyCor <- colnames(bc_data)[findCorrelation(corMatMy, cutoff = 0.75, verbose = TRUE)]
bc_data_cor <- bc_data[, which(!colnames(bc_data) %in% highlyCor)]

train <- cbind(bc_data_cor,diagnosis=bc_data[,2])
bestglms <- bestglm(train, family=binomial) #, method="forward")
e <- resid(bestglms$BestModel)
qqnorm(e, ylab="residuals, best model")
hist(e)
summary(bestglms$BestModel)

plot(bestglms$BestModel)  # not meaningful

bc_model_step <- glm(diagnosis ~ texture_mean + concavity_mean +
                         perimeter_se + smoothness_se + fractal_dimension_se +
                         perimeter_worst + fractal_dimension_worst, 
                     data = bc_data, family=binomial(link="logit"),
                     control = list(maxit = 50))
summary(bc_model_step)
rsq(bc_model_step)
rsq(bc_model_step, adj=TRUE)


bc_model_step1 <- glm(diagnosis ~ radius_mean,
                     data = bc_data, family=binomial(link="logit"),
                     control = list(maxit = 50))
summary(bc_model_step1)
plot(residuals(bc_model_step1))


