# Much of the code was modified from what was provided in the MatchIt 
# documentation in order to perform matching on more than just one method, 
# but we have cited our use of the package/documentation in the poster.

library(MatchIt)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(MatchIt)
library(corrplot)
library(marginaleffects)
library(cobalt)


# data(package = "MatchIt")

data("lalonde")

?lalonde

str(lalonde)

sum(lalonde$re78 != 0 & lalonde$re75 != 0, na.rm = TRUE)
sum(lalonde$re78 != 0 & lalonde$re75 != 0 & lalonde$re74 != 0, na.rm = TRUE)


###############################################################################


lalonde <- lalonde %>%
  mutate(diff_income = lalonde$re78 - lalonde$re75)


################################################################################

# Checking propensity score and significant level for each variable


propensity_score <- glm(treat ~ age + educ + race + married + nodegree  + re75 + re74, data = lalonde, family = binomial)
summary(propensity_score)


################################################################################

match_nearest_3 <- matchit(treat ~ age + educ + race + married + nodegree  + re75 + re74, method = "nearest", data = lalonde, distance = "glm", link="probit")

summary(match_nearest_3, un = FALSE)

love.plot(match_nearest_3, drop.distance = TRUE, abs = TRUE,line = FALSE, sample.names = c("All", "Nearest Matching"), colors = c("red", "blue"), thresholds = c(m = .1), stars="raw", title = "Nearest Neighbor Covariate Balance")
plot(match_nearest_3, type = "histogram")


matched_data_nearest_3 <- match.data(match_nearest_3) # Throwing away the data points without its pair

subclass_count_3 <- table(matched_data_nearest_3$subclass) # Confirmation to make sure everyone has pairs
print(subclass_count_3)

fit_nearest <- lm(re78 ~ treat * (age + educ + race + married +
                            nodegree + re74 + re75),
          data = matched_data_nearest_3,
          weights = weights)

att_nearest <- avg_comparisons(fit_nearest,
                variables = "treat",
                vcov = ~subclass,
                newdata = subset(treat == 1))[[3]]

################################################################################



match_nearest_4 <- matchit(treat ~ age + educ + race + married + nodegree  + re75 + re74, method = "full", data = lalonde, distance = "glm", link="probit")

summary(match_nearest_4, un = FALSE)


love.plot(match_nearest_4, drop.distance = TRUE, abs = TRUE,line = FALSE, sample.names = c("All", "Optimal Full Matching"), colors = c("red", "darkgreen"), thresholds = c(m = .1), stars="raw", title = "Optimal Full Covariate Balance")
plot(match_nearest_4, type = "histogram")


matched_data_nearest_4 <- match.data(match_nearest_4) # Throwing away the data points without its pair


subclass_count_4 <- table(matched_data_nearest_4$subclass) # Confirmation to make sure everyone has pairs
print(subclass_count_4)

fit_optimal <- lm(re78 ~ treat * (age + educ + race + married +
                                    nodegree + re74 + re75),
                  data = matched_data_nearest_4,
                  weights = weights)

att_optimal <- avg_comparisons(fit_optimal,
                variables = "treat",
                vcov = ~subclass,
                newdata = subset(treat == 1))[[3]]

################################################################################

match_nearest_5 <- matchit(treat ~ age + educ + race + married + nodegree  + re75 + re74, method = "subclass", data = lalonde, distance = "glm", link="probit")

summary(match_nearest_5, un = FALSE)


love.plot(match_nearest_5, drop.distance = TRUE, abs = TRUE,line = FALSE, sample.names = c("All", "Subclass Matching"), colors = c("red", "deeppink4"), thresholds = c(m = .1), stars="raw", title = "Subclass Covariate Balance")
plot(match_nearest_5, type = "histogram")


matched_data_nearest_5 <- match.data(match_nearest_5) # Throwing away the data points without its pair


subclass_count_5 <- table(matched_data_nearest_5$subclass) # Confirmation to make sure everyone has pairs
print(subclass_count_5)

fit_subclass <- lm(re78 ~ treat * (age + educ + race + married +
                                    nodegree + re74 + re75),
                  data = matched_data_nearest_5,
                  weights = weights)

att_subclass <- avg_comparisons(fit_subclass,
                variables = "treat",
                vcov = ~subclass,
                newdata = subset(treat == 1))[[3]]

################################################################################

x <- subset(lalonde, lalonde$treat==1)
y <- subset(lalonde, lalonde$treat==0)

fit <- lm(re78 ~ treat * (age + educ + race + married +
                                    nodegree + re74 + re75),
                  data = lalonde)

att_no_match <- avg_comparisons(fit,
                variables = "treat")[[3]]


Matching_Method <- c("No Match", "Nearest Neighbor", "Optimal Full", "Subclass")
ATT <- c(att_no_match, att_nearest, att_optimal, att_subclass)
barplot_df <- data.frame(Matching_Method, ATT)
barplot_df$Matching_Method <- factor(barplot_df$Matching_Method, levels=unique(barplot_df$Matching_Method))
ggplot(data=barplot_df, aes(x=Matching_Method, y=ATT)) + geom_bar(stat = 'identity') + geom_text(aes(label = round(ATT)), nudge_y = 100)

