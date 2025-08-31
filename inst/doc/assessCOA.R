## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##"
)

## ----setup--------------------------------------------------------------------
library(Keng)
library(ggplot2)
library(tidyr)

## -----------------------------------------------------------------------------
data <- data.frame(
session1 = 60 + sample.int(40, 100, 1), 
session2 = 60 + sample.int(40, 100, 1))

## -----------------------------------------------------------------------------
session_weights <- c(0.4, 0.6)

## -----------------------------------------------------------------------------
objective_weights1 <- c(0.5, 0.3, 0.2)
objective_weights2 <- c(0.1, 0.3, 0.6)

## -----------------------------------------------------------------------------
coa <- assess_coa(data, session_weights, objective_weights1, objective_weights2)

## -----------------------------------------------------------------------------
head(coa)

## -----------------------------------------------------------------------------
attr(coa, "weights")

## -----------------------------------------------------------------------------
coa_overall <- data.frame(
  objective = factor(1:nrow(attr(coa, "weights")[[2]])),
  achievement = round(colMeans(coa[row.names(attr(coa, "weights")[[2]])]), 2)
)

ggplot(coa_overall, aes(objective, achievement)) +
  geom_bar(aes(fill = objective), stat = "identity") +
  geom_text(aes(y = achievement, label = achievement), vjust = 0) +
  coord_cartesian(ylim = c(0.6, 1)) +
  theme_minimal()

## -----------------------------------------------------------------------------
# add individual ID
coa$ID <- as.numeric(row.names(coa))
# wide format to long format
coa_long <- pivot_longer(coa,
             cols = starts_with("objective"),
             values_to = "achievement",
             names_to = "objective",
             names_prefix = "objective")
# plot individual achievement around the overall mean for each objective
ggplot(coa_long , aes(ID, achievement, color = objective)) +
  geom_point(size = 2) +
  geom_hline(aes(yintercept = achievement, color = objective), linewidth = 2, data = coa_overall) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ objective) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal()
# visualize the distribution of achievements for each objective
ggplot(coa_long, aes(achievement, fill =  objective)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ objective) +
  theme_minimal()

