# Title     : TODO
# Objective : TODO
# Created by: dusty
# Created on: 10.06.18
library(tidyr)
library(ggplot2)

args <- commandArgs(TRUE)

posterior_path <- args[1]
prior_path <- args[2]

posterior <- read.csv(posterior_path, header=T)
prior <- read.csv(prior_path, header=T)

all <- cbind(posterior, prior)
all_long = gather(all, key=c("lambda.1", "lambda.2", "l_1_prior", "l_2_prior"))
names(all_long) = c("variable", "value")

p = ggplot(all_long, aes(value)) +
    geom_histogram(aes(y=..density..), bins=20) +
    facet_wrap(~ variable, ncol=2)
ggsave(file="lambdas.pdf", plot=p)