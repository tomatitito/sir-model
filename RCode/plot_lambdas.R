# Title     : TODO
# Objective : TODO
# Created by: dusty
# Created on: 10.06.18
library(tidyr)
library(ggplot2)

args <- commandArgs(TRUE)

posterior_path <- args[1]
# prior_path <- args[2]

posterior <- read.csv(posterior_path, header=T)
prior <- data.frame(prior_lambda_1=runif(10000, min=0.4,max=0.9), prior_lambda_2=runif(10000, min=1.0,max=1.9))
all <- cbind(posterior, prior)
all_long = gather(all, key="variable")

p = ggplot(all_long, aes(value)) +
    geom_histogram(aes(y=..density..), bins=20) +
    facet_wrap(~ variable, ncol=2) +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())
ggsave(file="lambdas.pdf", plot=p)