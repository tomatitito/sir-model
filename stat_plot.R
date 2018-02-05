library(ggplot2)
library(purrr)
#2018-01-31T17:03:27.831_two-stage-poisson-query_1000_1_50
#dat = data.frame(v1=rnorm(10), v2=rnorm(10), v3=rnorm(10), v4=rnorm(10))
dat = read.csv("data/2018-01-25T12:44:29.119_two-stage-poisson-query_1000_1_50.csv",header=TRUE)

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

compute_stat = function(df, f){
  df %>%
    split(.$week) %>%
    map_dbl(~f(.$new)) 
}

stats = data.frame(means=compute_stat(dat, mean), medians=compute_stat(dat, median), modes=compute_stat(dat, mode))

print(head(stats))

p = ggplot(dat, aes(x=week,y=new)) + geom_point(alpha=1/100) +
  geom_line(data=stats, aes(x=1:nrow(stats),y=means, color="mean"), inherit.aes=FALSE) +
  geom_line(data=stats, aes(x=1:nrow(stats),y=medians, color="median"), inherit.aes=FALSE) +
  geom_line(data=stats, aes(x=1:nrow(stats),y=modes, color="mode"), inherit.aes=FALSE) 

ggsave(filename="stat_plot.pdf", plot=p, device="pdf")

