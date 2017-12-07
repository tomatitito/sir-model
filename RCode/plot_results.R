library(ggplot2)


plot_results = function(path){
  dat <- read.csv(path,colClasses=c("numeric", "numeric", "factor"),header=FALSE)

  ## dat needs three columns:
  ## week, cases, simulation-index
  names(dat) <- c("week", "cases", "sim_id")

  p <- ggplot(dat, aes(week, cases)) +
  #geom_line(aes(group=sim_id),alpha=1/10) +
    geom_point(alpha=1/10) + geom_smooth(aes(group=1))
  ggsave(file="season.pdf")
}

plot_results("~/sir-model/data/testdat.csv")
