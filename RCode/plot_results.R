library(ggplot2)


plot_results = function(path){
  dat <- read.csv(path,colClasses=c("factor", "numeric", "factor"),header=FALSE)

  ## dat needs three columns:
  ## week, cases, simulation-index
  names(dat) <- c("week", "cases", "sim_id")

  p <- ggplot(dat, aes(week, cases)) + geom_line(aes(group=sim_id),alpha=1/10) 
  ggsave(file="season.pdf")
}

plot_results("~/sir-model/data/seasons.csv")
