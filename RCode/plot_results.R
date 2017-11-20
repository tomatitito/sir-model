library(ggplot2)


plot_results = function(path){
  dat <- read.csv(path,colClasses=c("factor", "numeric", "factor"),header=FALSE)

  ## dat needs three columns:
  ## week, cases, simulation-index
  names(dat) <- c("week", "cases", "sim_id")

  p <- ggplot(dat, aes(week, cases, color=sim_id)) + geom_line() + geom_point()
  ggsave(file="season.pdf")
}

plot_results("~/sir-model/data/seasons.csv")
