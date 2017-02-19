plot_results = function(path){
  library(ggplot2)
  dat = read.csv(path,header=FALSE)
  names(dat) = "Param"
  p = ggplot(dat, aes(Param)) + geom_histogram()
  ggsave(file="results.pdf")
}

plot_results("data/results.dat")
