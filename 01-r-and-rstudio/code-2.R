for(i in 1:25){
  dat <- read.csv(paste0("data/data-",i,".csv"))
  print(i)
  print(t.test(measurement ~ type, data = dat))
}

results <- sapply(1:25, function(i){
  dat <- read.csv(paste0("data/data-",i,".csv"))
  t.test(measurement ~ type, data = dat)$p.value
})
