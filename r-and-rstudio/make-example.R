set.seed(0)
n <- 10
cases <- rnorm(n, log2(512), 0.25)
controls <- rnorm(n, log2(512), 0.25)
cases <- 2^(cases)
controls <- 2^(controls)
cases[1:2] <- 999
cases <- round(cases)
controls <- round(controls)
dat <- data.frame(type = factor(rep(c("controls", "cases"), each = n), levels = c("controls", "cases")),
                  measurement = c(controls, cases))
summary(lm(measurement ~ type, data=dat))

write.csv(dat, file = "data/example-data.csv", quote = FALSE, row.names = FALSE)

library(dplyr)
library(ggthemes)
library(ggplot2)
p <- dat %>% group_by(type) %>% summarize(average = mean(measurement), 
                                       se=sd(measurement)/sqrt(n())) %>%
  ggplot(aes(type, average)) + theme_excel() + 
  geom_errorbar(aes(ymin = average - 2*se, ymax = average+2*se), width = 0.25)+
  geom_bar(stat = "identity", width=0.5, fill=4, col = 1) +
  annotate(geom="text", x="cases", y=790, label = "*", cex = 15) +
  xlab("") + ylab("measurement")

ggsave(p, file = "figs/dynamite-plot.pdf", device = "pdf", height = 6, width = 8)


set.seed(1)
n <- 10
for(i in 1:25){
  cases <- rnorm(n, log2(512), 0.25)
  controls <- rnorm(n, log2(512), 0.25)
  cases <- 2^(cases)
  controls <- 2^(controls)
  cases <- round(cases)
  controls <- round(controls)
  dat <- data.frame(type = factor(rep(c("controls", "cases"), each = n), levels = c("controls", "cases")),
                    measurement = c(controls, cases))
  write.csv(dat, file = paste0("data/data-",i,".csv"), quote = FALSE, row.names = FALSE)
}


