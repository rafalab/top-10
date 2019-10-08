dat <- read.csv("data/example-data.csv")

t.test(measurement ~ type, data = dat)

boxplot(measurement ~ type, data = dat)
points(measurement ~ type, data = dat)

points(measurement ~ jitter(as.numeric(type)), data = dat)

dat <- dat[dat$measurement!=999, ]

t.test(measurement ~ type, data = dat)

