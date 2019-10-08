## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
library(tidyverse)
library(dslabs)
ds_theme_set()

## ----echo=FALSE----------------------------------------------------------
n <- 1
tmp <- data.frame(outcome=rep("?",n), 
                  feature_1 = paste0("$X_1$"),
                  feature_2 = paste0("$X_2$"),
                  feature_3 = paste0("$X_3$"),
                  feature_4 = paste0("$X_4$"),
                  feature_5 = paste0("$X_5$"))
tmp %>% knitr::kable(align="c")

## ---- echo=FALSE---------------------------------------------------------
n <- 6
tmp <- data.frame(outcome = paste0("$y_", 1:n,"$"), 
                  feature_1 = paste0("$x_{",1:n,",1}$"),
                  feature_2 = paste0("$x_{",1:n,",2}$"),
                  feature_3 = paste0("$x_{",1:n,",3}$"),
                  feature_4 = paste0("$x_{",1:n,",4}$"),
                  feature_5 = paste0("$x_{",1:n,",5}$"))
tmp %>% knitr::kable()

## ---- echo=FALSE, out.width="75%"----------------------------------------
knitr::include_graphics("https://rafalab.github.io/dsbook/img//how-to-write-a-address-on-an-envelope-how-to-write-the-address-on-an-envelope-write-address-on-envelope-india-finishedenvelope-x69070.png")

## ---- echo=FALSE, cache=TRUE---------------------------------------------
mnist <- read_mnist()
tmp <- lapply( c(1,4,5), function(i){
    expand.grid(Row=1:28, Column=1:28) %>%  
      mutate(id=i, label=mnist$train$label[i],  
             value = unlist(mnist$train$images[i,])) 
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) + 
    geom_raster() + 
    scale_y_reverse() +
    scale_fill_gradient(low="white", high="black") +
    facet_grid(.~label)

## ---- echo=FALSE---------------------------------------------------------
tmp %>% ggplot(aes(Row, Column, fill=value)) + 
    geom_point(pch=21) + 
    scale_y_reverse() +
    scale_fill_gradient(low="white", high="black") +
    facet_grid(.~label)

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(tidyverse)
library(dslabs)
ds_theme_set()

## ---- message=FALSE, warning=FALSE---------------------------------------
library(caret)
library(dslabs)
data(heights)

## ------------------------------------------------------------------------
y <- heights$sex
x <- heights$height

## ------------------------------------------------------------------------
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

## ------------------------------------------------------------------------
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

## ------------------------------------------------------------------------
mean(y_hat == test_set$sex)

## ------------------------------------------------------------------------
y_hat <- ifelse(x > 62, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))

## ------------------------------------------------------------------------
mean(y == y_hat)

## ------------------------------------------------------------------------
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

## ----accuracy-v-cutoff, echo=FALSE---------------------------------------
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 

## ------------------------------------------------------------------------
max(accuracy)

## ------------------------------------------------------------------------
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

## ------------------------------------------------------------------------
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

## ------------------------------------------------------------------------
table(predicted = y_hat, actual = test_set$sex)

## ------------------------------------------------------------------------
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

## ------------------------------------------------------------------------
prev <- mean(y == "Male")
prev

## ---- echo=FALSE---------------------------------------------------------
mat <- matrix(c("True positives (TP)", "False negatives (FN)", 
                "False positives (FP)", "True negatives (TN)"), 2, 2)
colnames(mat) <- c("Actually Positive", "Actually Negative")
rownames(mat) <- c("Predicted positve", "Predicted negative")
as.data.frame(mat) %>% knitr::kable()

## ------------------------------------------------------------------------
confusionMatrix(data = y_hat, reference = test_set$sex)

## ------------------------------------------------------------------------
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, 
                prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

## ---- echo=FALSE---------------------------------------------------------
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), 
                  length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., 
                   xlab = "1 - Specificity", 
                   ylab = "Sensitivity")

## ------------------------------------------------------------------------
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
   list(method = "Height cutoff",
        FPR = 1-specificity(y_hat, test_set$sex),
        TPR = sensitivity(y_hat, test_set$sex))
})

## ---- echo=FALSE---------------------------------------------------------
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

## ----echo=FALSE----------------------------------------------------------
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
   list(method = "Height cutoff",
        cutoff = x, 
        FPR = 1-specificity(y_hat, test_set$sex),
        TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text(nudge_y = 0.01)

## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
cutoffs <- c(50, seq(55, 75), 80)
probs <- seq(0.05, .95, length.out = 50)
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
    recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
    precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
    precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

## ------------------------------------------------------------------------
data("mnist_27")

## ---- echo=FALSE---------------------------------------------------------
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

## ---- echo=FALSE---------------------------------------------------------

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
    expand.grid(Row=1:28, Column=1:28) %>%  
      mutate(label=titles[i],  
             value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) + 
    geom_raster() + 
    scale_y_reverse() +
    scale_fill_gradient(low="white", high="black") +
    facet_grid(.~label) + 
    geom_vline(xintercept = 14.5) +
    geom_hline(yintercept = 14.5)

## ---- echo=FALSE---------------------------------------------------------
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
    expand.grid(Row=1:28, Column=1:28) %>%  
      mutate(label=titles[i],  
             value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) + 
    geom_raster() + 
    scale_y_reverse() +
    scale_fill_gradient(low="white", high="black") +
    facet_grid(.~label) + 
    geom_vline(xintercept = 14.5) +
    geom_hline(yintercept = 14.5)

## ------------------------------------------------------------------------
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")

## ------------------------------------------------------------------------
p_hat <- predict(fit_glm, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(data = y_hat, reference = mnist_27$test$y)

## ---- echo=FALSE---------------------------------------------------------
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black")

## ---- echo=FALSE---------------------------------------------------------
p_hat <- predict(fit_glm, newdata = mnist_27$true_p, type = "response")
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

## ---- echo=FALSE---------------------------------------------------------
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 

## ---- eval=FALSE---------------------------------------------------------
## knn_fit <- knn3(y ~ ., data = mnist_27$train)

## ------------------------------------------------------------------------
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

## ------------------------------------------------------------------------
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

## ---- echo=FALSE---------------------------------------------------------
# We use this function to plot the estimated conditional probabilities
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster(show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black")
}

## ----knn-fit, echo=FALSE, message=FALSE, warning=FALSE-------------------
p1 <- plot_cond_prob() + ggtitle("True conditional probability")

p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("knn-5 estimate")
library(gridExtra)

grid.arrange(p1, p2, nrow=1)

## ------------------------------------------------------------------------
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, 
                reference = mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

## ------------------------------------------------------------------------
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, 
                reference=mnist_27$train$y)$overall["Accuracy"]

## ------------------------------------------------------------------------
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall["Accuracy"]

## ----knn-1-overfit, echo=FALSE-------------------------------------------
p1 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$train, aes(x_1, x_2, color= y), pch=21) +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Train set")

p2 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Test set")

grid.arrange(p1, p2, nrow=1)

## ------------------------------------------------------------------------
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

## ---- echo=FALSE---------------------------------------------------------
p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
  ggtitle("Logistic regression")

p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("knn-401")
  
grid.arrange(p1, p2, nrow=1)

## ---- warning=FALSE, message=FALSE---------------------------------------
library(purrr)
ks <- seq(3, 151, 2)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)

  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]

  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]

  tibble(train = train_error, test = test_error)
})

## ----accuracy-vs-k-knn, echo=FALSE---------------------------------------
accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, color = set)) +
  geom_line() +
  geom_point()

## ------------------------------------------------------------------------
ks[which.max(accuracy$test)]
max(accuracy$test)

## ---- include=FALSE------------------------------------------------------
if(knitr::is_html_output()){
  knitr::opts_chunk$set(out.width = "500px", out.extra='style="display: block; margin-left: auto; margin-right: auto; background-color: #000; padding:3px;"')
  } else{
  knitr::opts_chunk$set(out.width = "5in")
}

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("img//cv-1.png")

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("img//cv-2.png")

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("img//cv-3.png")

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("img//cv-4.png")

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("img//cv-5.png")

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("img//cv-6.png")

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("img//cv-7.png")

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("img//cv-8.png")

## ------------------------------------------------------------------------
ks <- seq(3, 251, 2)

## ------------------------------------------------------------------------
train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

## ------------------------------------------------------------------------
train_knn$bestTune

## ------------------------------------------------------------------------
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

## ----best-knn-fit, echo=FALSE--------------------------------------------
p1 <- plot_cond_prob() + ggtitle("True conditional probability")

p2 <- plot_cond_prob(predict(train_knn, newdata = mnist_27$true_p, type = "prob")[,2]) +
  ggtitle("kNN")

grid.arrange(p2, p1, nrow=1)

