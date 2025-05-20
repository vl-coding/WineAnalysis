# load necessary packages
library(tidyverse)

###INPUT DATA:

# load datasets into file
red <- read_csv("R/red.csv", show_col_types = FALSE)
white <- read_csv("R/white.csv", show_col_types = FALSE)

###PREPROCESS:

# Data Cleansing -

# combine datasets into one
data <- red %>% full_join(white)
remove(red, white)

# mutate quality to integer
data <- data %>% mutate(quality = as.integer(quality))

## Exploratory Analysis -
str(data)
summary(data)

# Categorical features -

# color
# pie chart
color_table <- data$color %>% table()
percentages <- color_table %>% prop.table() %>% round(3) * 100
txt <- paste0(names(color_table), '\n', percentages, '%')
pie(color_table, labels=txt)
remove(color_table, percentages, txt)

# quality
# histogram
quality_table <- data$quality %>% table()
percentages <- quality_table %>% prop.table() %>% round(3) * 100
txt <- paste0(percentages, '%')
# quality_table %>% as.data.frame() %>% ggplot(aes(., Freq, fill = "white")) + 
#   geom_col(show.legend = FALSE) + geom_text(aes(label = txt), show.legend = FALSE) + 
#   labs(x = "Quality Level", y = NULL)
# ... with color
quality_color_table <- data[, c("quality", "color")] %>% table()
percentages <- quality_color_table %>% prop.table() %>% round(5) * 100
txt <- paste0(txt, '\nRed: ', percentages[1:7], '%\nWhite:', percentages[8:14], '%')
plot <- quality_table %>% as.data.frame() %>% ggplot(aes(., Freq, fill = "blue")) + 
  geom_col(show.legend = FALSE) + 
  labs(x = "Quality Level", y = NULL)
plot <- plot + geom_text(aes(label = txt), show.legend = FALSE)
layer <- data[, c("quality", "color")] %>% as.data.frame() %>% filter(color == "White") %>% 
  select(-2) %>% table()
layer <- geom_col(data = as.data.frame(layer), mapping = aes(quality, Freq, fill = "white"), 
                  show.legend = FALSE)
plot <- plot + layer + geom_text(aes(label = txt), show.legend = FALSE)
plot
remove(quality_table, percentages, txt, quality_color_table, plot, layer)

# BALANCE OUT DATA -- mutate quality
data <- data %>% mutate(quality = ifelse(quality == 6, "nicest can buy", 
                                         ifelse(quality < 6, "not good enough", "too expensive")))
str(data)

# quality
# histogram
quality_table <- data$quality %>% table()
percentages <- quality_table %>% prop.table() %>% round(3) * 100
txt <- paste0(percentages, '%')
# ... with color
quality_color_table <- data[, c("quality", "color")] %>% table()
percentages <- quality_color_table %>% prop.table() %>% round(5) * 100
txt <- paste0(txt, '\nRed: ', percentages[1:3], '%\nWhite:', percentages[4:6], '%')
plot <- quality_table %>% as.data.frame() %>% ggplot(aes(., Freq, fill = "blue")) + 
  geom_col(show.legend = FALSE) + 
  labs(x = "Quality Level", y = NULL)
plot <- plot + geom_text(aes(label = txt), show.legend = FALSE)
layer <- data[, c("quality", "color")] %>% as.data.frame() %>% filter(color == "White") %>% 
  select(-2) %>% table()
layer <- geom_col(data = as.data.frame(layer), mapping = aes(quality, Freq, fill = "white"), 
                  show.legend = FALSE)
plot <- plot + layer + geom_text(aes(label = txt), show.legend = FALSE)
plot
remove(quality_table, percentages, txt, quality_color_table, plot, layer)

# Numeric features - 

# NORMALIZE NUMERIC FEATURES
data2 <- data
remove(data)
for ( i in 1:11 ) {
  for (j in 1:length(row_number(data2))) {
    data2[j,i] <- ((data2[j,i] - min(data2[, i]))/(max(data2[, i])-min(data2[, i])))
  }
}
remove(i, j)
summary(data2)

# correlation strength heatmap
# all numeric features
sub <- data2[, -(12:13)]
n_of <- length(sub)
sub <- as.data.frame(round(cor(sub), 3))
names <- as.vector(colnames(sub))
corr_table <- data.frame(x = character(25), y = character(25), value = numeric(25))
for(i in 1:(n_of^2)) {
  corr_table[i, 1] <- names[ ceiling(i / n_of) ]
  corr_table[i, 2] <- names[ (i - 1) %% n_of + 1 ]
  corr_table[i, 3] <- sub[ ceiling(i / n_of), (i - 1) %% n_of + 1 ]
}
corr_table %>% ggplot(aes(x, y, fill = abs(value))) + geom_tile() + 
  geom_text(aes(label = value, color = "red"), show.legend = FALSE) + 
  labs(title = "The Correlation Heatmap of the Numeric Variables", x = NULL, y = NULL, 
       fill = "correlation\nstrength") + scale_fill_gradient(low = "white", high = "blue")
remove(n_of, sub, names, corr_table, i)

# numeric features with strongest correlations
# scatter plot matrix
pairs(data2[, c("total sulfur dioxide", "citric acid", "free sulfur dioxide", "density", 
                "residual sugar", "fixed acidity")])

### DATA ALGORITHMS:
model_data <- data2 %>% mutate(quality = as.factor(quality), color = as.factor(color)) %>% 
                    rename(fixedAcidity = `fixed acidity`, volatileAcidity = `volatile acidity`,
                               citricAcid = `citric acid`, residualSugar = `residual sugar`,
                           freeSulfurDioxide = `free sulfur dioxide`, 
                           totalSulfurDioxide = `total sulfur dioxide`)
# split into training and testing data
set.seed(1234)
ind <- sample(2, nrow(model_data), replace = TRUE, prob = c(0.7, 0.3))
train.data <- model_data[ind == 1, ]
test.data <- model_data[ind == 2, ]
remove(ind)

## Classification

## decision tree (ctree)
library(party)
myFormula <- quality ~ .
wine.ctree <- ctree(myFormula, data = train.data)
# check the prediction
table(predict(wine.ctree), train.data$quality)
# plot ctree
plot(wine.ctree, type = "simple")
# print ctree
print(wine.ctree)
# predict on test data
winePred <- predict(wine.ctree, newdata = test.data)
table(winePred, test.data$quality)
remove(wine.ctree)

# decision tree (rpart)
library(rpart)
library(rpart.plot)
myFormula <- quality ~ .
wine.rpart <- rpart(myFormula, data = train.data, control = rpart.control(minsplit = 10))
print(wine.rpart$cptable)
rpart.plot(wine.rpart)
# select the tree with the minimum prediction error
opt <- which.min(wine.rpart$cptable[, "xerror"])
cp <- wine.rpart$cptable[opt, "CP"]
# prune tree
wine.prune <- prune(wine.rpart, cp = cp)
# plot tree
rpart.plot(wine.prune)
# make prediction
pred <- predict(wine.prune, newdata = test.data)
winePred <- c()
for ( i in 1:length(pred[, 1]) )
  for (j in 1:length(pred[1, ]) )
    if( max(pred[i, 1], pred[i, 2], pred[i, 3]) == pred[i, j] )
    {      winePred[i] <- colnames(pred)[j]    }
table(winePred, test.data$quality)
remove(i, j)
xlim <- length(model_data)
# evaluate prediction
plot(quality ~ ., data = test.data, xlab = "Observed",
     ylab = "Prediction", ylim = xlim, xlim = xlim)
abline(a = 0, b = 1)
remove(wine.prune, wine.rpart, cp, opt, pred, xlim)

# random forest
library(randomForest)
myFormula <- quality ~ .
rf <- randomForest(myFormula, data=train.data, ntree=100,
                   proximity=T)
table(predict(rf), train.data$quality)
# error rate of random forest model
plot(rf, main = "")
# variable importance
importance(rf)
varImpPlot(rf)
# margin of predictions
winePred <- predict(rf, newdata = test.data)
table(winePred, test.data$quality)
plot(margin(rf, test.data$quality))
remove(rf, myFormula, winePred)
remove(test.data, train.data)

## Clustering

# k-Means (pam)
library(cluster)
set.seed(1234)
model_data <- model_data %>% select(-12)
# determine best k value via Sum of Squared Error
sum(pam(model_data, 3)$clusinfo[, 1]^2) # k = 3
sum(pam(model_data, 5)$clusinfo[, 1]^2) # k = 5
sum(pam(model_data, 7)$clusinfo[, 1]^2) # k = 7
sum(pam(model_data, 9)$clusinfo[, 1]^2) # k = 9
sum(pam(model_data, 11)$clusinfo[, 1]^2) # k = 11
sum(pam(model_data, 12)$clusinfo[, 1]^2) # k = 12 - *best*
sum(pam(model_data, 13)$clusinfo[, 1]^2) # k = 13
# plot *best* k-Means model
pam.result <- pam(model_data, 12)
table(pam(model_data, 12)$clustering, data2$quality)
plot(pam.result)
remove(pam.result)

# hierarchical clustering
set.seed(1234)
idx <- sample(1:dim(data2)[1], 50)
model_data <- data2[idx, ]
model_data$quality <- NULL
model_data$color <- NULL
hc <- hclust(dist(model_data), method = "complete")
# plot hierarchical clustering model
plot(hc, hang = -1, labels = data2$quality[idx])
rect.hclust(hc, k = 3)
groups <- cutree(hc, k = 3)
table(groups, data2$quality[idx])
# evaluate
plot(silhouette(groups, dist(model_data)))
remove(idx, hc, groups)

# density-based clustering
library(fpc)
set.seed(1234)
model_data <- data2[-c(12, 13)]
# determine best eps
dist_matrix <- dist(model_data)
k <- 7
sorted_distances <- apply(as.matrix(dist_matrix), 1, function(x) sort(x)[k])
plot(sorted_distances, type = "l", main = "k-NN Distance Plot", xlab = "Points", ylab = "kNN Distance")
abline(h = 0.13, col = "red")
remove(k, dist_matrix, sorted_distances)
# plot model
ds <- dbscan(model_data, eps = 0.13, MinPts = 5)
table(ds$cluster, data2$quality)
plot(ds, model_data)
remove(ds)


## Association Analysis

# apriori
library(arules)
rules.all <- data2 %>% apriori()
rules.all %>% length() # number of rules discovered
rules.quality <- data2 %>% apriori(
  control = list(verbose=F),
  parameter = list(minlen=2, supp=0.005, conf=0.8),
  appearance = list(rhs=c("quality=too expensive",
                          "quality=nicest can buy",
                          "quality=not good enough"),
                    default="lhs"))
quality(rules.quality) <- rules.quality %>% quality() %>% round(digits=3) ## keep three decimal places
rules.quality.sorted <- rules.quality %>% sort(by="lift") # sort rules by lift
subset.matrix <- is.subset(rules.quality.sorted, rules.quality.sorted) # find redundant rules
subset.matrix[lower.tri(subset.matrix, diag = T)] <- F
redundant <- colSums(subset.matrix) >= 1
## remove redundant rules
rules.quality.pruned <- rules.quality.sorted[!redundant]
## rule visualization - evaluation
library(arulesViz)
rules.all %>% plot()
rules.quality.pruned %>% plot()
rules.quality.pruned %>% plot(method = "grouped")
remove(rules.all, rules.quality, rules.quality.sorted, rules.quality.pruned, subset.matrix, redundant)

# neural networks model



remove(model_data, data2)


# DATA VISUALIZATIONS

# citric acid --> alcohol??
data[, c("citric acid", "alcohol", "quality")] %>% table() %>% data.frame() %>% filter(Freq != 0) %>%
  ggplot(aes(color = quality, x = citric.acid, y = alcohol, size = Freq)) + 
  geom_point(aes(alpha = 0.8)) + scale_x_discrete(breaks=seq(0,1.70,0.05)) + 
  scale_y_discrete(breaks=seq(8, 15, 0.5))

# alcohol level --> quality ??
data[, c("alcohol", "quality")] %>% mutate(alcohol = floor(alcohol)) %>% table() %>% 
  data.frame() %>% ggplot(aes(fill = quality, x = alcohol, y = Freq)) + 
  geom_bar(position = "stack", stat = "identity")

# quality
# pie chart
quality_table <- data$quality %>% table()
percentages <- quality_table %>% prop.table() %>% round(3) * 100
txt <- paste0(names(quality_table), '\n', percentages, '%')
pie(quality_table, labels=txt)
remove(quality_table, percentages, txt)

remove(data)
