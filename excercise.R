x1 <- runif(30,-1, 1)
x2 <- runif(30,-1, 1)

x <- cbind(x1, x2)

Y <- ifelse(x2 > 0.5 + x1,+1,-1)

plot(
  x,
  pch = ifelse(Y > 0, "+", "-"),
  xlim = c(-1, 1),
  ylim = c(-1, 1),
  cex = 2
)

abline(0.5, 1)

#초평면으로부터의 거리를 계산할 헬퍼 함수
calculate_distance = function(x, w, b) {
  sum(x * w) + b
}

# 선형 분류기
linear_classifier = function(x, w, b) {
  distances = apply(x, 1, calculate_distance, w, b)
  return(ifelse(distances < 0,-1,+1))
}


height <-
  c(
    69.1,
    56.4,
    65.3,
    62.8,
    63,
    57.3,
    59.8,
    62.5,
    62.5,
    59.0,
    51.3,
    64,
    56.4,
    66.5,
    72.2,
    65.0,
    67,
    57.6,
    66.6
  )
weight <-
  c(113,
    84,
    99,
    103,
    102,
    83,
    85,
    113,
    84,
    99,
    51,
    90,
    77,
    112,
    150,
    128,
    133,
    85,
    112)

plot(height, weight)
cor(height, weight)

model <- lm(weight ~ height)
model

attributes(model)
model$coefficients[1]
residuals(model)
summary(model)
abline(model)

iris


library(ggvis)
iris %>% ggvis( ~ Petal.Length, ~ Petal.Width, fill = ~ factor(Species)) %>% layer_points()
<<<<<<< HEAD

# normalization function

min_max_normalizer <- function(x)
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

normalized_iris <- as.data.frame(lapply(iris[1:4], min_max_normalizer))

summary(normalized_iris)

table(iris$Species)

set.seed(1234)

# 트레이닝-테스트로 67%, 33%씩 나눈다
random_sample <- sample(2, nrow(iris), replace = TRUE, prob = c(0.67, 0.33))

# training data set
iris.training <- iris[
  random_sample ==1, 1:4]

# label of training
iris.trainLabel <- iris[
  random_sample ==1, 5
]

# test data set
iris.test <- iris[
  random_sample ==2, 1:4
]

# label of testing
iris.testLabel <- iris[
  random_sample==2, 5
]

library(class)

# KNN for k=3
iris_model <- knn(train = iris.training, test = iris.test, cl = iris.trainLabel, k=3)

iris_model

library(gmodels)

CrossTable(x = iris.testLabel, y = iris_model, prop.chisq = FALSE)
