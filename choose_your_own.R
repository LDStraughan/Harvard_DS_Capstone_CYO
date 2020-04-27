########
# Set-Up
########
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(data.table)) install.packages("data.table")
if(!require(dplyr)) install.packages("dplyr")
if(!require(randomForest)) install.packages("randomForest")
if(!require(Rborist)) install.packages("Rborist")
if(!require(gam)) install.packages("gam")

################
# Data Wrangling
################
# Download data
download.file("https://raw.githubusercontent.com/LDStraughan/Harvard_DS_Capstone_CYO/master/divorce.csv", "divorce.csv")

# Check data
file.exists("divorce.csv")
read_lines("divorce.csv", n_max = 3)

# Clean/Format data
divorce <- read_delim("divorce.csv", ";")
head(divorce)

# Assign attributes to object
rownames <- c("1. If one of us apologizes when our discussion deteriorates, the discussion ends.",
              "2. I know we can ignore our differences, even if things get hard sometimes.",
              "3. When we need it, we can take our discussions with my spouse from the beginning and correct it.",
              "4. When I discuss with my spouse, to contact him will eventually work.",
              "5. The time I spent with my wife is special for us.",
              "6. We don't have time at home as partners.",
              "7. We are like two strangers who share the same environment at home rather than family.",
              "8. I enjoy our holidays with my wife.",
              "9. I enjoy traveling with my wife.",
              "10. Most of our goals are common to my spouse.",
              "11. I think that one day in the future, when I look back, I see that my spouse and I have been in harmony with each other.",
              "12. My spouse and I have similar values in terms of personal freedom.",
              "13. My spouse and I have similar sense of entertainment.",
              "14. Most of our goals for people (children, friends, etc.) are the same.",
              "15. Our dreams with my spouse are similar and harmonious.",
              "16. We're compatible with my spouse about what love should be.",
              "17. We share the same views about being happy in our life with my spouse",
              "18. My spouse and I have similar ideas about how marriage should be",
              "19. My spouse and I have similar ideas about how roles should be in marriage",
              "20. My spouse and I have similar values in trust.",
              "21. I know exactly what my wife likes.",
              "22. I know how my spouse wants to be taken care of when she/he sick.",
              "23. I know my spouse's favorite food.",
              "24. I can tell you what kind of stress my spouse is facing in her/his life.",
              "25. I have knowledge of my spouse's inner world.",
              "26. I know my spouse's basic anxieties.",
              "27. I know what my spouse's current sources of stress are.",
              "28. I know my spouse's hopes and wishes.",
              "29. I know my spouse very well.",
              "30. I know my spouse's friends and their social relationships.",
              "31. I feel aggressive when I argue with my spouse.",
              "32. When discussing with my spouse, I usually use expressions such as ‘you always’ or ‘you never’ .",
              "33. I can use negative statements about my spouse's personality during our discussions.",
              "34. I can use offensive expressions during our discussions.",
              "35. I can insult my spouse during our discussions.",
              "36. I can be humiliating when we discussions.",
              "37. My discussion with my spouse is not calm.",
              "38. I hate my spouse's way of open a subject.",
              "39. Our discussions often occur suddenly.",
              "40. We're just starting a discussion before I know what's going on.",
              "41. When I talk to my spouse about something, my calm suddenly breaks.",
              "42. When I argue with my spouse, ı only go out and I don't say a word.",
              "43. I mostly stay silent to calm the environment a little bit.",
              "44. Sometimes I think it's good for me to leave home for a while.",
              "45. I'd rather stay silent than discuss with my spouse.",
              "46. Even if I'm right in the discussion, I stay silent to hurt my spouse.",
              "47. When I discuss with my spouse, I stay silent because I am afraid of not being able to control my anger.",
              "48. I feel right in our discussions.",
              "49. I have nothing to do with what I've been accused of.",
              "50. I'm not actually the one who's guilty about what I'm accused of.",
              "51. I'm not the one who's wrong about problems at home.",
              "52. I wouldn't hesitate to tell my spouse about her/his inadequacy.",
              "53. When I discuss, I remind my spouse of her/his inadequacy.",
              "54. I'm not afraid to tell my spouse about her/his incompetence.")

# Convert data to data frame
divorce <- data.frame(divorce)
divorce

# Separate divorce data into dat & validation set
y <- divorce$Class
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times=1, p=0.9, list=FALSE)
dat <- divorce[test_index,]
validation <- divorce[-test_index,]

###############
# Data Analysis
###############
# Check divorce
dim(divorce)
# 170 couples
# 55 = 54 Attributes + Class Column

# Check dat
dim(dat)
# 153 couples
# 55 = 54 Attributes + Class Column

# Separate dat into training and testing sets
y <- dat$Class
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times=1, p=0.6, list=FALSE) # There are only 153 couples so, to avoid overfitting, we assign 60% to the training data and 40% to the test set.
train <- dat[test_index,]
test <- dat[-test_index,]

# Check train
dim(train)
# 92 Couples
# 55 = 54 Attributes + Class Column

# Check test
dim(test)
# 61 Couples
# 55 = 54 Attributes + Class Column

## Accuracies
func <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'1','0')
    mean(y_hat==train$Class)
  })
}
predictions <- apply(train,2,func)
acc <- sapply(predictions,max)
max_acc <- order(acc, decreasing = TRUE)[2] # Naturally, Class has a 100% accuracy so we want the next best thing
predictions <- func(train[,max_acc])
rangedValues <- seq(range(train[,max_acc])[1],range(train[,max_acc])[2],by=1)
cutoffs <-rangedValues[which(predictions==max(predictions))]
cutoffs

y_hat <- ifelse(test[,max_acc]>cutoffs[1],'1','0')
mean(y_hat==test$Class)

func <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'1','0')
    mean(y_hat==test$Class)
  })
}
predictions <- apply(test,2,func)
accuracies <- sapply(predictions,max)[1:54]	# Only include Attributes, not Class
accuracies

# Mean Accuracy
mean(accuracies) # 0.900425 - already meets 90% objective

### Top Accuracies
tail(sort(accuracies), 10)   

### Highest Accuracy
tail(sort(accuracies), 4) # Atr17, 26, 39 & 40 = 0.9672131 (Only 92 couples in training set & seed need to be taken into account)

### Table
acc_tibble <- accuracies %>% as.tibble
row.names(acc_tibble) <- rownames
acc_tibble <- tibble::rownames_to_column(acc_tibble, var = "Attributes")
acc_tibble %>% arrange(desc(value))

###########
# Modelling
###########
# Ensure the data is a factor
train <- droplevels(train)
train$Class <- factor(train$Class)
class(train$Class)

# Create ensemble model
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "rf", "adaboost")

# Model - may take some time
fits <- lapply(models, function(model){ 
  print(model)
  train(Class ~ ., method = model, data = train)
})

## Test
names(fits) <- models # Name all the fits

# Prediction function
pred <- sapply(fits, function(object) 
  predict(object, newdata = test))

# Check accuracy
acc <- colMeans(pred == test$Class)
acc
mean(acc) # 0.9508197

# Check results
votes <- rowMeans(pred == "1")
y_hat <- ifelse(votes > 0.5, "1", "0")
mean(y_hat == test$Class) # 0.9508197

## Final Modelling (on larger dat set)
# Ensure the data is a factor
dat <- droplevels(dat)
dat$Class <- factor(dat$Class) 
class(dat$Class)

# Ensemble model remains the same
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "rf", "adaboost")

# Model - may take some time
fits <- lapply(models, function(model){ 
  print(model)
  train(Class ~ ., method = model, data = dat)
}) 

#########
# Results
#########
## Final Test (on validation set)
names(fits) <- models # Name all the fits

# Final prediction function
final_pred <- sapply(fits, function(object) 
  predict(object, newdata = validation))

# Check final accuracy
final_acc <- colMeans(final_pred == validation$Class)
final_acc
mean(final_acc) # 1

# Check final results
final_votes <- rowMeans(final_pred == "1")
final_y_hat <- ifelse(final_votes > 0.5, "1", "0")
mean(final_y_hat == validation$Class) # 1

# Ensure all predictions are correct
getmode <- function(v) { # Create mode function - prediction will be mode of ensemble results
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- as.numeric(apply(final_pred, 1, getmode)) # Most common result
identical(result, validation$Class) # 100% accuracy

# Bibliography
Yöntem, M.K., Adem, K., İlhan, T. and Kılıçarslan, S., 2019. Divorce prediction using correlation based feature selection and artificial neural networks. $Nevşehir Hacı Bektaş Veli Üniversitesi SBE Dergisi$, 9(1), pp.259-273. Available from: <https://dergipark.org.tr/tr/download/article-file/748448> [Cited 10 April 2020]