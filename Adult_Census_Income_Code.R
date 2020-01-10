if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

col_names <- c("age","workclass","fnlwgt","education","education_num","marriage","occupation","relationship","race","sex","capital-gain","capital-loss","hours","country","income")

train_link <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
train_set <- read_csv(train_link,col_names = FALSE)
train_set <- setNames(train_set,col_names)
head(train_set)

test_link <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"
test_set <- read_csv(test_link,col_names = FALSE,skip = 1)
test_set <- setNames(test_set,col_names)
head(test_set)

#Data Pre Processing
#Removing All Unknows From The Train Set
train_set <- train_set %>% 
  filter(workclass != "?") %>%
  filter(occupation != "?") %>%
  filter(`native-country` != "?")

#Removing All Unknows From The Test Set
test_set <- test_set %>% 
  filter(workclass != "?") %>%
  filter(occupation != "?") %>%
  filter(`native-country` != "?")

#We Create Two Groups In Dataset, Lower_Than_Avg & Higher_Than_Avg
mean_hours <- mean(train_set$hours)
train_set <- train_set %>%
  mutate(work_hour_group = ifelse(hours < mean_hours,"Lower","Higher"))

test_set <- test_set %>%
  mutate(work_hour_group = ifelse(hours < mean_hours,"Lower","Higher"))

#Categorising Countries As Developed & Under Development
dc <- c("Germany","England","France","Japan","Canada","United-States","Ireland","Italy")

train_set <- train_set %>% 
  mutate(country_type = ifelse(country %in% dc,"D","UD"))

test_set <- test_set %>% 
  mutate(country_type = ifelse(country %in% dc,"D","UD"))

#Creating New Column y
train_set <- train_set %>% mutate(y = ifelse(income == "<=50K",0,1))
train_set <- train_set %>% mutate(y = factor(y))
test_set <- test_set %>% mutate(y = ifelse(income == "<=50K.",0,1))
test_set <- test_set %>% mutate(y = factor(y))

#Exploratory Data Analysis
#Total People
nrow(train_set)

#Total Countries
n_distinct(train_set$country)

#Total Male & Females
train_set %>% group_by(sex) %>% summarise(count = n()) %>% knitr::kable()

#Analysis Between Income & Sex
train_set %>% group_by(sex,income) %>% summarise(count = n()) %>% knitr::kable()
train_set %>% group_by(sex,income) %>% summarise(count = n()) %>% ggplot(aes(income,count,fill = sex)) + geom_bar(stat = "identity",position = position_dodge2()) + xlab("Sex") + ylab("Count") + theme_gdocs() + ggtitle("Income Over Sex")

#Analysis Between Income & Race
train_set %>% group_by(race,income) %>% summarise(count = n())
train_set %>% group_by(race,income) %>% summarise(count = n()) %>% ggplot(aes(income,count,fill = race)) + geom_bar(stat = "identity",position = position_dodge2()) + theme_gdocs() + xlab("Income") + ylab("Count") + ggtitle("Income Over Different Races")

#Analysis Between Income & Education
train_set %>% group_by(education,income) %>% 
  summarise(count = n()) %>% knitr::kable()

train_set %>% group_by(education,income) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(reorder(education,count),count,fill = income)) + 
  geom_bar(stat = "identity",position = position_dodge2()) + 
  coord_flip() + 
  theme_gdocs() + 
  xlab("Education") + ylab("Count") + ggtitle("Income Over Education")

#Analysis Between Income & Occupation
train_set %>% group_by(occupation,income) %>% 
  summarise(count = n()) %>% knitr::kable()

train_set %>% group_by(occupation,income) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(reorder(occupation,count),count,fill = income)) +
  geom_bar(stat = "identity",position = position_dodge2()) + 
  coord_flip() + 
  ggtitle("Income Over Different Occupations") + 
  xlab("Occupation") + ylab("Count") + theme_gdocs()

#Hours Per Week Analysis

train_set %>% group_by(work_hour_group,income) %>% summarise(count = n()) %>% knitr::kable()
train_set %>% group_by(work_hour_group,income) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(work_hour_group,count,fill = income)) + 
  geom_bar(stat = "identity",position = position_dodge2()) + 
  xlab("Work Hour Group") + 
  ylab("Count") + 
  ggtitle("Income Over Work Hour Group") + 
  theme_gdocs()

train_set %>% group_by(work_hour_group,income,sex) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(work_hour_group,count,fill = income)) + 
  geom_bar(stat = "identity",position = position_dodge2()) + 
  xlab("Work Hour Group") + 
  ylab("Count") + 
  ggtitle("Income Over Work Hour Group & Sex") + 
  theme_gdocs() + facet_grid(.~sex)

#Analysis Of Maritial Status

train_set %>% group_by(marriage,income) %>% summarise(count = n()) %>% knitr::kable()
train_set %>% group_by(marriage,income) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(`marital-status`,count,fill = income)) + 
  geom_bar(stat = "identity",position = position_dodge2()) + 
  xlab("Maritial Status") + 
  ylab("Count") + 
  ggtitle("Income Over Maritial Status") + 
  theme_gdocs() + coord_flip()

#Analysis Of Relationship

train_set %>% group_by(relationship,income) %>% summarise(count = n()) %>% knitr::kable()
train_set %>% group_by(relationship,income) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(relationship,count,fill = income)) + 
  geom_bar(stat = "identity",position = position_dodge2()) + 
  xlab("Relationship") + 
  ylab("Count") + 
  ggtitle("Income Over Relationship") + 
  theme_gdocs()

#Analysis Of Country

train_set %>% group_by(country_type,income) %>%
  summarise(count = n()) %>% 
  ggplot(aes(country_type,count,fill = income)) +
  geom_bar(stat = "identity",position = position_dodge2()) +
  ggtitle("Income Over Country Type") +
  xlab("Country Type") + 
  ylab("Count") + theme_gdocs()
  
selected_features <- c("sex","age","occupation","education","relationship","race","marriage","hours","country","y")

train_set <- train_set %>% select(selected_features)
test_set <- test_set %>% select(selected_features)



#Using Logistic Regression
control <- trainControl(method = "cv",number = 10,p = 0.9)
lga_fit <- train(y ~ .,data = train_set,method = "glm",trControl = control)
y_hat <- predict(lga_fit,test_set)

confusionMatrix(y_hat,test_set$y)
confusionMatrix(y_hat,test_set$y)$overall[1]


#Using Classifier Tree

tree_fit <- train(y ~ .,data = train_set,method = "rpart",tuneGrid = data.frame(cp = seq(0,0.05,0.002)))
ggplot(tree_fit,highlight = TRUE) + theme_gdocs()
tree_fit$bestTune

plot(tree_fit$finalModel,margin = 0.01)
text(tree_fit$finalModel,cex = 0.6)

y_hat <- predict(tree_fit,test_set)
confusionMatrix(y_hat,test_set$y)


write.csv(train_set,"E://Programming//Edx HX Data Science//Machine Learning//train_set.csv")
write.csv(test_set,"E://Programming//Edx HX Data Science//Machine Learning//test_set.csv")