
dataset = read.csv('Salary_Data.csv')


library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# feature scaling
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])
# regressor = lm(formula = Salary ~ YearsExperience, data= training_set)
# y_pred = predict(regressor, new_data= testing_set)

regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

y_pred = predict(regressor, newdata = testing_set)

# visualize
# install.packages('ggplot2')
library(ggplot2)

ggplot() +
  geom_point(aes(x= training_set$YearsExperience,y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x= training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Experience vs Salary') +
  xlab('Experience') + 
  ylab('Salary')

ggplot() +
  geom_point(aes(x= testing_set$YearsExperience,y = testing_set$Salary),
             colour = 'red') +
  geom_line(aes(x= training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Experience vs Salary[testset]') +
  xlab('Experience') + 
  ylab('Salary')