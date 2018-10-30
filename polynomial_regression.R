dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# testing_set = subset(dataset, split == FALSE)

# feature scaling
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])

lin_reg = lm(formula = Salary ~ ., data= dataset)
#y_pred = predict(lin_reg, new_data= dataset)

dataset$Level2 = dataset$Level ^ 2
dataset$Level3 = dataset$Level ^ 3
dataset$Level4 = dataset$Level ^ 4
poly_reg = lm(formula = Salary ~ ., data = dataset)

#visualization
ggplot() +
  geom_point(aes(x=dataset$Level,y= dataset$Salary),colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg,newdata= dataset)), colour = 'blue') +
  ggtitle('Salary vs Level (linear rgression)')+
  xlab('Level') + 
  ylab('Salary')

ggplot() +
  geom_point(aes(x=dataset$Level,y= dataset$Salary),colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg,newdata = dataset)), colour = 'blue') +
  ggtitle('Salary vs Level (poly rgression)')+
  xlab('Level') + 
  ylab('Salary')

y_pred = predict(lin_reg,data.frame(Level = 6.5))

y_pred1 = predict(poly_reg,data.frame(Level = 6.5,
                                                Level2 = 6.5^2,
                                                Level3 = 6.5^3,
                                                Level4 = 6.5^4))