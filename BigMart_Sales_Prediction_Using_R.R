# install.packages('data.table')
#libary(data.table)

train = fread('Train_UWu5bXk.csv')
test = fread('Test_u94Q5KV.csv')
submission = fread('SampleSubmission_TmnO39y.csv')

test[,Item_Outlet_Sales := NA] 
combi = rbind(train,test) #combine train and test sets


# EDA

# Dependent variable Item Outlet Sales
#Numerical variables
ggplot(train)+
  geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill ='green') +
  xlab('Item Outlet Sales')

# Item weight, Item Visibility, Item MRp

p1 = ggplot(combi) + geom_histogram(aes(combi$Item_Weight), binwidth = 0.5, fill = 'blue') +
     xlab('Item Weight')

p2 = ggplot(combi) + geom_histogram(aes(combi$Item_Visibility), binwidth = 0.005, fill = 'blue') +
  xlab('Item Visibility')

p3 = ggplot(combi) + geom_histogram(aes(combi$Item_MRP), binwidth = 1, fill = 'blue') +
  xlab('Item MRP')
  
plot_grid(p1,p2,p3, nrow = 1)

# categorical variables
# Item Identifier, Item fat content, Item type
# install.packages('magrittr')
# install.packages('dplyr')

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
  geom_bar(aes(Item_Fat_Content,Count), stat = 'identity', fill = 'coral1')

combi$Item_Fat_Content[combi$Item_Fat_Content == 'LF'] = 'Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content == 'low fat'] = 'Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content == 'reg'] = 'Regular'
  
p4 = ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
  geom_bar(aes(Item_Fat_Content,Count), stat = 'identity', fill = 'coral1')+
  geom_label(aes(Item_Fat_Content,Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Item_Fat_Content')

p5 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +
  geom_bar(aes(Item_Type,Count), stat = 'identity', fill = 'coral1') +
  geom_label(aes(Item_Type,Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle= 45, hjust = 1)) + 
  ggtitle('Item Type')

p6 = ggplot(combi %>% group_by(Item_Identifier) %>% summarise(Count = n())) +
  geom_bar(aes(Item_Identifier,Count), stat = 'identity', fill = 'coral1') +
  geom_label(aes(Item_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x= element_text(angle = 45, hjust = 1))+
  ggtitle('Item Identifier')

second_row = plot_grid(p5,p6,nrow = 1)
plot_grid(p4,second_row,ncol = 1)


p7 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Identifier, Count), stat = 'identity', fill = 'coral1' ) +
  geom_label(aes(Outlet_Identifier,Count,label=Count), vjust =0.5) +
  theme(axis.text.x = element_text(angle =45, hjust =1)) +
  ggtitle('Outlet Identifier')
  
p8 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Size, Count), stat = 'identity', fill = 'coral1' ) +
  geom_label(aes(Outlet_Size,Count,label=Count), vjust =0.5) +
  theme(axis.text.x = element_text(angle =45, hjust =1)) +
  ggtitle('Outlet Size')

combi$Outlet_Size[combi$Outlet_Size == ' '] = 'Small'

p9 = ggplot(combi %>% group_by(Outlet_Location_Type) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Location_Type, Count), stat = 'identity', fill = 'coral1' ) +
  geom_label(aes(Outlet_Location_Type,Count,label=Count), vjust =0.5) +
  theme(axis.text.x = element_text(angle =45, hjust =1)) +
  ggtitle('Outlet Location Type')

p10 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Type, Count), stat = 'identity', fill = 'coral1' ) +
  geom_label(aes(Outlet_Type,Count,label=Count), vjust =0.5) +
  theme(axis.text.x = element_text(angle =45, hjust =1)) +
  ggtitle('Outlet Type')

p11 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = 'identity', fill = 'coral1' ) +
  geom_label(aes(factor(Outlet_Establishment_Year),Count,label=Count), vjust =0.5) +
  xlab("Outlet_Establishment_Year") +
  ggtitle('Outlet Establishment Year')

#extract train
train = combi[1:nrow(train)]

# Bivariate Analysis
# Numerical Independent variables

p12 = ggplot(train) + 
  geom_point(aes(Item_Weight, Item_Outlet_Sales), color = 'violet', alpha =0.3) +
  theme(axis.title = element_text(size= 8.5))

p13 = ggplot(train) + 
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), color = 'violet', alpha =0.3) +
  theme(axis.title = element_text(size= 8.5))

p14 = ggplot(train) + 
  geom_point(aes(Item_MRP, Item_Outlet_Sales), color = 'violet', alpha =0.3) +
  theme(axis.title = element_text(size= 8.5))

# categorical Independent variable

p15 = ggplot(train) +
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill='magenta') + 
  theme(axis.text.x = element_text(angle = 45, hjust =1),
        axis.text = element_text(size = 6) ,
        axis.title = element_text(size = 8.5) )

p16 = ggplot(train) +
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill='magenta') + 
  theme(axis.text.x = element_text(angle = 45, hjust =1),
        axis.text = element_text(size = 6) ,
        axis.title = element_text(size = 8.5) )

p17 = ggplot(train) +
  geom_violin(aes(Item_Identifier, Item_Outlet_Sales), fill='magenta') + 
  theme(axis.text.x = element_text(angle = 45, hjust =1),
        axis.text = element_text(size = 6) ,
        axis.title = element_text(size = 8.5) )

p18 = ggplot(train) +
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill='magenta') + 
  theme(axis.text.x = element_text(angle = 45, hjust =1),
        axis.text = element_text(size = 6) ,
        axis.title = element_text(size = 8.5) )

p19 = ggplot(train) +
  geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill='magenta') + 
  theme(axis.text.x = element_text(angle = 45, hjust =1),
        axis.text = element_text(size = 6) ,
        axis.title = element_text(size = 8.5) )

p20 = ggplot(train) +
  geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill='magenta') + 
  theme(axis.text.x = element_text(angle = 45, hjust =1),
        axis.text = element_text(size = 6) ,
        axis.title = element_text(size = 8.5) )

p21 = ggplot(train) +
  geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill='magenta') + 
  theme(axis.text.x = element_text(angle = 45, hjust =1),
        axis.text = element_text(size = 6) ,
        axis.title = element_text(size = 8.5) )

sum(is.na(combi$Item_Weight))
sum(is.na(combi$Item_Visibility))
sum(is.na(combi$Item_MRP))
sum(is.na(combi$Item_Identifier))
sum(is.na(combi$Item_Fat_Content))
sum(is.na(combi$Item_Type))
sum(is.na(combi$Outlet_Identifier))
sum(is.na(combi$Outlet_Size))
sum(is.na(combi$Outlet_Location_Type))
sum(is.na(combi$Outlet_Type))
sum(is.na(combi$Outlet_Establishment_Year))
sum(is.na(combi$Item_Outlet_Sales))
#outlet type assigning small to missing entry
missing_index = which(combi$Outlet_Size == '')

for (i in missing_index){
  
combi$Outlet_Size[i] = 'Small'

}

missing_index = which(is.na(combi$Item_Weight))

for (i in missing_index){
  item = combi$Item_Identifier[i]
  
  item_index = combi$Item_Identifier == item
  combi$Item_Weight[i] = mean(combi$Item_Weight[item_index], na.rm = T)
}

sum(is.na(combi$Item_Weight))

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)
# replace 0 in item visibility with mean of that
zero_index = which(combi$Item_Visibility == 0)
for (i in zero_index){
  item = combi$Item_Identifier[i]
  
  item_index = combi$Item_Identifier == item
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[item_index], na.rm = T)
}
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

# feature engineering
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

combi[,Item_New_Type := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable","not_sure"))]

combi[,Item_Category := substr(combi$Item_Identifier,1,2)]

combi$Item_Fat_Content[combi$Item_Category == 'NC'] = 'Non-Edible'

combi[,Outlet_Years := 2013 - combi$Outlet_Establishment_Year]

combi[,price_per_unit_wt := combi$Item_MRP/combi$Item_Weight]

combi[,Item_MRP := ifelse(Item_MRP < 69, '1st', ifelse(Item_MRP >= 69 & Item_MRP < 136, '2nd',ifelse(Item_MRP >=136 & Item_MRP < 203, '3rd','4th')))]

# Label encoding for categorical variables
combi[,Outlet_Size_num := ifelse(Outlet_Size == 'Small',0, ifelse(Outlet_Size == 'Medium',1,2))]

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == 'Tier 3',0, ifelse(Outlet_Location_Type == 'Tier 2', 1,2))]

combi[,c("Outlet_Size","Outlet_Location_Type") := NULL]

# One hot encoding for categorcial variables
# install.packages('caret')
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)

# data transformation 
ggplot(combi) + geom_histogram(aes(combi$price_per_unit_wt), binwidth = 0.005, fill = 'blue') +
  xlab('combi$price_per_unit_wt')

combi[,Item_Visibility := log(Item_Visibility + 1)]
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]

# scaling numeric predictors

num_vars = which(sapply(combi,is.numeric))
num_vars_names = names(num_vars)
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method = c('center','scale'))
combi_numeric_norm = predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL]

combi = cbind(combi,combi_numeric_norm)

# splitting data back to train and test

train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1) : nrow(combi)]
test[,Item_Outlet_Sales := NULL]

##correlated features

cor_train = cor(train[,-c("Item_Identifier")])
# install.packages('corrplot')
corrplot(cor_train, method = "pie", type = 'lower',t1.cex = 0.9)

# Building Model

linear_reg_model = lm(Item_Outlet_Sales ~ ., data = train[,-c('Item_Identifier')])
# Predictions on test data
#Linear Regression
submission$Item_Outlet_Sales = predict(linear_reg_model, test[,-c('Item_Identifier')])
write.csv(submission, "Linear_Reg_submit.csv",row.names = F)

# Lasso Regression
set.seed(1235)
my_control = trainControl(method = 'cv', number = 10)
Grid = expand.grid(alpha=1, lambda= seq(0.001,0.1,by = 0.0002))

lasso_linear_reg_mod = train(x= train[,-c("Item_Identifier","Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                                                      method = 'glmnet', trControl= my_control,tuneGrid =Grid)

submission$Item_Outlet_Sales = predict(lasso_linear_reg_mod, test[,-c("Item_Identifier")])
write.csv(submission, "Lasso_Linear_Reg_submit.csv", row.names = F)

# Ridge Regression
set.seed(1236)
my_control = trainControl(method = 'cv', number = 10)
Grid = expand.grid(alpha=0, lambda= 0.8) #seq(0.1,0.8,by = 0.1))

Ridge_linear_reg_mod = train(x= train[,-c("Item_Identifier","Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                             method = 'glmnet', trControl= my_control,tuneGrid =Grid)

submission$Item_Outlet_Sales = predict(Ridge_linear_reg_mod, test[,-c("Item_Identifier")])
write.csv(submission, "Ridge_Linear_Reg_submit.csv", row.names = F)

# Random Forest

set.seed(1237)
my_control = trainControl(method="cv", number=5) # 5-fold CV
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)
rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], 
               y = train$Item_Outlet_Sales,
               method='ranger', 
               trControl= my_control, 
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")

submission$Item_Outlet_Sales = predict(rf_mod, test[,-c("Item_Identifier")])
write.csv(submission, "Random_Forest_submit.csv", row.names = F)

plot(rf_mod)
plot(varImp(rf_mod))

# XGBOost
param_list = list(
  objective = "reg:linear",
  eta = 0.01,
  gamma = 1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.5
)

# install.packages('xgboost')
dtrain = xgb.DMatrix(data= as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label = train$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))

set.seed(112)
xgbcv = xgb.cv(params = param_list,
               data = dtrain,
               nrounds = 1000,
               nfold = 5,
               print_every_n = 10,
               early_stopping_rounds = 30,
               maximize = F)

xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 531)

submission$Item_Outlet_Sales = predict(xgb_model, as.matrix(test[,-c("Item_Identifier")]))
write.csv(submission, "XGB_submit.csv", row.names = F)

var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier","Item_Outlet_Sales")),
                                        model = xgb_model)

xgb.plot.importance(var_imp)

var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), 
                         model = xgb_model)

xgb.plot.importance(var_imp)