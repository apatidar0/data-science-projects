### Big market sales analysis
##The data scientists at BigMart have collected sales data for 1559 products across 10 stores in different cities for the year 2013. Now each product has certain attributes that sets it apart from other products. Same is the case with each store.

#The aim is to build a predictive model to find out the sales of each product at a particular store so that it would help the decision makers at BigMart to find out the properties of any product or store, which play a key role in increasing the overall sales.


Store Level Hypotheses

# 1. City type: Stores located in urban or Tier 1 cities should have higher sales because of the higher income levels of people there.

#2. Population Density: Stores located in densely populated areas should have higher sales because of more demand. Store Capacity: Stores which are very big in size should have higher sales as they act like one-stop-shops and people would prefer getting everything from one place

# 3. Competitors: Stores having similar establishments nearby should have less sales because of more competition.

# 4. Marketing: Stores which have a good marketing division should have higher sales as it will be able to attract customers through the right offers and advertising.

# 5. Location: Stores located within popular marketplaces should have higher sales because of better access to customers.

# 6. Ambiance: Stores which are well-maintained and managed by polite and humble people are expected to have higher footfall and thus higher sales.


Product Level Hypotheses
 
# 1. Brand: Branded products should have higher sales because of higher trust in the customer.

# 2 Packaging: Products with good packaging can attract customers and sell more.

# 3. Utility: Daily use products should have a higher tendency to sell as compared to the specific use products.

# 4. Display Area: Products which are given bigger shelves in the store are likely to catch attention first and sell more. Visibility in Store: The location of product in a store will impact sales. Ones which are right at entrance will catch the eye of customer first rather than the ones in back.

# 5. Advertising: Better advertising of products in the store will should higher sales in most cases. Promotional Offers: Products accompanied with attractive offers and discounts will sell more.

Customer Level Hypotheses

#1. Customer Behavior: Stores keeping the right set of products to meet the local needs of customers will have higher sales.

# 2.Job Profile: Customer working at executive levels would have higher chances of purchasing high amount products as compared to customers working at entry or mid senior level.

# 3. Family Size: More the number of family members, more amount will be spent by a customer to buy products

# 4. Annual Income: Higher the annual income of a customer, customer is more likely to buy high cost products. Past Purchase History: Availablity of this information can help us to determine the frequency of a product being purchased by a user.

Macro Level Hypotheses

# 1. Environment: If the environment is declared safe by government, customer would be more likely to purchase products without worrying if it's environment friendly or not.

# 2.Economic Growth: If the current economy shows a consistent growth, per capita income will rise, therefore buying power of customers will increase.

Please note that this is not an exhaustive list. You can come up with more hypotheses of your own, the more the better. Let's begin exploring the dataset and try to find interesting patterns.





library(data.table) # used for reading and manipulation of data 
library(dplyr)      # used for data manipulation and joining 
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling 
library(corrplot)   # used for making correlation plot 
library(xgboost)    # used for building XGBoost model 
library(cowplot)    # used for combining multiple plots 


# reading data
library(readr)
train <- read_csv("C:/Users/sandY/Downloads/Train_UWu5bXk.txt")
View(train)

library(readr)
test <- read_csv("C:/Users/sandY/Downloads/Test_u94Q5KV.txt")
View(test)


#Dimensions of Data

dim(train)
dim(test)
# features in data
names(train)

names(test)


# structure of data
str(train)
str(test)

# combine train and test
test["Item_Outlet_Sales"]<- NA 
combi = rbind(train, test) # combining train and test datasets dim(combi)

dim(combi)

# UNIVARIATE ANALYSIS
# Target Variable
# Since our target variable is continuous, we can visualise it by plotting its histogram.

ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +  xlab("Item_Outlet_Sales")

# As we can see, it is a right skewd variable and would need some data transformation to treat its skewness.

p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue") 
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue") 
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue") 
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package


# There seems to be no clear-cut pattern in Item_Weight.
# Item_Visibility is right-skewed and should be transformed to curb its skewness.
# We can clearly see 4 different distributions for Item_MRP. It is an interesting insight.


#     Independent Variables (categorical variables)


ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

#  in the figure above, 'LF', 'low fat', and 'Low Fat' are the same category and can be combined into one. Similarly we can be done for 'reg' and 'Regular' into one. After making these corrections we'll plot the same figure again.

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular" 
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

3  Now let's check the other categorical variables.

 #plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +   geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +  xlab("") +  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  ggtitle("Item_Type")
 #plot for Outlet_Identifier 
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# plot for Outlet_Size 
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
second_row = plot_grid(p5, p6, nrow = 1) 
plot_grid(p4, second_row, ncol = 1)




# reamining catgorical variables
# plot for Outlet_Establishment_Year 
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  xlab("Outlet_Establishment_Year") +  theme(axis.text.x = element_text(size = 8.5))
# plot for Outlet_Type 
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(size = 8.5))
plot_grid(p7, p8, ncol = 2)

# Lesser number of observations in the data for the outlets established in the year 1998 as compared to the other years.
# Supermarket Type 1 seems to be the most popular category of Outlet_Type.



# BIvariate analysis



# Target Variable vs Independent Numerical Variables

# Item_Weight vs Item_Outlet_Sales

p9 = ggplot(train) +      geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +     theme(axis.title = element_text(size = 8.5))
# Item_Visibility vs Item_Outlet_Sales 
p10 = ggplot(train) +       geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))
# Item_MRP vs Item_Outlet_Sales 
p11 = ggplot(train) +       geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))
second_row_2 = plot_grid(p10, p11, ncol = 2) 
plot_grid(p9, second_row_2, nrow = 2)

# Item_Outlet_Sales is spread well across the entire range of the Item_Weight without any obvious pattern.
 # In Item_Visibility vs Item_Outlet_Sales, there is a string of points at Item_Visibility = 0.0 which seems strange as item visibility cannot be completely zero. We will take note of this issue and deal with it in the later stages.
# In the third plot of Item_MRP vs Item_Outlet_Sales, we can clearly see 4 segments of prices that can be used in feature engineering to create a new variable.
.



# Target Variable vs Independent Categorical Variables


# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) +       geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +      theme(axis.text.x = element_text(angle = 45, hjust = 1),            axis.text = element_text(size = 6),            axis.title = element_text(size = 8.5))
# Item_Fat_Content vs Item_Outlet_Sales 
p13 = ggplot(train) +       geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +      theme(axis.text.x = element_text(angle = 45, hjust = 1),            axis.text = element_text(size = 8),            axis.title = element_text(size = 8.5))
# Outlet_Identifier vs Item_Outlet_Sales p14 = ggplot(train) +       geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +      theme(axis.text.x = element_text(angle = 45, hjust = 1),            axis.text = element_text(size = 8),            axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2) 
plot_grid(p12, second_row_3, ncol = 1)


#Imputing Missing Value

missing_index = which(is.na(combi$Item_Weight)) 
for(i in missing_index){
  item = combi$Item_Identifier[i]  
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T) }



# replacing 0
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

zero_index = which(combi$Item_Visibility == 0)

for(i in zero_index){
  item = combi$Item_Identifier[i]  
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)  }


## feature engineering
#create the following new features

# 1. Item_Type_new: Broader categories for the variable Item_Type.
# 2. Item_category: Categorical variable derived from Item_Identifier.
# 3. Outlet_Years: Years of operation for outlets.
# 4. price_per_unit_wt: Item_MRP/Item_Weight
# 5. Item_MRP_clusters: Binned feature for Item_MRP.

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
# create a new feature 'Item_Type_new' 
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]


#Let's compare Item_Type with the first 2 characters of Item_Identifier, i.e., 'DR', 'FD', and 'NC'. These identifiers most probably stand for drinks, food, and non-consumable.

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)


# Based on the above table we can create a new feature. Let's call it Item_category.

combi["Item_category"]= substr(combi$Item_Identifier, 1, 2)]

# Item_category.

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 
# years of operation for outlets 
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 
# Price per unit weight 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]


# Earlier in the Item_MRP vs Item_Outlet_Sales plot, we saw Item_MRP was spread across in 4 chunks. Now let's assign a label to each of these chunks and use this label as a new variable.

# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]



# Label encoding for the categorical variables
# We will label encode Outlet_Size and Outlet_Location_Type as these are ordinal variables.

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,                                 ifelse(Outlet_Size == "Medium", 1, 2))] 

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))] 

# removing categorical variables after label encoding 

combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]
One hot encoding for the categorical variable
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])) 
combi = cbind(combi[,"Item_Identifier"], ohe_df)



#Removing Skewness
combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]



# Scaling numeric predictors

num_vars = which(sapply(combi, is.numeric)) # index of numeric features num_vars_names = names(num_vars) combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F] prep_num = preProcess(combi_numeric, method=c("center", "scale")) combi_numeric_norm = predict(prep_num, combi_numeric)
combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables combi = cbind(combi, combi_numeric_norm)

# Splitting the combined data combi back to train and test set.


train = combi[1:nrow(train)] test = combi[(nrow(train) + 1):nrow(combi)] 
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset

# corelated variable

cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)


#  Variables price_per_unit_wt and Item_Weight are highly correlated as the former one was created from the latter. Similarly price_per_unit_wt and Item_MRP are highly correlated for the same reason


# Building Model
linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])
summary(linear_reg_mod)
pred1=predict(linear_reg_mod,test)

# Making Predictions on test Data


set.seed(1235) 
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002)) 

lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,method='glmnet', trControl= my_control, tuneGrid = Grid)
summary(lasso_linear_reg_mod)
pred=predict(lasso_linear_reg_mod,test)



# ridge
set.seed(1236) 
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002)) 
ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,method='glmnet', trControl= my_control, tuneGrid = Grid)
pred3= predict(ridge_linear_reg_mod,test)


# random forest
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

pred4=predict(rf_mod,test)


# Best Model Parameters
plot(rf_mod)

# Variable Importance
plot(varImp(rf_mod))


# XGBoost
param_list = list(objective = "reg:linear", eta=0.01,gamma = 1,max_depth=6,subsample=0.8,colsample_bytree=0.5)
dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales) 
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))


set.seed(112) 
xgbcv = xgb.cv(params = param_list,data = dtrain,nrounds = 1000,nfold = 5,print_every_n = 10,early_stopping_rounds = 30,maximize = F)

