library("rlang")
library("scatterplot3d")
library("bindr")
library("dplyr")
library("crayon")
library("curl")
library("DescTools")
library("tidyverse")
library("GGally")
library("ggplot2")
library("mongolite")
library("plotly")
library("plyr")
library("reshape")
library("reshape2")
library("e1071")
library("utils")
library("caret")

#import the mongoDB dataset
m <- mongo(collection = "BlackFriday",db="analysis_3",url="mongodb://localhost")
blackfriday <- m$find()

#****************************************************#

#charts and plots to gain basic understanding of data

# 1) gender vs purchase

# first we query from the db a subset containing the total purchase per gender 
# and average purchase (by gender)
# then convert this to a r-dataset
# finally plot this on a barchart

m$mapreduce(
            mapfunction = function(){
              emit(this.gender,this.purchase)
            },
            reducefunction = function(varG,varP){
              return Array.avg(varP)
            },
            {out:}
)

# 2) age vs purchase

# 3) occupation vs purchase

# 4) city_category vs purchase


#****************************************************#

#queries to extract relevant tables (subsets) for further analysis

# 1) find number of products

length(m$distinct("product_id"))

# 2) find list of distinct age groups

m$distinct("age")

# 3) find list of distinct ocupations

sort(m$distinct("occupation"))

# 4) find list of unique city_category

m$distinct("city_category")

# 5) find unique 3-level product category combination

sort(m$distinct("product_category_1"))
sort(as.numeric(m$distinct("product_category_2")))
sort(as.numeric(m$distinct("product_category_3")))

m$aggregate('[
             {"$group":
                 { "_id":
                     {"tag1":"$product_category_1",
                      "tag2":"$product_category_2",
                      "tag3":"$product_category_3"}
                 }
               }
]')

# 6) find no of customers in each age group by gender


m$aggregate('[
    {"$group": {"_id": {"gender":"$gender","age":"$age"},
      "numOfCust":{"$sum": 1}}},
    {"$sort": {"gender":1}}
    ]')

# 7) find average and total spending by each age group


grp_spending_by_age <- m$aggregate('[
            {"$group": 
                 { "_id": "$age",
                  "tot_sales": {"$sum": "$purchase"},
                  "avg_sales": {"$avg": "$purchase"}
                 }
}]')


# 8) find average and total spending by each gender

grp_spending_by_gender <- m$aggregate('[
            {"$group": 
                              { "_id": "$gender",
                              "tot_sales": {"$sum": "$purchase"},
                              "avg_sales": {"$avg": "$purchase"}
                              }
                              }]')

# 9) find average and total spending by each occupation

grp_spending_by_occupation <- m$aggregate('[
            {"$group": 
                              { "_id": "$occupation",
                              "tot_sales": {"$sum": "$purchase"},
                              "avg_sales": {"$avg": "$purchase"}
                              }
                              }]')



# 10) find average and total spending by each city category

m$aggregate('[
            {"$group": 
                              { "_id": {"city_category":"$city_category",
                                        "no_of_yrs_in_city":"$stay_in_current_city_years"},
                              "total_sales": {"$sum": "$purchase"},
                              "avge_sales": {"$avg": "$purchase"}
                              }
                              }]')



#****************************************************#

#find correlations between the data
correlation <- ggpairs(bf[,c(7,12)],aes(color=status,alpha=0.4))
print(correlation)

bf <-blackfriday
dummy <- dummy_vars(" ~ .", data = bf)
trsf <- data.frame(predict(dummy, newdata = bf))
cor_mat <- round(cor(bf[,c(3:12)]),2)
head(cormat)

#****************************************************#

#checking hypothesis

# first change data types
bf$product_ID = as.factor(bf$product_id)
bf$gender= as.factor(bf$gender)
bf$age = as.factor(bf$age)
bf$city_category =as.factor(bf$city_category)
bf$stay_in_current_city_years= as.factor(bf$stay_in_current_city_years)
bf$product_category_2 =as.integer(bf$product_category_2)
bf$product_category_3 = as.integer(bf$product_category_3)
str(bf)

#find missing values
t(colSums(is.na(bf)))

# 1) women are more likely to spend
female_purchase <- bf %>% filter(gender =="F") %>% select(purchase)
male_purchase <- bf %>% filter(gender =="M") %>% select(purchase)
t.test(male_purchase,female_purchase, alternative = "less")
  ###implies that men are more likely to spend more
ggplot(aes(x=gender,y=purchase,fill=gender),data=bf)+
  geom_boxplot()+ggtitle("Analysis - Purchases on Black Friday  : Men vs Women")


# 2) married couples spend more on black friday sales
single <-bf %>% filter(marital_status == 0) %>% select(purchase)
married <-bf %>% filter(marital_status == 1) %>% select(purchase)
t.test(single,married)
  ###implies taht there is no significant difference between 
    #married or single customers
ggplot(aes(x=factor(marital_status),y=purchase,fill=gender),data=bf)+
  geom_boxplot()+ggtitle("Analysis - Purchases on Black Friday  :Married vs Single")

# 3) how does age impact spending
diff_spend <- aov(purchase~age,data=bf)
summary(diff_spend)
  ### implies that the Pr(>F) betwen the different ranges is too small
    # and hence there is an impact on purchase 
#pairwise.t.test(bf$Purchase,as.factor(bf$Age))
ggplot(aes(x=age,y=purchase,fill=age),data=bf)+
  geom_boxplot()+ggtitle("Analysis - Purchases on Black Friday  :Who Shall We Target")

# 4) does it(spending) matter which part of the city they live in
city_spend <- aov(purchase~city_category,data=bf)
summary(city_spend)
  ###implies that the area of residence in the city affects amt of spending
ggplot(aes(x=city_category,y=purchase,fill=city_category),data=bf)+
  geom_boxplot()+ggtitle("Analysis - Purchases on Black Friday  :City Divisions")

# 5) does how long they have been at their current residences impact spending
resi_spend <- aov(purchase~stay_in_current_city_years,data=bf)
summary(resi_spend)
   ### implies no significant difference
ggplot(aes(x=stay_in_current_city_years,y=purchase,fill=city_category),data=bf)+
  geom_boxplot()+ggtitle("Analysis - Purchases on Black Friday  :Years of Residence")

# 6) do people in certain occupations tend to spend more
occu_spend<-aov(purchase~occupation,data=bf)
summary(occu_spend)
ggplot(aes(x=as.factor(occupation),y=purchase,fill=gender),data=bf)+
  geom_boxplot()+ggtitle("Analysis - Purchases on Black Friday  :Occupation")


# 7)Men spend more than women,btw ages 51-55,living in area C,engaged in occupation 12
bf %>% filter(age=="51-55",city_category=="C",occupation=="17") %>%
  ggplot(aes(x=gender,y=purchase,fill =as.factor(marital_status)))+geom_boxplot()+
  ggtitle("Analysis - Purchases on Black Friday  :Occupation-17")
   ### guess we could say that our hypothesis is true visually ie.
f<-bf %>% filter(age=="51-55",city_category=="C",gender=="F") %>% select(purchase)
m<-bf %>% filter(age=="51-55",city_category=="C",gender=="M") %>% select(purchase)
t.test(f,m,alternative = "less")
#****************************************************#

#linear regression to predict purchase price

summary(blackfriday)
str(bf)
#find number of missing values
sapply(bf,function(x) sum(is.na(x)))
#dependent variable : blackfriday$purchase
#independent categorical variable : blackfriday$Gender,blackfriday$Age,
#blackfriday$Occupation, blackfriday$City_Category, blackfriday$Marital_Status,
#independent variable:blackfriday$Stay_In_Current_City_Years

sample <- sample(1:nrow(bf),size=floor(nrow(bf)*0.7))
train<-bf[sample,]
test<-bf[-sample,]

lr_mod1 <- lm(purchase~
                gender+age+occupation+city_category+stay_in_current_city_years+
                marital_status+product_category_1,data = train)
summary(lr_mod1)
step<-step(lr_mod1)

train_pred_pur <- predict(lr_mod1,newdata = train)
tr <- cbind(train,train_pred_pur)
test_pred_pur <- predict(lr_mod1,newdata = test)
te <- cbind(test,test_pred_pur)

#checking model accuracy by MAPE
(mean(abs((tr$purchase-tr$train_pred_pur)/tr$train_pred_pur)))
(mean(abs((te$purchase-te$test_pred_pur)/te$test_pred_pur)))
#checing model accuracy by RMSE
(sqrt(mean(tr$purchase-tr$train_pred_pur)**2))
(sqrt(mean(te$purchase-te$test_pred_pur)**2))
  ############################################


#****************************************************#
