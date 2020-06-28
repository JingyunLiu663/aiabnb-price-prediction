####################################### 1. Read the File ##########################################
# set working directory to source file location 

# remove all the variables in the global environment
rm(list = ls())
airbnb = read.table('airbnb.csv', header = TRUE, sep = ",")
str(airbnb)
summary(airbnb)
attach(airbnb)

# convert categorical values into numeric value 
x0=model.matrix(price~.,airbnb)
x=x0[,-1]
airbnb.new<-cbind(airbnb$price,x)
airbnb.df<-data.frame(airbnb.new)

# data partition
set.seed(1)
train = sample(1:nrow(airbnb), 4*nrow(airbnb)/5)
y_test = airbnb[-train, "price"]

################################### 2. Exploratory Data Analysis ####################################### 
#import packages for Exploratary Data Anlysis
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggjoy)
library(gridExtra)
library(waffle)
library(leaflet)
library(rgdal)
library(corrplot)
# readin data
airbnb = read.table('airbnb.csv', header = TRUE, sep = ",")
str(airbnb)
summary(airbnb)
attach(airbnb)

# To plot the correlation heatmap for all the quantitative variables
data_cor <- cor(airbnb[-c(2, 4, 5, 6, 9, 13, 19, 20, 23, 24)], use = "complete.obs", method = "pearson")
cor_heatmap <- corrplot(data_cor, method="circle", type = "upper", order="hclust", tl.col="black")

# create a list for all the boroughs belonging to Inner London (Other boroughs are considered Outer London)
inner.ldn <- list('Camden', 'City of London', 'Greenwich', 'Hackney', 'Hammersmith and Fulham', 'Islington',
                  'Kensington and Chelsea', 'Lambeth', 'Lewisham', 'Southwark', 'Tower Hamlets','Wandsworth', 'Westminster')

# add a column named 'neighbour_in_out' indicating whether the neighbourhood belongs to Inner London or Outer London
eda.data <- airbnb %>% 
  mutate(neighbour_in_out = ifelse(neighbour %in% inner.ldn, "Inner","Outer"))

# add a column named 'price_group' indicating which price bracket the listing price falls into
# Very high - Higher than 80% percentile
# High - Higher than 60% percentile, lower than 80% percentile
# Moderate - Higher than 40% percentile, lower than 60% percentile
# Low - Higher than 20% percentile, lower than 40% percentile
# Very Low - Lower than 20% percentile
eda.data <- eda.data %>% 
  mutate(price_group = ifelse(price < quantile(price, prob = 0.2), "Very Low",
                              ifelse(price < quantile(price, prob = 0.4), "Low",
                                     ifelse(price < quantile(price, prob = 0.6), "Moderate",
                                            ifelse(price < quantile(price, prob = 0.8), "High", "Very High")))))

# create a column named 'usage_group' indicating the number of revires under the listed accommodation
eda.data <- eda.data %>% 
  mutate(usage_group=ifelse(number_of_reviews < quantile(number_of_reviews, prob = 0.25), "None",
                            ifelse(number_of_reviews < quantile(number_of_reviews, prob = 0.5), "Low",
                                   ifelse(number_of_reviews < quantile(number_of_reviews, prob = 0.75), "Moderate",
                                          ifelse(number_of_reviews < quantile(number_of_reviews, prob = 0.85), "High", "Very High")))))

# Since nearly 70% of the listings belongs to top 10 boroughs in inner London, 
# we keep the top 10 boroughs (in terms of the number of listings), while treat other boroughs like 'Others'
top.10.boroughs <- split(names(tail(sort(table(neighbour)), 10)), gl(10, 1))
eda.data <- eda.data %>% 
  mutate(neighbour_top10 = ifelse(neighbour %in% top.10.boroughs, as.character(neighbour), "Others"))

# the dataframe named 'eda.data' is used for EDA
attach(eda.data)

# set global theme for ggplot2 objects
thm <- theme_fivethirtyeight() + theme(axis.title = element_text(), axis.title.x = element_text()) 

## Plot 1: Distribution of Price
plot1 <- ggplot(eda.data, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "lightblue") + 
  geom_density(alpha = 0.2, fill = " lightblue") +
  thm +
  ggtitle("Distribution of Price",
          subtitle = "The price of listed accommodation is skewed to the right") +
  geom_vline(xintercept = round(mean(eda.data$price), 2), size = 1, linetype = 3)
# to display plot1
plot1

## Plot 2: Heatmap for listings of accommodation
# import the spatial data of London
# Attention: You should change 'dsn' to the shp file path on your local computer
ldn <- readOGR(dsn = 'London_Sport/london_sport.shp')
proj4string(ldn) <- CRS("+init=epsg:27700")
ldn.wgs84 <- spTransform(ldn, CRS("+init=epsg:4326"))
#create a map object for London
map.ldn <- ggplot(ldn.wgs84) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = 'white', colour = 'black')

plot2 <- map.ldn +
  stat_density2d(data=eda.data, aes(longitude, latitude, fill=..level.., alpha=..level..), geom="polygon", size=0.01, bins=120) +
  scale_fill_gradient(low="#4b8ac8", high="#e8f4f8", breaks=seq(0,80000,10000),
                      limits=c(0, 80000), guide = "legend", name = "Acommodation") +
  scale_alpha(range=c(0.2, 0.4), guide=FALSE) +
  labs(x = "", y = "") +
  theme_classic() + 
  ggtitle("Heatmap for listings of accommodation in London",
          subtitle = "This figure reflects the geographic distribution of the number of listings")
# to display plot2
plot2

# Plot 3:  Relationship between Price Group and Geographic Location
pal <- colorFactor(palette = c("orange", "blue", "green", "red", "purple"), domain = eda.data$price_group)
plot3 <- leaflet(data = eda.data) %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal(price_group), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1) %>% 
  addLegend("bottomright", pal = pal, values = ~price_group,
            title = "Price Levels",
            opacity = 1)
# to display plot3
plot3

# Plot 4: Geographic Distribution of Listings Grouped by Different Room Type 
pal <- colorFactor(palette = c("yellow", "blue", "purple", "red"), domain = eda.data$room_type)
plot4 <-leaflet(data = eda.data) %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal(room_type), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1) %>% 
  addLegend("bottomright", pal = pal, values = ~room_type,
            title = "Room Type",
            opacity = 1)
# to display plot4
plot4

# Plot 5: Description Square Pie Charts by Different Room Type
# calculate the number of listings from different room type
entire.room.no <- nrow(subset(eda.data, room_type == "Entire home/apt"))
private.room.no <- nrow(subset(eda.data, room_type == "Private room"))
hotel.room.no<- nrow(subset(eda.data, room_type == "Hotel room"))
shared.room.no <- nrow(subset(eda.data, room_type == "Shared room"))

room_types <- c("Entire home/ap" = entire.room.no, "Private room" = private.room.no, "Hotel room" = hotel.room.no, "Shared room" = shared.room.no)
plot5 <- waffle(room_types/400, rows = 9, colors = c("lightblue1", "lightblue2", "lightblue3", "lightblue4"),
                size = 0.25, legend_pos = "top", xlab = "One square = about 400 listed rooms") + 
  thm + 
  ggtitle("Room Type", 
          subtitle = "Nearly half of the rooms are Entire home/ap, the other half are Private room, Hotel room & Shared room appear to be trivia")
# to display plot5
plot5

# Plot 6: Geographic Distribution for Extremely High Listings Grouped by Room Type
very.high <- eda.data %>%
  filter(price > quantile(price, prob = 0.8))

plot6 <- map.ldn + geom_point(data = very.high, aes(x = longitude, y = latitude, size = price, colour = factor(room_type), alpha=0.05)) + 
  scale_colour_brewer(type="qual", palette="Dark2", name = "Room type")+ 
  labs(x = 'Longitude', y = 'Latitude', title =  "Most Expensive Listings Grouped by Room Types", subtitle = '(Price Higher Than 80% Percentile)') +
  thm
# to display plot6
plot6

# Plot 7: Geographic Distribution for Extremely Low Listings Grouped by Room Type
very.low <- eda.data %>%
  filter(price < quantile(price, prob = 0.2))

plot7 <- map.ldn + geom_point(data = very.low, aes(x = longitude, y = latitude, size = price, colour = factor(room_type), alpha=0.05)) + 
  scale_colour_brewer(type="qual", palette="Dark2", name = "Room type")+ 
  labs(x = 'Longitude', y = 'Latitude', title = "Cheapest Listings Grouped by Room Types", subtitle = '(Price Lower Than 20% Percentile)') +
  thm
# to display plot7
plot7

# Plot 8: Boxplot of Price by Room Type
plot8 <- ggplot(eda.data, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) +
  thm +
  xlab("Room Type") + 
  ylab("Price") +
  ggtitle("Boxplot of Price by Room Type",
          subtitle = "Entire home/apt & Hotel Room have price higher than Private Room & Shared Room") +
  geom_hline(yintercept = mean(eda.data$price), color = "purple", linetype = 2)
# to display plot8
plot8

# Boxplot of price by neighbour group
plot9 <-ggplot(eda.data, aes(x = neighbour_in_out, y = price)) +
  geom_boxplot(aes(fill = neighbour_in_out)) +
  thm +
  xlab("Inner/Outer London") + 
  ylab("Price") +
  ggtitle("Boxplots of Price (Inner/Outer London)",
          subtitle = "Most of Outer London is under average price") +
  geom_hline(yintercept = mean(eda.data$price), color = "purple", linetype = 2)
# to display plot9
plot9

# Plot 10: Boxplots of Price by Neighbourhoods (Top 10 and Others)
plot10 <- ggplot(eda.data, aes(x = neighbour_top10, y = price)) +
  geom_boxplot(aes(fill = neighbour_top10)) +
  thm +
  xlab("Boroughs") + 
  ylab("Price") +
  ggtitle("Boxplots of Price by Neighbourhoods (Top 10 and Others)",
          subtitle = "Kensington & Chelsea, Westminster and Camden have the higher pricing level") +
  geom_hline(yintercept = mean(eda.data$price), color = "purple", linetype = 2)
# to display plot10
plot10

# Plot 11: Number of Above Average Price Objects
plot11 <- eda.data %>% filter(price >= mean(price)) %>% group_by(neighbour_top10, room_type) %>% tally %>% 
  ggplot(aes(reorder(neighbour_top10,desc(n)), n, fill = room_type)) +
  thm +
  xlab(NULL) +
  ylab("Number of objects") +
  ggtitle("Number of Above Average Price Objects",
          subtitle = "Most of them are Entire Home/Apts") +
  geom_bar(stat = "identity")
# to display plot11
plot11

# Plot12: The Relationship between Price and Number of Reviews - Scatter Plot
plot12 <- ggplot(eda.data, aes(number_of_reviews, price)) +
  thm + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_point(alpha = 0.1, color = "lightblue2") +
  xlab("Number of Reviews") +
  ylab("Price") +
  ggtitle("The Relationship between Price and Number of Reviews - Scatter Plot",
          subtitle = "When the number of reviews reached certain level, the price will be at a relative low level")
# to display plot12
plot12

# Plot 13: Price Distribution for Different Room Type
plot13 <- ggplot(eda.data, aes(x = price, y = room_type)) +
  geom_joy(scale = 3.1, fill = "skyblue2", alpha = 0.65)+
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,40))+
  labs(x = "Price [in $]", y = "") +
  thm +
  ggtitle("Price Distribution for Different Room Type") 
# to display plot13
plot13

# Plot 14: Relationship between Price and Neighbourhood (Inner/ Outer London)
plot14 <- ggplot(eda.data, aes(x = price)) +
  geom_histogram(aes(color = neighbour_in_out, fill = neighbour_in_out), 
                 position = "identity", bins = 30, alpha = 0.4) +
  thm +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  xlab("Inner/Outer London") +
  ylab("Price") +
  ggtitle("Relationship between Price and Neighbourhood (Inner/ Outer London") 
# to display plot14
plot14

### Distribution of Predict Variables + Scatter Plot for Response and Predict Variables
subplot1 <- ggplot(eda.data, aes(bathrooms)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "lightblue") + 
  geom_density(alpha = 0.2, fill = " lightblue") +
  thm +
  ggtitle("Distribution of Number of Bathroom") +
  geom_vline(xintercept = round(mean(bathrooms), 2), size = 1, linetype = 3)


subplot2 <- ggplot(eda.data, aes(bedrooms)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "lightblue") + 
  geom_density(alpha = 0.2, fill = " lightblue") +
  thm +
  ggtitle("Distribution of Number of Bedroom") +
  geom_vline(xintercept = round(mean(bedrooms), 2), size = 1, linetype = 3)


subplot3 <- ggplot(eda.data, aes(number_of_reviews)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "lightblue") + 
  geom_density(alpha = 0.2, fill = " lightblue") +
  thm +
  ggtitle("Distribution of Number of Reviews") +
  geom_vline(xintercept = round(mean(number_of_reviews), 2), size = 1, linetype = 3)


subplot4 <- ggplot(eda.data, aes(bathrooms, price)) +
  thm + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_point(alpha = 0.3, color = "lightblue2") +
  xlab("Number of Bathroom") +
  ylab("Price") +
  ggtitle("Price vs Number of Bathroom")

subplot5 <- ggplot(eda.data, aes(bedrooms, price)) +
  thm + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_point(alpha = 0.3, color = "lightblue2") +
  xlab("Number of Bedrooms") +
  ylab("Price") +
  ggtitle("Price vs Number of Bedroom")

subplot6 <- ggplot(eda.data, aes(number_of_reviews, price)) +
  thm + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_point(alpha = 0.3, color = "lightblue2") +
  xlab("Number of Reviews") +
  ylab("Price") +
  ggtitle("Price vs Number of Reviews")

plot15 <- grid.arrange(subplot1,subplot4, ncol=1)
plot16 <- grid.arrange(subplot2,subplot5, ncol=1)
plot17 <- grid.arrange(subplot3,subplot6, ncol=1)

subplot7 <- ggplot(eda.data, aes(availability_365)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "lightblue") + 
  geom_density(alpha = 0.2, fill = " lightblue") +
  thm +
  ggtitle("Distribution of Availability Days in a Year") +
  geom_vline(xintercept = round(mean(availability_365), 2), size = 1, linetype = 3)

subplot8 <- ggplot(eda.data, aes(minimum_nights)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "lightblue") + 
  geom_density(alpha = 0.2, fill = " lightblue") +
  thm +
  ggtitle("Distribution of Minimum Booking Nights") +
  geom_vline(xintercept = round(mean(minimum_nights), 2), size = 1, linetype = 3)

subplot9 <- ggplot(eda.data, aes(extra_people)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "lightblue") + 
  geom_density(alpha = 0.2, fill = " lightblue") +
  thm +
  ggtitle("Distribution of Price for Extra People") +
  geom_vline(xintercept = round(mean(extra_people), 2), size = 1, linetype = 3)

subplot10 <- ggplot(eda.data, aes(availability_365, price)) +
  thm + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_point(alpha = 0.3, color = "lightblue2") +
  xlab("Availability in a Year") +
  ylab("Price") +
  ggtitle(" Price vs Availability Days in a Year")

subplot11 <- ggplot(eda.data, aes(minimum_nights, price)) +
  thm + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_point(alpha = 0.3, color = "lightblue2") +
  xlab("Minimum Booking Nights") +
  ylab("Price") +
  ggtitle("Price vs Minimum Booking Nights")

subplot12 <- ggplot(eda.data, aes(extra_people, price)) +
  thm + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_point(alpha = 0.3, color = "lightblue2") +
  xlab("Price for Extra People") +
  ylab("Price") +
  ggtitle("Price vs Price for Extra People")

plot18 <- grid.arrange(subplot7,subplot10, ncol=1)
plot19 <- grid.arrange(subplot8,subplot11, ncol=1)
plot20 <- grid.arrange(subplot9,subplot12, ncol=1)

######################################## 3. Multi Linear Regression ############################################## 
## Forward Selection 
library(leaps)

fit.fwd=regsubsets(V1 ~.,airbnb.df,nvmax=60,method="forward",subset = train) 
sum.fwd=summary(fit.fwd)

sum.fwd$adjr2
which.max(sum.fwd$adjr2) 
coef(fit.fwd,58)

my.lm = lm(price ~ days_since_host+host_is_superhost+host_listings_count+host_identity_verified+neighbourBarnet+
             neighbourBexley+neighbourBrent+neighbourBromley+neighbourCamden+neighbourCity.of.London+neighbourCroydon+
             neighbourEaling+neighbourEnfield+neighbourGreenwich+neighbourHackney+neighbourHammersmith.and.Fulham+
             neighbourHaringey+neighbourHarrow+neighbourHavering+neighbourHillingdon+neighbourHounslow+neighbourIslington+
             neighbourKensington.and.Chelsea+neighbourKingston.upon.Thames+neighbourLambeth+neighbourLewisham+
             neighbourMerton+neighbourNewham+neighbourRedbridge+neighbourRichmond.upon.Thames+neighbourSouthwark+
             neighbourSutton+neighbourTower.Hamlets+neighbourWaltham.Forest+neighbourWandsworth+neighbourWestminster+
             latitude+longitude+room_typeHotel.room+room_typePrivate.room+room_typeShared.room+accommodates+bathrooms+
             bedrooms+extra_people+minimum_nights+availability_365+number_of_reviews+last_review+instant_bookable+
             longterm_allowed+number_of_amenities+number_of_verifications+property_typeCondominium+property_typeHouse+
             property_typeOthers+property_typeTownhouse+real_bed, 
             airbnb.df,subset = train)

print("The model")
my.lm

print("Summary of Regression")
summary(my.lm)

y_pred = predict(my.lm, newdata = airbnb.df[-train,])
print("MSE on Test Set")
mean((y_test-y_pred)^2)

plot(sum.fwd$adjr2)
plot(y_test, y_pred) # plot y_pred vs y_test

## Ridge Regression
library(glmnet)
grid=10^seq(10,-2,length=100)
y=price
cvfit <- cv.glmnet(x[train,],y[train], alpha =0, lambda = grid )
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=0.01) 

print("The model")
ridge.mod

print("Summary of Ridge Regression")
summary(ridge.mod)

y_pred = predict(ridge.mod,s=grid[100],newx=x[-train,])
print("MSE on Test Set")
mean((y_test-y_pred)^2)

plot(y_test, y_pred) # plot y_pred vs y_test

############################################## 4. Decision Tree ############################################## 
library(tree)
## without pruning
tree.airbnb=tree(V1 ~ .,data = airbnb.df,subset = train)
summary(tree.airbnb)

plot(tree.airbnb)
text(tree.airbnb,pretty=0)

y.pred.treeoriginal = predict(tree.airbnb,newdata = airbnb.df[-train,])

print("MSE on Test Set (Not Pruned)")
mean((y_test-y.pred.treeoriginal)^2)

plot(y_test,y.pred.treeoriginal)
abline(0,1)

## with pruning
set.seed(5)
cv.airbnb=cv.tree(tree.airbnb)
plot(cv.airbnb$size,cv.airbnb$dev,type='b')
prune.airbnb=prune.tree(tree.airbnb,best=6)

plot(prune.airbnb)
text(prune.airbnb,pretty=0)

y.pred.treeprune=predict(prune.airbnb,newdata = airbnb.df[-train,])

print("MSE on Test Set (Pruned Tree)")
mean((y_test-y.pred.treeprune)^2)

plot(y_test,y.pred.treeprune)

######################################## 5. Random Forest Regression ############################################## 
library(randomForest)

set.seed(1)
fit.forest = randomForest(price ~ ., data = airbnb, subset = train, mtry = 8, ntree = 50, importance = TRUE)

# sink("rf_output.txt")
print("The model")
fit.forest

print("Feature Importance")
importance(fit.forest)

print("Summary of Random Forest")
summary(fit.forest)

y_pred = predict(fit.forest, newdata = airbnb[-train,])
print("MSE on Test Set")
mean((y_test-y_pred)^2)
# sink()

varImpPlot(fit.forest) # plot feature importance

plot(y_test, y_pred) # plot y_pred vs y_test
abline(0, 1)

## cross validation: to select mtry in c(6, 8, 10, 12)
## skip as a result of large computation cost
# mse_oob=rep(0,4)
# mse_test=rep(0,4)
# for (i in c(6,8,10,12)){
#   set.seed(1)
#   fit.forest = randomForest(price~., data = airbnb, subset = train, mtry = i, ntree = 30, importance = TRUE)
#   mse_oob[i] = fit.forest$mse[fit.forest$ntree]
#   importance(fit.forest)
#   summary(fit.forest)
#   
#   y_pred = predict(fit.forest, newdata = airbnb[-train,])
#   mse_test[i] = mean((y_test-y_pred)^2)
# }
# 
# plot(mse_oob, type = "l", col = "red")
# lines(mse_test, col = "blue")

########################################## 6. Support Vector Regression #############################################
library(e1071)

set.seed(1)
svr_airbnb = svm(as.formula(paste('price ~', paste(colnames(airbnb.df[, -1 ]), collapse = ' + '))),
                 data = airbnb.df,subset = train)

y_pred_svr=predict(svr_airbnb,newdata=airbnb.df[-train,])

print("The model")
svr_airbnb

print("Summary of SVR")
summary(svr_airbnb)

print("MSE on Test Set")
mean((y_test-y_pred_svr)^2)

plot(y_test,y_pred_svr)
abline(0,1)

####################################### 7. Gradient Boosting Machine ###############################################
library(gbm)

set.seed(1)
gbm_airbnb =gbm(V1 ~., data = airbnb.df[train,], n.trees = 15000,interaction.depth=4,shrinkage=0.1)
print("The model")
gbm_airbnb

print("Summary of GBM")
summary(gbm_airbnb)

print("MSE on Test Set")
y_pred_gbm=predict(gbm_airbnb,newdata=airbnb.df[-train,],n.trees=15000)
mean((y_test-y_pred_gbm)^2)

plot(y_test,y_pred_gbm)
abline(0,1)

summary.gbm(gbm_airbnb,cBars = 10)

########################################### 8. Neural Networks ##################################################
library(dplyr)
library(janitor)
library(neuralnet)

# Since nearly 70% of the listings belongs to top 10 boroughs in inner London, 
# we keep the top 10 boroughs (in terms of the number of listings), while treat other boroughs like 'Others'
top.10.boroughs <- split(names(tail(sort(table(neighbour)), 10)), gl(10, 1))
nn.data <- airbnb %>% 
  mutate(neighbour_top10 = ifelse(neighbour %in% top.10.boroughs, as.character(neighbour), "Others"))

keeps <- c("price", "room_type", "accommodates", "bedrooms","bathrooms", "number_of_amenities", "longitude", "latitude","days_since_host", "host_listings_count", "availability_365", "neighbour_top10")
nn.data <- nn.data[ , (names(nn.data) %in% keeps)]

# convert categorical values into numeric value 
nn.data <- model.matrix( 
  as.formula(paste('~ ', paste(colnames(nn.data), collapse = ' + '), ' + 0')), 
  data = nn.data
)
nn.data <- as.data.frame(nn.data)

#replace the space in newly-generated column names with underline
nn.data <- clean_names(nn.data)

# MAX-MIN NORMALIZATION
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
nn.data <- as.data.frame(sapply(nn.data, normalize))

# train - test partition
set.seed(1)
train <- sample(1:nrow(nn.data), 4*nrow(nn.data)/5)

fml <- as.formula(paste('price ~', paste(colnames(nn.data[, - which(colnames(nn.data) == "price")]), collapse = ' + ')))
set.seed(1)
nn.airbnb <- neuralnet(fml, nn.data[train,], hidden = 2, threshold = 0.01, stepmax=1e+08)
plot(nn.airbnb)

# Test the resulting output
comp <- compute(nn.airbnb, nn.data[-train, - which(colnames(nn.data) == "price")])
# Scale back the target variable
y.pred.nn <- comp$net.result * (max(airbnb$price) - min(airbnb$price)) + min(airbnb$price)

print("MSE on Test Set")
mean((y_test - y.pred.nn)^2)

# plot the scatter plot
plot(y_test, y.pred.nn)
abline(0, 1)

