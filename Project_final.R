library(readr)
library(dplyr)
library(lme4)
library(ggplot2)
library(readxl)
library(ggcorrplot)
library(mice)
library(mitools)
library(psych)
library(ggplot2)
library(car)
library(gridExtra)
library(nortest)

#Importing data
data <- read_excel("latest_games_update_2.xlsx")

str(data)
summary(data)

# Check for missing values
sum(is.na(data))

# Convert columns to the appropriate data types
data$platform <- as.factor(data$platform)
data$genre <- as.factor(data$genre)
data$publisher <- as.factor(data$publisher)

# Remove yen symbol from price column
data$price <- gsub("¥", "", data$price)

# Find non-numeric values in the 'price' column
non_numeric_price <- which(!is.na(as.numeric(data$price, warn = FALSE)) == FALSE)
data$price[non_numeric_price]

# Replace non-numeric values in the 'price' column with NAs or appropriate values
data$price[non_numeric_price] <- NA  # Replace with NA

# Check unique values in price column
unique(data$price)

# Check if there are any missing values in price column
any(is.na(data$price))
summary(data$price)
table(is.na(data$price))

# Impute missing price values with the median
median_price <- median(data$price, na.rm = TRUE)
data$price[is.na(data$price)] <- median_price

# Verify that there are no more missing values in price column
sum(is.na(data$price))

# Find non-numeric values in the 'weekly_sales' column
non_numeric_weekly_sales <- which(!is.na(as.numeric(data$weekly_sales, warn = FALSE)) == FALSE)
data$weekly_sales[non_numeric_weekly_sales]

# Replace non-numeric values in the 'weekly_sales' column with NAs or appropriate values
data$weekly_sales[non_numeric_weekly_sales] <- NA  # Replace with NA

# Convert columns to the appropriate data types
data$platform <- as.factor(data$platform)
data$genre <- as.factor(data$genre)
data$publisher <- as.factor(data$publisher)
data$price <- as.numeric(data$price)
data$weekly_sales <- as.numeric(data$weekly_sales)

# Create plots to visualize the distribution of the variables
ggplot(data, aes(x = total_sales)) + geom_histogram(bins = 50) + ggtitle("Total Sales")
ggplot(data, aes(x = market_share)) + geom_histogram(bins = 50) + ggtitle("Market Share")
ggplot(data, aes(x = price)) + geom_histogram(bins = 50) + ggtitle("Price")+scale_x_continuous(limits = c(0, 13))

# Calculate mean and standard deviation
mean_price <- mean(data$price, na.rm = TRUE)
sd_price <- sd(data$price, na.rm = TRUE)

# Define the threshold for outliers (mean + 2 * standard deviation)
threshold <- mean_price + 2 * sd_price

# Add row number to the data
data <- data %>%
  mutate(row_id = row_number())

# Create the scatter plot
ggplot(data, aes(x = row_id, y = price, color = is_outlier)) +
  geom_point() +
  ggtitle("Price Scatter Plot") +
  scale_color_manual(values = c("Not Outlier" = "black", "Outlier" = "red")) +
  theme_minimal()

# Find the exact outlier rows
outlier_rows <- which(data$is_outlier == "Outlier")
outlier_rows

df_new <- data[-c(230,379,434,468,559,616,650,963,1143,1173,1292,2788,2789,2817,4323,4735,4742,4743,4766,4801,5248,5249,5251,5880,5881,5910,8453,8485,8510,8543,8570,8604,8634,8701,8725,8757,8970,8971,9266,9267,9358,9387,9473,9506,11187,11188,11209,11210,11246,11247,11278,11279,11309,11789,12121), ]

df_new <- data[-c(3421), ]

data<- df_new

# Filter out missing and non-finite values
data_filtered <- data %>%
  filter(!is.na(price) & is.finite(price))

# Create the plot without limits
ggplot(data_filtered, aes(x = price)) +
  geom_histogram(bins = 50) +
  ggtitle("Price") + scale_x_continuous(limits = c(0,11))
geom_vline(aes(xintercept = threshold), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = threshold, y = Inf, label = "Outliers Threshold", vjust = 2, hjust = 0, color = "red")

# Create scatter plots to check linearity between predictors
data$release_date_numeric <- as.numeric(data$release_date - min(data$release_date, na.rm = TRUE), units = "days")
pairs.panels(data[, c("no_of_weeks", "total_sales", "market_share", "price", "release_date_numeric")])

# Create scatter plots to check homoscedasticity between predictors
plot(data$total_sales, data$market_share, xlab = "Total Sales", ylab = "Market Share")
plot(data$total_sales, data$price, xlab = "Total Sales", ylab = "Price")
plot(data$total_sales, data$release_date, xlab = "Total Sales", ylab = "Release Week")
plot(data$market_share, data$price, xlab = "Market Share", ylab = "Price")
plot(data$market_share, data$release_date, xlab = "Market Share", ylab = "Release Week")
plot(data$price, data$release_date, xlab = "Price", ylab = "Release Week")

# Select relevant features
data_1 <- select(data, weekly_sales,game_title, genre, no_of_weeks, total_sales, market_share, price, year, week_no, top_10)
#including publisher column
data_2 <- select(data, weekly_sales,publisher,game_title, genre, no_of_weeks, total_sales, market_share, price, year, week_no, top_10)

# Rescale numeric predictor variables
data_scaled <- data_1
data_scaled$total_sales <- scale(data$total_sales)
data_scaled$market_share <- scale(data$market_share)

#scaling for data including publisher
data_scaled_pub <- data_2
data_scaled_pub$total_sales <- scale(data$total_sales)
data_scaled_pub$market_share <- scale(data$market_share)

#Linear model
lm_fit <- lm(weekly_sales ~ total_sales + market_share + price + week_no + top_10, data = data_1)
summary(lm_fit)
vif(lm_fit)

# Check model assumptions
plot(lm_fit, which = 1)  # Residuals vs Fitted
plot(lm_fit, which = 2)  # Normal Q-Q plot
plot(lm_fit, which = 3)  # Scale-Location
plot(lm_fit, which = 4)  # Cook's distance

#based on genre
fit_genre <- lmer(weekly_sales ~ market_share + price + week_no + top_10 +  genre + (1| genre),
                  data = data_1, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
vif(fit_genre) #for genre the data needs to be scaled to address convergence issues with the model
summary(fit_genre)

#based on scaled data
fit_genre_sc <- lmer(weekly_sales ~ market_share + price + week_no + top_10 + (1| genre),
                     data = data_scaled, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

#Log-log model

fit_genre_log <- lmer(log(weekly_sales) ~ market_share + log(price) + week_no + top_10 + (1| genre),
                      data = data_1, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# Random effects for fit_genre_sc
ranef(fit_genre_sc)

# Random effects for fit_genre_log
ranef(fit_genre_log)

---------------------------------------Research investigations-----------------------------------------------------------
  
#based on game title
fit_title <- lmer(weekly_sales ~ market_share + price + week_no + top_10 +  genre + (1| game_title),
                    data = data_1, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(fit_title)

#based on publisher
fit_pub <- lmer(weekly_sales ~ market_share + price + week_no + top_10 +  genre + (1| publisher),
                data = data_2, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(fit_pub)

#game and publisher
fit_gp <- lmer(weekly_sales ~ market_share + price + week_no + top_10 + (1| publisher) + (1| genre),
               data = data_2, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(fit_gp)

#without market share
fit_wms <- lmer(weekly_sales ~ price + (week_no * week_no) + top_10 +  genre + (1| publisher) + (1| genre),
                data = data_2, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(fit_wms)

Models_data<-stargazer(fit_title,fit_genre,fit_pub,fit_gp, fit_wms, type='text')

Models_data<-stargazer(fit_title,fit_genre,fit_pub,type='html',out='a.html')

#building models by scaling the variables
fit_title_sc <- lmer(weekly_sales ~ market_share + price + week_no + top_10 +  genre + (1| game_title),
                     data = data_scaled, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


fit_pub_sc <- lmer(weekly_sales ~ market_share + price + week_no + top_10 +  genre + (1| publisher),
                   data = data_scaled_pub, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


Models_scaled_data<-stargazer(fit_title_sc, fit_genre_sc, fit_pub_sc, type = 'text')
Models_scaled_data<-stargazer(fit_title_sc, fit_genre_sc, fit_pub_sc, type = 'html',out='b.html')


# Calculate residuals and fitted values
residuals_title <- residuals(fit_title)
fitted_values_title <- fitted(fit_title)

residuals_genre <- residuals(fit_genre)
fitted_values_genre <- fitted(fit_genre)

residuals_pub <- residuals(fit_pub)
fitted_values_pub <- fitted(fit_pub)

#scaled genre
residuals_genre <- residuals(fit_genre_sc)
fitted_values_genre <- fitted(fit_genre_sc)

residuals_pub <- residuals(fit_pub_sc)
fitted_values_pub <- fitted(fit_pub_sc)

#log-log model
residuals_genre_log <- residuals(fit_genre_log)
fitted_values_genre_log <- fitted(fit_genre_log)

# Create the individual residual vs. fitted plots for each model using ggplot()
plot_title <- ggplot() +
  geom_point(aes(x = fitted_values_title, y = residuals_title)) +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Title Model")

plot_genre <- ggplot() +
  geom_point(aes(x = fitted_values_genre, y = residuals_genre)) +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Genre Model")

plot_pub <- ggplot() +
  geom_point(aes(x = fitted_values_pub, y = residuals_pub)) +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Publisher Model")

# Combine the three plots into one using grid.arrange()
grid.arrange(plot_title, plot_genre, plot_pub, ncol = 3)

# Calculate residuals and fitted values for scaled data models
residuals_title_sc <- residuals(fit_title_sc)
fitted_values_title_sc <- fitted(fit_title_sc)

residuals_genre_sc <- residuals(fit_genre_sc)
fitted_values_genre_sc <- fitted(fit_genre_sc)

residuals_pub_sc <- residuals(fit_pub_sc)
fitted_values_pub_sc <- fitted(fit_pub_sc)

# Create the individual residual vs. fitted plots for each scaled model using ggplot()
plot_title_sc <- ggplot() +
  geom_point(aes(x = fitted_values_title_sc, y = residuals_title_sc)) +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Scaled Title Model")

plot_genre_sc <- ggplot() +
  geom_point(aes(x = fitted_values_genre_sc, y = residuals_genre_sc)) +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Scaled Genre Model")


plot_pub_sc <- ggplot() +
  geom_point(aes(x = fitted_values_pub_sc, y = residuals_pub_sc)) +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Scaled Publisher Model")

# Combine the three plots into one using grid.arrange()
grid.arrange(plot_title_sc, plot_genre_sc, plot_pub_sc, ncol = 3)

#Create the individual residual vs. fitted plots for log-log model
plot_genre_log <- ggplot() +
  geom_point(aes(x = fitted_values_genre_log, y = residuals_genre_log)) +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Log-log Model")

#normality Checks
# Q-Q plot for residuals
qqnorm(residuals_title)
qqline(residuals_title)

qqnorm(residuals_genre)
qqline(residuals_genre)
title("Genre Model")

qqnorm(residuals_pub)
qqline(residuals_pub)
title("Publisher Model")

#Anderson-Darling test for normality
ad.test(residuals_title)
ad.test(residuals_genre)
ad.test(residuals_pub)

qqnorm(residuals_title_sc)
qqline(residuals_title)

qqnorm(residuals_genre_sc)
qqline(residuals_genre_sc)
title("Genre Model")

qqnorm(residuals_pub_sc)
qqline(residuals_pub_sc)
title("Publisher Model")


# Kolmogorov-Smirnov test for residuals_title_genre
ks_genre <- ks.test(residuals_genre, "pnorm", mean(residuals_genre), sd(residuals_genre))
print(ks_genre)

# Kolmogorov-Smirnov test for residuals_genre_sc
ks_genre_sc <- ks.test(residuals_genre_sc, "pnorm", mean(residuals_genre_sc), sd(residuals_genre_sc))
print(ks_genre_sc)


residuals_fit_genre_log <- residuals(fit_genre_log)
fitted_genre_log<- fitted(fit_genre_log)

# Kolmogorov-Smirnov test for residuals_fit_genre_log
ks_genre_log <- ks.test(residuals_fit_genre_log, "pnorm", mean(residuals_fit_genre_log), sd(residuals_fit_genre_log))
print(ks_genre_log)





#cross validation for genre models based on research statement
# Set seed for reproducibility
set.seed(123)

any(is.na(data_1$weekly_sales))
data_imputed <- data_1[!is.na(data_1$weekly_sales), ]

# Split the dataset into training (80%) and testing (20%) sets
train_indices <- createDataPartition(data_imputed$weekly_sales, p = 0.8, list = FALSE)
train_data <- data_scaled[train_indices, ]
test_data <- data_scaled[-train_indices, ]

# Fit the model on the training set
fit_genre <- lmer(weekly_sales ~ market_share + price + week_no + top_10 +  genre + (1| genre),
                  data = train_data, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


fit_genre_sc <- lmer(weekly_sales ~ market_share + price + week_no + top_10 + (1| genre),
                     data = train_data, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


fit_genre_log <- lmer(log(weekly_sales) ~ market_share + log(price) + week_no + top_10 + (1| genre),
                      data = train_data, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


# Calculate predictions on the testing set
test_data$predicted_sales <- predict(fit_genre, newdata = test_data)
test_data$predicted_sales <- predict(fit_genre_sc, newdata = test_data)
test_data$predicted_sales <- predict(fit_genre_log, newdata = test_data)

# Evaluate the performance using mean squared error (MSE) and R-squared
mse <- mean((test_data$weekly_sales - test_data$predicted_sales)^2)
cat("Mean Squared Error:", mse, "\n")

r_squared <- 1 - mse / var(test_data$weekly_sales)
cat("R-squared:", r_squared)

a<-rnorm(1000)
b<-ks.test(a,residuals_genre_sc)
