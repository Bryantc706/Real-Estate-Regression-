# Load necessary libraries
library(ggplot2)
library(corrplot)

#Please import both csv before running the code

# Load the dataset
data <- Housing_Prices_24

# Set correct column names
colnames(data) <- c("Id", "SalePrice", "LotArea", "TotalBsmtSF", "1stFlrSF", "2ndFlrSF", 
                    "FullBath", "HalfBath", "BedroomAbvGr", "TotRmsAbvGrd", "Fireplaces", 
                    "GarageCars", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", 
                    "3SsnPorch", "ScreenPorch", "PoolArea", "LotConfig", "BldgType", 
                    "YearBuilt", "YearRemodAdd", "Foundation", "CentralAir", "KitchenQual", 
                    "PavedDrive", "YrSold")

# Convert necessary columns to numeric if they're read as characters
data$Id <- as.numeric(data$Id)
data$SalePrice <- as.numeric(data$SalePrice)
data$LotArea <- as.numeric(data$LotArea)
data$TotalBsmtSF <- as.numeric(data$TotalBsmtSF)
data$`1stFlrSF` <- as.numeric(data$`1stFlrSF`)
data$`2ndFlrSF` <- as.numeric(data$`2ndFlrSF`)
data$FullBath <- as.numeric(data$FullBath)
data$HalfBath <- as.numeric(data$HalfBath)
data$BedroomAbvGr <- as.numeric(data$BedroomAbvGr)
data$TotRmsAbvGrd <- as.numeric(data$TotRmsAbvGrd)
data$Fireplaces <- as.numeric(data$Fireplaces)
data$GarageCars <- as.numeric(data$GarageCars)
data$WoodDeckSF <- as.numeric(data$WoodDeckSF)
data$OpenPorchSF <- as.numeric(data$OpenPorchSF)
data$EnclosedPorch <- as.numeric(data$EnclosedPorch)
data$`3SsnPorch` <- as.numeric(data$`3SsnPorch`)
data$ScreenPorch <- as.numeric(data$ScreenPorch)
data$PoolArea <- as.numeric(data$PoolArea)
data$LotConfig <- as.factor(data$LotConfig)
data$BldgType <- as.factor(data$BldgType)
data$YearBuilt <- as.numeric(data$YearBuilt)
data$YearRemodAdd <- as.numeric(data$YearRemodAdd)
data$Foundation <- as.factor(data$Foundation)
data$CentralAir <- as.factor(data$CentralAir)
data$KitchenQual <- as.factor(data$KitchenQual)
data$PavedDrive <- as.factor(data$PavedDrive)
data$YrSold <- as.numeric(data$YrSold)

# Create a linear regression model with all variables
linear_model <- lm(SalePrice ~ ., data = data)

# Display the summary of the linear regression model
summary(linear_model)

plot(linear_model)

# Finding the minimum and maximum values in the SalePrice column
min_saleprice <- min(data1$SalePrice, na.rm = TRUE)  # Use na.rm = TRUE to remove NA values
max_saleprice <- max(data1$SalePrice, na.rm = TRUE)

# Display the results
print(min_saleprice)
print(max_saleprice)

#Used model to Find the VIF
model <- lm(SalePrice ~ ., data = data)

# Calculate VIF for all predictors in the model
vif_values <- vif(model)

# Display the VIF values
print(vif_values)



# Create a refined linear regression model with only significant variables
model2 <- lm(SalePrice ~ LotArea + TotalBsmtSF + `1stFlrSF` + `2ndFlrSF` + Fireplaces + GarageCars + WoodDeckSF + 
               ScreenPorch + BldgType + YearBuilt + YearRemodAdd + 
               KitchenQual, data = data)

# Create a refined linear regression model with only significant variables and the log method 
log_model2 <- lm(log(SalePrice) ~ LotArea + TotalBsmtSF + `1stFlrSF` + `2ndFlrSF` + Fireplaces + GarageCars + WoodDeckSF + 
               ScreenPorch + BldgType + YearBuilt + YearRemodAdd + 
               KitchenQual, data = data)

#Displayed the summary and plot for the log model
summary(log_model2)

plot(log_model2)


# Display the summary and plot of the refined linear regression model
summary(model2)

plot(model2)


# Calculate VIF for all predictors in the model
vif_values <- vif(model2)

# Display the VIF values
print(vif_values)


# Box plot of SalePrice against KitchenQualGd
data1 <- subset(data, KitchenQual %in% c("Ex", "Fa", "Gd", "TA"))
data1$KitchenQual <- droplevels(data1$KitchenQual)
boxplot(SalePrice ~ KitchenQual, data = data1, main = "SalePrice vs KitchenQuality", 
        xlab = "Kitchen Quality ", ylab = "Sale Price", col = "lightpink")


# Box plot of SalePrice against Bldgtype
data1$BldgType <- droplevels(data1$BldgType)
boxplot(SalePrice ~ BldgType, data = data1, main = "SalePrice vs Buliding Type", 
        xlab = "Buliding Type", ylab = "Sale Price", col = "lightblue")

