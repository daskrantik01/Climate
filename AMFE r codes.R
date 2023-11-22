#loading the required libraries
install.packages("corrplot")
library(readxl)
library(ggplot2)
library(tseries)
library(corrplot)
library(vars)

#Reading the file

GB_OIL_data <- read_xlsx("C:/Users/Pratibha/OneDrive/Desktop/DATA_GB_OIL.xlsx")
GB_OIL_data

# Check if data uploaded is a data frame
if (is.data.frame(GB_OIL_data)) {
  print("Your data is already in a data frame.")
} else {
  print("Your data is not in a data frame.")
}

# Use sapply to get the class (data type) of each variable in the data frame
variable_types <- sapply(GB_OIL_data, class)

# Print the variable types
print(variable_types)

# Re-check the class of Date
print(class(GB_OIL_data$Date))

# Check for missing values in the entire data frame
missing_values <- sapply(GB_OIL_data, function(x) sum(is.na(x)))

# Print the number of missing values for each variable
print(missing_values)

#Creatimg Timeplots

#Timeplot for Crude oil prices
ggplot(GB_OIL_data, aes(x = Date, y = EU_brentOIL)) +
  geom_line() +
  labs(title = "EU_brentOIL Time Plot",
       x = "Date",
       y = "Brent Oil Prices") +
  theme_minimal()

# Timeplot for NAV
ggplot(GB_OIL_data, aes(x = Date, y = GB_OIL_data$`NAV`)) +
  geom_line() +
  labs(title = "Net Asset Value Time Plot",
       x = "Date",
       y = "NAV") +
  theme_minimal()


# Timeplot for NAC change%
ggplot(GB_OIL_data, aes(x = Date, y = GB_OIL_data$`NAV Change%` )) +
  geom_line() +
  labs(title = "NAV Change% Time Plot",
       x = "Date",
       y = "NAV Change%") +
  theme_minimal()

#Stationarity check


# Perform ADF test on NAV% change
adf_result_NAV <- adf.test(GB_OIL_data$NAV, alternative = "stationary")
# Print the ADF test results
print(adf_result_NAV)

# Perform ADF test on NAV% change
adf_result_NAV1 <- adf.test(GB_OIL_data$`NAV Change%`, alternative = "stationary")
# Print the ADF test results
print(adf_result_NAV1)

# Perform ADF test on EU_brent_OIL close prices
adf_result_OIL <- adf.test(GB_OIL_data$EU_brentOIL , alternative = "stationary")
# Print the ADF test results
print(adf_result_OIL)

# Calculate the first differences and store them in a new column
GB_OIL_data$EU_brentOIL1 <- c(NA, diff(GB_OIL_data$EU_brentOIL))

# The first value in the new column will be NA since there's no previous value to calculate the difference.

# Now, view the updated data frame
View(GB_OIL_data)


# Perform ADF test on the first difference of EU_brent_OIL close prices
adf_result_diffOIL <- adf.test(GB_OIL_data$EU_brentOIL1[-1], alternative = "stationary")
# Print the ADF test results
print(adf_result_diffOIL)

# Calculate returns from first differenced prices (excluding NA)
EU_brentOIL_returns <- GB_OIL_data$EU_brentOIL1[-1] / GB_OIL_data$EU_brentOIL[-1]

# Add NA as the first value in the returns column to match the structure
EU_brentOIL_returns <- c(NA, EU_brentOIL_returns)

# Add the new column to the data frame
GB_OIL_data$EU_brentOIL_returns <- EU_brentOIL_returns

# Now, view the updated data frame
View(GB_OIL_data)

#Now perform the ADF test on the EU_Brent_OIL close price returns
adf_result_returns_OIL <- adf.test(GB_OIL_data$EU_brentOIL_returns[-1], alternative = "stationary")
# Print the ADF test results
print(adf_result_returns_OIL)

#plotting the EU_brent_OIL close price returns
ggplot(GB_OIL_data, aes(x = Date, y = GB_OIL_data$EU_brentOIL_returns)) +
  geom_line() +
  labs(title = "EU Brent OIL close price returns Time Plot",
       x = "Date",
       y = "Close price returns") +
  theme_minimal()

# Create a data frame with the variables of interest
data_for_correlation <- GB_OIL_data[c("EU_brentOIL", "NAV")]

# Calculate the correlation matrix
cor_matrix <- cor(data_for_correlation, use = "complete.obs")

# Create the correlation plot
corrplot(cor_matrix, method = "color")

#creating two time series. 
#DData is stationary of degree 1. Data is non-stationary. 
DData= ts(GB_OIL_data[, c("NAV Change%", "EU_brentOIL_returns")], start = 1, frequency = 1)
Data= ts(GB_OIL_data[,c("NAV", "EU_brentOIL")], start=1, frequency=1)

# Handle missing values in Data (assuming you're using listwise deletion)
Data_clean <- na.omit(Data)
DData_clean<- na.omit(DData)
#Johansen cointegration test
result <- ca.jo(Data_clean, type = "trace", K=3, ecdet = "none",spec="transitory")
summary(result)

# Use VARselect on the cleaned data
lag_selection <- VARselect(DData_clean, lag.max = 6, type = "both")$selection

print(lag_selection)

# Estimate the VAR model using the 'VAR' function from the 'vars' package
var_model <- VAR(DData_clean, p =  1)

# Print the summary of the VAR model
summary(var_model)

print(var_model)
residuals <- residuals(var_model)
print(residuals)
View(var_model)

# Assuming you have already estimated your VAR model and named it 'var_model'
# Replace 'var_model' with your actual VAR model object
# Example:
# var_model <- VAR(data, p = lag_order)

# Extract the residuals from the VAR model
residuals <- residuals(var_model)

# Split the residuals into two variables
residuals_var1 <- residuals[, "NAV.Change."]
residuals_var2 <- residuals[, "EU_brentOIL_returns"]
library(lmtest)

# Calculate the Durbin-Watson statistic manually for Variable 1
durbin_watson_var1 <- function(residuals) {
  n <- length(residuals)
  diff_res <- diff(residuals)
  num <- sum(diff_res^2)
  den <- sum(residuals^2)
  DW <- num / den
  return(DW)
}

# Calculate the Durbin-Watson statistic manually for Variable 2
durbin_watson_var2 <- function(residuals) {
  n <- length(residuals)
  diff_res <- diff(residuals)
  num <- sum(diff_res^2)
  den <- sum(residuals^2)
  DW <- num / den
  return(DW)
}

# Calculate the Durbin-Watson test for both variables
dw_test_var1 <- durbin_watson_var1(residuals_var1)
dw_test_var2 <- durbin_watson_var2(residuals_var2)

# Print the Durbin-Watson test results
cat("Durbin-Watson Test for NAV % change", dw_test_var1, "\n")
cat("Durbin-Watson Test for EU_brentOIL_returns", dw_test_var2, "\n")

# Run the Jarque-Bera test for Variable 1
jb_test_var1 <- jarque.bera.test(residuals_var1)

# Run the Jarque-Bera test for Variable 2
jb_test_var2 <- jarque.bera.test(residuals_var2)

# Print the Jarque-Bera test results
cat("Jarque-Bera Test for Variable 1:\n")
print(jb_test_var1)

cat("Jarque-Bera Test for Variable 2:\n")
print(jb_test_var2)

#granger for var
granger_test_result1 <- causality(var_model, cause = "NAV.Change.")
granger_test_result2 <- causality(var_model, cause = "EU_brentOIL_returns")

granger_test_result1
granger_test_result2


# Assuming your VAR model includes two variables named "EU_brentOIL_returns" and "NAV.change."
ir1 <- irf(var_model, n.ahead = 20, impulse = "EU_brentOIL_returns", response = c("NAV.Change.", "EU_brentOIL_returns"),runs = 500)

plot(ir1)

ir2 <- irf(var_model, n.ahead = 20, impulse = "NAV.Change.", response = c("NAV.Change.", "EU_brentOIL_returns"),runs = 500)

plot(ir2)


#Error Variance Decomposition
result <- fevd(var_model, n.ahead = 50)
print(result)

# Plot the FEVD for the first variable
plot(result, which = "NAV.Change.")
plot(result, which="EU_brentOIL_returns")



