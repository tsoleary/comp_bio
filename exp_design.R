# Experimental Design in Comp Bio


# regression analysis for continuous independant and dependant variables
# lecture notes from compbio

# create the random variables
n <- 50 # number of observations 
var_a <- runif(n) # 50 rand uniforms
var_b <- runif(n) # dependant variable
var_c <- 5.5 + var_a*10 # creates a noisy 
id <- seq_len(n) 

# create a dataframe
reg_df <- data.frame(id, var_a, var_b, var_c)

# regression model
reg_model <- lm(var_b ~ var_a, data = reg_df)
summary(reg_model)
