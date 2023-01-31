
##       Module 16 Challenge, PART 1: Linear Regression to Predict MPG        ##

# Import dplyr package (tidyverse)
library(tidyverse)

# Import MechaCar_mpg dataset
mechacar_mpg <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)

# Perform multiple linear regression on all six variables of MechaCar data
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance +
          AWD, data=mechacar_mpg)

# Determine the p-value and the r-squared value for the linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
                  ground_clearance + AWD, data=mechacar_mpg))



##  Module 16 Challenge, PART 2: Create Visualizations for the Trip Analysis  ##

# Import Suspension_Coil dataset
suspension_coil <- read.csv('Suspension_Coil.csv',check.names = F,
                              stringsAsFactors = F)

# The suspension coil’s PSI continuous variable across all manufacturing lots
total_summary <- suspension_coil %>% summarize(Mean=mean(PSI),Median=median(PSI),
                                     Variance=var(PSI), SD=sd(PSI))

# The following PSI metrics for each lot: mean, median, variance, and S.D.
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% 
                                   summarize(Mean=mean(PSI),Median=median(PSI),
                                   Variance=var(PSI), SD=sd(PSI), .groups = 'keep')



##          Module 16 Challenge, PART 3: T-Tests on Suspension Coils          ##

# 1. Determine if the Suspension Coil PSI across all manufacturing lots is statistically
#     different from the population mean of 1,500 pounds per square inch

# Visualize the distribution of PSI readings across all lots (normally distributed?)
ggplot(suspension_coil,aes(x=PSI)) + geom_density()

# Run one sample t-test for coil PSI (all lots) against pop. mean of 1500 psi
t.test(suspension_coil$PSI, mu=1500)


# 2. Determine if the PSI for each manufacturing lot is statistically different 
#     from the population mean of 1,500 pounds per square inch

# LOT 1: Run one sample t-test for coil PSI against pop. mean of 1500 psi
lot1_psi <- subset(suspension_coil, Manufacturing_Lot == "Lot1")
t.test(lot1_psi$PSI, mu=1500)

# LOT 2: Run one sample t-test for coil PSI against pop. mean of 1500 psi
lot2_psi <- subset(suspension_coil, Manufacturing_Lot == "Lot2")
t.test(lot2_psi$PSI, mu=1500)

# LOT 3: Run one sample t-test for coil PSI against pop. mean of 1500 psi
lot3_psi <- subset(suspension_coil, Manufacturing_Lot == "Lot3")
t.test(lot3_psi$PSI, mu=1500)


