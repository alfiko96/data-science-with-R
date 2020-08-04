# 1. The committee is interested to know each field of the data collected 
# through descriptive analysis to gain basic insights into the data set and 
# to prepare for further analysis.

insurance <- read.csv(file.choose())
summary(insurance)

# 2.The total value of payment by an insurance company is an important factor 
# be monitored. So the committee has decided to find whether this payment is 
# related to the number of claims and the number of insured policy years. They 
# also want to visualize the results for better understanding. 

cor(insurance$Payment,insurance$Claims)
# 0.9954003 Highly positive correlation
cor(insurance$Payment,insurance$Insured)
# 0.933217 ""

plot(insurance$Claims,insurance$Payment)
plot(insurance$Insured,insurance$Payment)

# 3. The committee wants to figure out the reasons for insurance payment 
# and decrease. So they have decided to find whether distance, location, 
# bonus, make, and insured amount or claims are affecting the payment or 
# all or some of these are affecting it. 

mod <- lm(Payment~., data=insurance)
summary(mod)

# 4. The insurance company is planning to establish a new branch office, 
# so they are interested to find at what location, kilometre, and bonus 
# level their insured amount, claims, and payment gets increased. 
# (Hint: Aggregate Dataset) 


ZoneResult <- apply(insurance[,c(5,6,7)],2, function(x)tapply(x, insurance$Zone, mean))
ZoneResult

KMResult <- apply(insurance[,c(5,6,7)],2, function(x)tapply(x, insurance$Kilometres, mean))
KMResult

BonusResult <- apply(insurance[,c(5,6,7)],2, function(x)tapply(x, insurance$Bonus, mean))
BonusResult

# 5. The committee wants to understand what affects their claim rates so 
# as to decide the right premiums for a certain set of situations. Hence, 
# they need to find whether the insured amount, zone, kilometre, bonus, or 
# make affects the claim rates and to what extent. 

mod_two <- lm(formula=Claims~Insured+Zone+Kilometres+Bonus+Make,data=insurance)
summary(mod_two)



