#reading in dataset
av <- read.csv("C:\\Users\\jmol2\\OneDrive\\Documents\\STAT 5905\\NFL AV.csv", header = T)
View(av)

#removing team and draft number columns; column w/o data; and combine measurement columns
AV <- av[,-c(2:3,9,17:24)]
AV <- AV[-180,]
View(AV)

#change necessary variables to factor variables
AV$School <- factor(AV$School, labels = c("P5","G5","FCS","Other"))
AV$Yrs.at.College <- factor(AV$Yrs.at.College, labels = c("Left Early","4 Years","More than 4","Other to D1"))
AV$First.Start <- factor(AV$First.Start, labels = c("Y1_W1","Y1_Other","Y2_W1","Y2_Other","After_Y2"))
AV$Rd.Drafted <- factor(AV$Rd.Drafted, labels = c("1st","2nd","3rd","4th","5th","6th","7th","UDFA"))

#summary statistics for each variable
summary(AV)

#temporarily change all variables to integer so I can create a correlation matrix
AV[] <- lapply(AV,as.integer)
windows()
par(mfrow = c(1,1))
sjp.corr(AV[,c(2:13)], show.values=FALSE)

#check for possible multicollinearity
car::vif(AV_fit)

#model build
AV_fit <- lm(Total.AV~.-Player-sqrtAV, data = AV)
ols_step_both_p(AV_fit)
plot(ols_step_both_p(AV_fit))

#take the significant variables at the 0.05 level and put them into a MR model
AV_final <- lm(Total.AV~First.Start+Avg.rk.contract+Rd.Drafted+Team.Playoffs.,data = AV)
summary(AV_final)

#residual plots - megaphone shape in Resid. Plot; ends in Normal Q-Q deviate a lot - need to transform
windows()
par(mfrow = c(1,2))
plot(AV_final, which = c(2,1))

#histogram of response variable (Total AV)
windows()
hist(AV$Total.AV,
     main = "5-Year Total AV Histogram",
     xlab = "Total AV",
     col = "dark blue",
     border = "white",
     breaks = c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65),
     freq = TRUE)

#Histogram of Average Rookie Contract (majority of data between $0.5M-$1M)
windows()
hist(av$Avg.rk.contract,
     main = "Average Rookie Contract Histogram",
     xlab = "Average contract (millions of dollars)")

#Exploratory Analysis between response and explanatory variables; as well as between 2 explanatory variables
windows()
plot(AV$Rd.Drafted, AV$Total.AV, xlab="Round drafted", ylab="Total AV", main="Total AV vs Round Drafted")
plot(AV$Position, AV$Total.AV, xlab="Position", ylab="Total AV")
plot(AV$Avg.Pos.Rk, AV$Total.AV)
plot(AV$First.Start, AV$Total.AV, xlab="First Start", ylab="Total AV", main="Total AV vs First Start")
plot(AV$Pos...Dr., AV$Total.AV)
plot(AV$Avg.rk.contract, AV$Total.AV, xlab="Average Rookie Contract($ Millions)", ylab = "Total AV", 
     main = "Total AV vs Avg Rookie Contract")
lineARC <- lm(Total.AV ~ Avg.rk.contract, data = AV)
abline(lineARC)
plot(AV$Avg.Pos.Rk, AV$Pos...Dr., xlab = "Avg Position Rank", ylab = "Position # Drafted",
     main = "Position Number Drafted vs Average Position Rank")
lineAPR <- lm(Pos...Dr. ~ Avg.Pos.Rk, data = AV)
abline(lineAPR)
plot(AV$Rd.Drafted, AV$Avg.rk.contract, xlab="Round Drafted", ylab="Average Rookie Contract ($Millions)",
     main = "Round Drafted vs Avg Rookie Contract")
plot(AV$Rd.Drafted, AV$Pos...Dr., xlab="Round Drafted", ylab="Position # Drafted",
     main = "Position Number Drafted vs Round Drafted")

AV$lnAV <- log(AV$Total.AV) #log transformation doesn't work bc of the 0s.
AV$sqrtAV <- (AV$Total.AV)^0.5 #square root transformation of response variables

#histogram of transformed response variable
hist(AV$sqrtAV)

car::vif(AV_SQRTFinal)

#model build with sqrt(Total AV) as response variables
AV_fitSQRT <- lm(sqrtAV~.-Player-Total.AV, data = AV)
ols_step_both_p(AV_fitSQRT)

#significant variables put into a MR model
AV_SQRTFinal <- lm(sqrtAV ~ First.Start+Avg.rk.contract+Rd.Drafted+Position+Yrs.at.College, data = AV)
summary(AV_SQRTFinal)

#Residual Plots - diamond in residual plot (due to the large number of 0s)
#Normal Q-Q plot looks pretty good though
windows()
par(mfrow = c(1,2))
plot(AV_SQRTFinal, which = c(2,1))


ols_plot_resid_lev(AV_SQRTFinal)

#residual plots for each of the explanatory variables in the final transformed model
fs_lm <- lm(sqrtAV ~ First.Start, data = AV)
summary(fs_lm)
par(mfrow = c(1,2))
plot(fs_lm, which = c(2,1))

arc_lm <- lm(sqrtAV ~ Avg.rk.contract, data = AV)
summary(arc_lm)
par(mfrow = c(1,2))
plot(arc_lm, which = c(2,1))

rd_lm <- lm(sqrtAV ~ Rd.Drafted, data = AV)
summary(rd_lm)
par(mfrow = c(1,2))
plot(rd_lm, which = c(2,1))

pos_lm <- lm(sqrtAV ~ Position, data = AV)
summary(pos_lm)
par(mfrow = c(1,2))
plot(pos_lm, which = c(2,1))

yrs_lm <- lm(sqrtAV ~ Yrs.at.College, data = AV)
summary(yrs_lm)
par(mfrow = c(1,2))
plot(yrs_lm, which = c(2,1))


#Now Looking at relationships between combine measurements
cor(av[,c(17:24)])
sjp.corr(av[,c(17:24)])

#histograms create for variables with a correlation coefficient > |0.7|
par(mfrow = c(1,1))
plot(av$X40..Official., av$Vertical, xlab = "40-yd Dash Time (s)", ylab = "Vertical Jump (in.)",
     main = "Vertical Jump vs 40-yd Dash")
plot(av$X40..Official., av$X20.Yard.Shuttle, xlab = "40-yd Dash Time (s)", ylab = "20-yd Shuttle (s)",
     main = "20-yd Shuttle vs 40-yd Dash")
plot(av$X40..Official., av$X3.Cone, xlab = "40-yd Dash Time (s)", ylab = "3 Cone Drill (s)",
     main = "3 Cone Drill Time vs 40-yd Dash")
plot(av$X3.Cone, av$X20.Yard.Shuttle, xlab = "3 Cone Drill (s)", ylab = "20-yd Shuttle (s)",
     main = "20-yd Shuttle Time vs 3 Cone Drill Time")

#Looking for any possible differences between players who accrued AV versus those with a Total AV of 0
Split_av <- read.csv("C:\\Users\\jmol2\\OneDrive\\Documents\\STAT 5905\\Split AV.csv", header = T)
View(Split_av) #1-330; 331-434

Split_av <- Split_av[-343,]

Split_av$School <- factor(Split_av$School, labels = c("P5","G5","FCS","Other"))
Split_av$Yrs.at.College <- factor(Split_av$Yrs.at.College, labels = c("Left Early","4 Years","More than 4","Other to D1"))
Split_av$First.Start <- factor(Split_av$First.Start, labels = c("Y1_W1","Y1_Other","Y2_W1","Y2_Other","After_Y2"))
Split_av$Rd.Drafted <- factor(Split_av$Rd.Drafted, labels = c("1st","2nd","3rd","4th","5th","6th","7th","UDFA"))

#split performed between nonzero players and zero players
nonz <- Split_av[1:330,]
zero <- Split_av[331:433,]

#Looked at summary statistics and plots between the explanatory variables in the dataset
#Most did not have differences that could show a possible difference between the split datasets
#Position was interesting in that Offensive and Defensive Lineman tended to have a great percentage
#of nonzeroes versus zeros.
#First Start was very eye-popping as the majority of zeros did not make their first start until after year 2
summary(nonz$Position)
summary(zero$Position)

plot(nonz$Position, xlab="Position", ylim=c(0,50))
plot(zero$Position, xlab="Position", ylim=c(0,50))

summary(nonz$School)
summary(zero$School)

plot(nonz$School, xlab="School")
plot(zero$School, xlab="School")

hist(nonz$College.Gms, xlab="# of College Games")
hist(zero$College.Gms, xlab="# of College Games")

plot(nonz$Yrs.at.College, xlab="Years at College")
plot(zero$Yrs.at.College, xlab="Years at College")

summary(nonz$Yrs.at.College)
summary(zero$Yrs.at.College)

hist(nonz$Avg.Pos.Rk, xlab="Average Position Rank")
hist(zero$Avg.Pos.Rk, xlab="Average Position Rank")

plot(nonz$Rd.Drafted, xlab="Round Drafted")
plot(zero$Rd.Drafted, xlab="Round Drafted")

summary(nonz$Rd.Drafted)
summary(zero$Rd.Drafted)

hist(nonz$Pos...Dr., xlab="Position Drafted")
hist(zero$Pos...Dr., xlab="Position Drafted")

hist(nonz$Org.AllPro.at.Pos, xlab="All Pros", breaks = 10)
hist(zero$Org.AllPro.at.Pos, xlab="All Pros", breaks = 10)

hist(nonz$Avg.rk.contract, xlab="Avg Rk Contract")
hist(zero$Avg.rk.contract, xlab="Avg Rk Contract")

summary(nonz$Avg.rk.contract)
summary(zero$Avg.rk.contract)

plot(nonz$First.Start, xlab="First Start")
plot(zero$First.Start, xlab="First Start")

windows()
summary(nonz$First.Start)
summary(zero$First.Start)

hist(nonz$Team.Playoffs., xlab="Yrs Since Playoffs")
hist(zero$Team.Playoffs., xlab="Yrs Since Playoffs")

hist(nonz$Arms, xlab="Arms")
hist(zero$Arms, xlab="Arms")

hist(nonz$Hands, xlab="Hands")
hist(zero$Hands, xlab="Hands")

hist(nonz$Bench.Press, xlab="Reps")
hist(zero$Bench.Press, xlab="Reps")

hist(nonz$X40..Official., xlab="40 yd dash")
hist(zero$X40..Official., xlab="40 yd dash")

hist(nonz$Vertical, xlab="Vertical Jump")
hist(zero$Vertical, xlab="Vertical Jump")

hist(nonz$Broad, xlab="Broad Jump")
hist(zero$Broad, xlab="Broad Jump")

hist(nonz$X3.Cone, xlab="3 Cone Drill")
hist(zero$X3.Cone, xlab="3 Cone Drill")

hist(nonz$X20.Yard.Shuttle, xlab="20 Yard Shuttle")
hist(zero$X20.Yard.Shuttle, xlab="20 Yard Shuttle")

