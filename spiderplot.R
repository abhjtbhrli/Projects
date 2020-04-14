install.packages("fmsb")
library(fmsb)


# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
# I have manually entered the player data and created the data frame as shown below:

JerryL <- c(key_passes=0.49,assists=0.09,chances=0.27,crosses=2.37,accurate_crosses=.19,
            good_dribbles=1.19,dribble_rate=0.44,ball_rec_opp_half=0.90,good_challenges=7.34,
            challenge_rate=0.54,good_tackles=2.84,tackle_rate=0.69,interceptions=3.76,
            air_challenges=2.17,aerial_rate=0.16)

GurjinderK <- c(key_passes=0.42,assists=0.05,chances=0.09,crosses=2.32,accurate_crosses=.21,
                good_dribbles=1.30,dribble_rate=0.60,ball_rec_opp_half=1.30,good_challenges=7.76,
                challenge_rate=0.57,good_tackles=1.91,tackle_rate=0.51,interceptions=4.74,
                air_challenges=3.44,aerial_rate=0.67)

# Here are the max and min rows:

max1 <- c(key_passes=1.69,assists=0.25,chances=0.67,crosses=3.6,accurate_crosses=1,
       good_dribbles=3.2,dribble_rate=1,ball_rec_opp_half=1.47,good_challenges=10,
       challenge_rate=1,good_tackles=3.3,tackle_rate=1,interceptions=5.9,
       air_challenges=4.6,aerial_rate=1)

min1 <- c(key_passes=0,assists=0,chances=0,crosses=0,accurate_crosses=0,
          good_dribbles=0,dribble_rate=0,ball_rec_opp_half=0,good_challenges=0,
          challenge_rate=0,good_tackles=0,tackle_rate=0,interceptions=0,
          air_challenges=0,aerial_rate=0)
data1 <- rbind(max1,min1,JerryL,GurjinderK)
data1
radarchart(data1) # This gives an error as data1 isn't a data frame, so we convert it to a data frame
data1 <- as.data.frame(data1)
class(data1)
radarchart(data1)

# Let's make some design changes now that the basic plot is done

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) )
radarchart(data1,axistype = 1,
           pcol = colors_border,pfcol = colors_in,plwd = 4,plty = 1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8)
legend(x=1, y=-0.7, legend = rownames(data1[-c(1,2),]), bty = "n", pch=15 , col=colors_in , text.col = "black", cex=1, pt.cex=3)

# data1[,1]
# I want to rename the column and row names and I'm doing it manually 


colnames(data1)[1] <- "Key Passes"
colnames(data1)
colnames(data1)[2] <- "Assists"
colnames(data1)[3] <- "Chances created"
colnames(data1)[4] <- "Crosses"
colnames(data1)[5] <- "Crossing%"
colnames(data1)[6] <- "Succ. dribbles"
colnames(data1)[7] <- "Dribbles%"
colnames(data1)[8] <- "Opp1/2 recovs."
colnames(data1)[9] <- "Challenges won"
colnames(data1)[10] <- "Challenges%"
colnames(data1)[11] <- "Succ. tackles"
colnames(data1)[12] <- "Tackles%"
colnames(data1)[13] <- "Interceptions"
colnames(data1)[14] <- "Aerial duels"
colnames(data1)[15] <- "Aerials win%"
colnames(data1)

rownames(data1)[3] <- "Jerry L."
rownames(data1)[4] <- "Gurjinder K."

radarchart(data1,axistype = 1,
           pcol = colors_border,pfcol = colors_in,plwd = 4,plty = 1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8)
legend(x=1, y=-0.7, legend = rownames(data1[-c(1,2),]), bty = "n", pch=15 , col=colors_in , text.col = "black", cex=1, pt.cex=3)

