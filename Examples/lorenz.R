### Download the data.
dat = read.csv('~/Downloads/nhlplayers.csv')

### Since I like doing my data visualizations in R, I've decided to define 2 functions rather than write one big loop.
### The lorenz function will give me cumulative percentiles for any team, which I can then graph easily if I want.
### The gini function gives me Gini coefficients for each team, similar to what I've done in the Python script.

### This function acts on any team name.
lorenz <- function(team) {
  ### Subset on that team.
  dat1 <- subset(dat, Team == team)
  ### Take out goalies.
  dat1 <- subset(dat1, Pos != "G")
  ### Sort by TOI to find the top 21 skaters.
  dat1 <- dat1[order(-dat1$TOI),][1:21,]
  ### Sort from least to most goals scored.
  dat1 <- dat1[order(dat1$Goals),]
  ### Reset the index so we can use it in the next step.
  rownames(dat1) <- NULL
  ### Use the index (which goes from 1 to 21) to get the cumulative player percentile.
  dat1$CumPlayer <- as.numeric(rownames(dat1))/nrow(dat1)
  ### Create a zero row so that the Lorenz curve starts at 0.
  zerorow <- data.frame(team,"NA",0,0,0,0,"NA",0,0)
  colnames(zerorow) <- colnames(dat1)
  ### Add the zero row to the dataframe.
  dat1 <- data.frame(rbind(zerorow, dat1))
  ### Calculate cumulative goals percentile.
  dat1$CumGoals <- cumsum(dat1$Goals)/sum(dat1$Goals)
  ### Calculate area under the curve using some basic calculus.
  dat1$Area <- ((dat1$CumGoals + c(0, dat1$CumGoals[1:(nrow(dat1)-1)]))/2)*dat1$CumPlayer[2]
  ### Return the whole data set up till this point so you can graph whatever you want.
  return(dat1)
}

### Also acts on any team name.
gini <- function(team) {
  ### Use the manipulated data from the lorenz function.
  dat1 <- lorenz(team)
  ### Calculate the team's Gini coefficient.
  coef <- 1-2*sum(dat1$Area)
  return(coef)
}

### Create a dataframe with team names.
df <- data.frame(Team = levels(dat$Team))

### Create a column in the dataframe with the corresponding Gini coefficients.
for (i in 1:nrow(df)) {
  df$Gini[i] <- gini(df[,1][i])
}

### Order from highest to lowest coefficient.
df <- df[order(-df$Gini),]
rownames(df) <- NULL
### Check your results and see if they match across all programs.
df
