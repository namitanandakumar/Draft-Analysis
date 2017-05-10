dat = read.csv('~/Downloads/nhlplayers.csv')

lorenz <- function(team) {
  dat1 <- subset(dat, Team == team)
  dat1 <- subset(dat1, Pos != "G")
  dat1 <- dat1[order(-dat1$TOI),][1:21,]
  dat1 <- dat1[order(dat1$Goals),]
  rownames(dat1) <- NULL
  dat1$CumPlayer <- as.numeric(rownames(dat1))/nrow(dat1)
  zerorow <- data.frame(team,"NA",0,0,0,0,"NA",0,0)
  colnames(zerorow) <- colnames(dat1)
  dat1 <- data.frame(rbind(zerorow, dat1))
  dat1$CumGoals <- cumsum(dat1$Goals)/sum(dat1$Goals)
  dat1$Area <- ((dat1$CumGoals + c(0, dat1$CumGoals[1:(nrow(dat1)-1)]))/2)*dat1$CumPlayer[2]
  coef <- 1-2*sum(dat1$Area)
  return(dat1)
}

gini <- function(team) {
  dat1 <- lorenz(team)
  coef <- 1-2*sum(dat1$Area)
  return(coef)
}

df <- data.frame(Team = levels(dat$Team))

for (i in 1:nrow(df)) {
  df$Gini[i] <- gini(df[,1][i])
}

df <- df[order(-df$Gini),]
rownames(df) <- NULL
df
