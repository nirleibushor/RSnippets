### import and process the data ###
data = read.csv("d2.csv")

len = length(data$ImportsTotal)
import = c()
for (i in 1 : len){
  import = c(import, as.numeric(gsub(",", "", levels(data$ImportsTotal)[i], fixed = TRUE)))
}

export = c()
for (i in 1 : len){
  export = c(export, as.numeric(gsub(",", "", levels(data$ExportsTotal)[i], fixed = TRUE)))
}

### transform data to biliards ###
import = import / 1000
export = export / 1000

### basic visualization of the data ###
min = min(min(export), min(import))
max = max(max(export), max(import))

hist_breakpoints = seq(from = min, to = max, length.out = 10)
dist = hist_breakpoints[2] - hist_breakpoints[1]
for (i in 1:2){
  hist_breakpoints = c(hist_breakpoints, hist_breakpoints[length(hist_breakpoints)] + dist)
  hist_breakpoints = c(hist_breakpoints[1] - dist, hist_breakpoints)  
}

hist(import, breaks = hist_breakpoints, ylim = c(0, 30))
hist(export, breaks = hist_breakpoints, ylim = c(0, 30))

plot(import, export)

### salaries - comparison data ###
salData = read.csv("CEO salaries.csv")
plot(salData$Age, salData$Salary)
##################################


### create linear model of the data ###
model = lm(export ~ import)
abline(model)
summary(model)

### inspect nois in data and clean model ###
residSd = sd(model$residuals)
model$standardResiduals = model$residuals / residSd
sds = 1.5

isNoisy = function (standardResList) {
  return(abs(model$standardResiduals) > sds)
}

noisySamples = which(isNoisy(model$standardResiduals))
cleanSamples = which(! isNoisy(model$standardResiduals))

data$colors[isNoisy(model$standardResiduals)] = "red"
data$colors[! isNoisy(model$standardResiduals)] = "black"

plot(import, export, col = data$colors)

### write noisy data to a file in order to include in report ###
noisyData = cbind(data$Period[noisySamples], import[noisySamples], export[noisySamples], model$standardResiduals[noisySamples])
write.csv(noisyData, file = "~/courses huji/stats applications/Rwork/noisyData.csv")

### final plot ###
cleanImport = import[cleanSamples]
cleanExport = export[cleanSamples]
cleanModel = lm(cleanExport ~ cleanImport)
abline(cleanModel, col = 'blue')
legend("top", legend = c("model with noise", "clean model", "noisy data"), bty = "n",
       lwd = 2, cex = 0.8, col = c("black", "blue", "red"), lty = c(1, 1, NA), pch = c(NA, NA, 1))
summary(cleanModel)
