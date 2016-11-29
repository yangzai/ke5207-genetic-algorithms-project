library(xlsx)
library(GA)

df = read.xlsx('WS1GA-soln.xlsx', sheetName = 'Data')
given = read.xlsx('WS1GA-soln.xlsx', sheetName = 'Solution')

package.count = nrow(df)
df.weight = df$Weight # reduce reference lookup time
weight.avg = mean(df.weight)

fitness = function (ids) {
        weights = sapply(ids, function (id) df.weight[id]) # map id to weights
        stack.matrix = matrix(weights, ncol = 4, byrow = T) # group stack as rows
        stack.avg = apply(stack.matrix, 1, mean)
        spread = sum((stack.avg - weight.avg) ^ 2)
        -spread # negate to minimise
}

given.individual = given$PackageID[!is.na(given$PackageID)]
given.fitness = fitness(given.individual)

ga.result = ga('permutation', fitness, min = 1, max = package.count,
               maxiter = 5000, run = 500, pmutation = 0.2, pcrossover = 0.7, seed = 2)

# constraint - sort by weight in stack
best = unname(summary(ga.result)$solution[1,]) # select the 1st of many solutions
best.matrix = matrix(best, ncol = 4, byrow = T)
best.weight = sapply(best, function (id) df.weight[id])
best.weight.matrix = matrix(best.weight, ncol = 4, byrow = T)
best.sorted = c(apply(cbind(best.matrix, best.weight.matrix), 1, function (v) {
        ids = v[1:4]
        weights = v[5:8]
        ids[order(weights, decreasing = T)] # smaller pos index -> closer to bottom
}))

# output findings
png(filename = "ga_plot.png")
plot(ga.result)

ga.summary = summary(ga.result)
cat('Given fitness: ', given.fitness, '\n')
cat('GA fitness: ', ga.summary$fitness, '\n')
cat('Given fitness / GA fitness: ', given.fitness/ga.summary$fitness, '\n')

cat('Best individual:\n')
print(strwrap(best.sorted))

dev.off()
