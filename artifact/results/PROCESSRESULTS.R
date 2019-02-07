library(dplyr)
library(ggplot2)
library(scales)
library(colorspace)
library(RColorBrewer)
library(xtable)
options("scipen"=100, "digits"=0)

res = list()
res <- read.csv("data.csv", header=TRUE, stringsAsFactors=FALSE)
configurations = c('boolector', 'cvc4', 'q3b', 'z3')
labels = c(boolector = 'Boolector', cvc4 = 'CVC4', q3b = 'Q3B', z3 = 'Z3')

timeout <- 1200

res[['trivial']] <- TRUE

for (c in configurations)
{
  res[[paste(c, 'solved', sep='.')]] <- res[[paste(c, 'result', sep='.')]] == "sat" | res[[paste(c, 'result', sep='.')]] == "unsat"
  #res[[paste(c, 'cputime', sep='.')]][!res[[paste(c, 'solved', sep='.')]]] <- timeout
  #res[[paste(c, 'walltime', sep='.')]][!res[[paste(c, 'solved', sep='.')]]] <- timeout
  res[['trivial']] <- res[['trivial']] & res[[paste(c, 'cputime', sep='.')]] < 0.1
}

res %>%
    mutate(sat = ((boolector.result == "sat") | (cvc4.result == "sat") | (z3.result == "sat") |  (q3b.result == "sat")),
           unsat = ((boolector.result == "unsat") | (cvc4.result == "unsat") | (z3.result == "unsat") |  (q3b.result == "unsat"))) %>%
    filter(sat & unsat) %>%
    select(benchmark, boolector.result, cvc4.result, z3.result, q3b.result)

getTable <- function()
{
  counts <- list()

  for (c in configurations)
  {
    counts[[c]] <- count_(res, paste(c, 'result', sep='.'))
    colnames(counts[[c]]) <- c('result', c)
  }

  return(Reduce(function(...) merge(..., all=TRUE), counts))
}

getTable()

table <- res %>%
    group_by(family) %>%
    summarise(total = sum(n()),
              boolector = sum(boolector.solved),
              cvc4 = sum(cvc4.solved),
              q3b = sum(q3b.solved),
              z3 = sum(z3.solved),
              maxSolved = max(boolector, cvc4, q3b, z3))

table <- rbind(table, data.frame(family='Total',
   total = sum(table$total),
   boolector = sum(table$boolector),
   cvc4 = sum(table$cvc4),
   q3b = sum(table$q3b),
   z3 = sum(table$z3),
   maxSolved = max(sum(table$boolector), sum(table$cvc4), sum(table$q3b), sum(table$z3))))

table %>% select(family, total, boolector, cvc4, q3b, z3)

maxTable <- transmute(table,
      Family = family,
      Total = total,
      Boolector = ifelse(boolector == maxSolved, sprintf("\\textbf{%d}", boolector), boolector),
      CVC4 = ifelse(cvc4 == maxSolved, sprintf("\\textbf{%d}", cvc4), cvc4),
      Q3B = ifelse(q3b == maxSolved, sprintf("\\textbf{%d}", q3b), q3b),
      Z3 = ifelse(z3 == maxSolved, sprintf("\\textbf{%d}", z3), z3)
)

    xt <- xtable(maxTable, type = "latex", label="tbl:solved",
      caption = "For each solver and benchmark family, the table shows the number of benchmarks from the given family solved by the given solver. The column \\emph{Total} shows the total number of benchmarks in the given family.")
    align(xt) <- c('l', 'l', 'r', 'r', 'r', 'r', 'r')
    print(xt,
          file = "tables/solved.tex", include.rownames=FALSE, booktabs=TRUE, hline.after = c(-1, 0, nrow(table)-1, nrow(table)), table.placement="tbp",
          sanitize.text.function=function(x) x)

satTable <- res %>%
    filter(z3.result == 'sat' | boolector.result == 'sat' | cvc4.result == 'sat' | q3b.result == 'sat') %>%
    group_by(family) %>%
    summarise(total = sum(n()),
              boolector = sum(boolector.solved),
              cvc4 = sum(cvc4.solved),
              q3b = sum(q3b.solved),
              z3 = sum(z3.solved),
              maxSolved = max(boolector, cvc4, q3b, z3))

satTable <- rbind(satTable, data.frame(family='Total',
   total = sum(satTable$total),
   boolector = sum(satTable$boolector),
   cvc4 = sum(satTable$cvc4),
   q3b = sum(satTable$q3b),
   z3 = sum(satTable$z3),
   maxSolved = max(sum(satTable$boolector), sum(satTable$cvc4), sum(satTable$q3b), sum(satTable$z3))))

satTable %>% select(family, total, boolector, cvc4, q3b, z3)

maxSatTable <- transmute(satTable,
      Family = family,
      Total = total,
      Boolector = ifelse(boolector == maxSolved, sprintf("\\textbf{%d}", boolector), boolector),
      CVC4 = ifelse(cvc4 == maxSolved, sprintf("\\textbf{%d}", cvc4), cvc4),
      Q3B = ifelse(q3b == maxSolved, sprintf("\\textbf{%d}", q3b), q3b),
      Z3 = ifelse(z3 == maxSolved, sprintf("\\textbf{%d}", z3), z3)
)

    xt <- xtable(maxSatTable, type = "latex", label="tbl:satSolved",
      caption = "For each solver and benchmark family, the table shows the number of \\emph{satisfiable} benchmarks solved from this benchmark family by the solver.")
    align(xt) <- c('l', 'l', 'r', 'r', 'r', 'r', 'r')
    print(xt,
          file = "tables/satSolved.tex", include.rownames=FALSE, booktabs=TRUE, hline.after = c(-1, 0, nrow(satTable)-1, nrow(satTable)),table.placement="tbp",
          sanitize.text.function=function(x) x)

unsatTable <- res %>%
    filter(z3.result == 'unsat' | boolector.result == 'unsat' | cvc4.result == 'unsat' | q3b.result == 'unsat') %>%
    group_by(family) %>%
    summarise(total = sum(n()),
              boolector = sum(boolector.solved),
              cvc4 = sum(cvc4.solved),
              q3b = sum(q3b.solved),
              z3 = sum(z3.solved),
              maxSolved = max(boolector, cvc4, q3b, z3))

unsatTable <- rbind(unsatTable, data.frame(family='Total',
   total = sum(unsatTable$total),
   boolector = sum(unsatTable$boolector),
   cvc4 = sum(unsatTable$cvc4),
   q3b = sum(unsatTable$q3b),
   z3 = sum(unsatTable$z3),
   maxSolved = max(sum(unsatTable$boolector), sum(unsatTable$cvc4), sum(unsatTable$q3b), sum(unsatTable$z3))))

unsatTable %>% select(family, total, boolector, cvc4, q3b, z3)

maxUnsatTable <- transmute(unsatTable,
      Family = family,
      Total = total,
      Boolector = ifelse(boolector == maxSolved, sprintf("\\textbf{%d}", boolector), boolector),
      CVC4 = ifelse(cvc4 == maxSolved, sprintf("\\textbf{%d}", cvc4), cvc4),
      Q3B = ifelse(q3b == maxSolved, sprintf("\\textbf{%d}", q3b), q3b),
      Z3 = ifelse(z3 == maxSolved, sprintf("\\textbf{%d}", z3), z3)
)

    xt <- xtable(maxUnsatTable, type = "latex", label="tbl:unsatSolved",
      caption = "For each solver and benchmark family, the table shows the number of \\emph{unsatisfiable} benchmarks solved from this benchmark family by the solver.")
    align(xt) <- c('l', 'l', 'r', 'r', 'r', 'r', 'r')
    print(xt,
          file = "tables/unsatSolved.tex", include.rownames=FALSE, booktabs=TRUE, hline.after = c(-1, 0, nrow(unsatTable)-1, nrow(unsatTable)), table.placement="tbp",
          sanitize.text.function=function(x) x)

z3Unique <- res %>%
    filter(z3.solved & !boolector.solved & !cvc4.solved & !q3b.solved) %>%
    select(family, benchmark) %>%
    group_by(family) %>%
    summarise(count = n()) %>%
    rename(Z3 = count)

boolectorUnique <- res %>%
  filter(!z3.solved & boolector.solved & !cvc4.solved & !q3b.solved) %>%
  select(family, benchmark) %>%
  group_by(family) %>%
  summarise(count = n()) %>%
  rename(Boolector = count)

cvc4Unique <- res %>%
  filter(!z3.solved & !boolector.solved & cvc4.solved & !q3b.solved) %>%
  select(family, benchmark) %>%
  group_by(family) %>%
  summarise(count = n()) %>%
  rename(CVC4 = count)

q3bUnique <- res %>%
  filter(!z3.solved & !boolector.solved & !cvc4.solved & q3b.solved) %>%
  select(family, benchmark) %>%
  group_by(family) %>%
  summarise(count = n()) %>%
  rename(Q3B = count)

uniqSolved <- Reduce(function(...) merge(..., all=TRUE), list(boolectorUnique, cvc4Unique, q3bUnique, z3Unique))
uniqSolved[is.na(uniqSolved)] <- 0
uniqSolved

xt <- xtable(uniqSolved, type = "latex", label="tbl:uniquelySolved",
  caption = "For each solver and benchmark family, the table shows the number of benchmarks solved only by the given solver.")
align(xt) <- c('l', 'l', 'r', 'r', 'r', 'r')
print(xt,
    file = "tables/uniqueSolved.tex", include.rownames=FALSE, booktabs=TRUE, table.placement="tbp",
    sanitize.text.function=function(x) x,
    NA.string="0")

uniqSolvedSum <- uniqSolved %>%
                 summarize(Boolector = as.integer(sum(Boolector)),
                         CVC4 = as.integer(sum(CVC4)),
                         Q3B = as.integer(sum(Q3B)),
                         Z3 = as.integer(sum(Z3)))

uniqSolvedSum <- as.data.frame(t(uniqSolvedSum))
colnames(uniqSolvedSum) <- c("Uniquely solved")
uniqSolvedSum

res %>%
  filter(!z3.solved & !boolector.solved & !cvc4.solved & !q3b.solved) %>%
  select(family, benchmark) %>%
  group_by(family) %>%
  summarise(count = n())

firstIsBetter <- function(c1, c2)
{
  c1Solved <- res[[paste(c1, 'solved', sep='.')]]
  c2Solved <- res[[paste(c2, 'solved', sep='.')]]

  onlyC1Solved <- c1Solved & !(c2Solved)
  return(onlyC1Solved)
}

formulasFirstIsBetter <- function(c1, c2)
{
  return(res[firstIsBetter(c1, c2)])
}

compareConfigurations <- function(c1, c2)
{
  return(sum(firstIsBetter(c1, c2)))
}

crossTable <- function()
{
  results <- c()
  for (c1 in configurations)
  {
    for (c2 in configurations)
    {
      results <- c(results, compareConfigurations(c1, c2))
    }
  }

  results.table <- matrix(results, ncol=4,byrow=TRUE)
  colnames(results.table) <- labels
  rownames(results.table) <- labels
  out <- as.data.frame(results.table)
  return(out)
}

table <- crossTable()
table <- merge(table, uniqSolvedSum, by='row.names')
colnames(table) <- c("", "Boolector", "CVC4", "Q3B", "Z3", "Uniquely solved")
table

xt <- xtable(table, type = "latex", label="tbl:cross",
             caption = "The table shows cross-comparison of solved benchmarks by all pairs of the solvers. Each cell shows the number of benchmarks that were solved by the solver in the corresponding row, but not by the solver by the corresponding column. The column \\emph{Uniquely solved} shows the number of benchmarks that were solved only by the given solver.")
align(xt) <- "llrrrr|r"
print(xt,
      file = "tables/cross.tex", include.rownames=FALSE, booktabs=TRUE, table.placement="tbp")

quantilePlot <- function(onlyTrivial = FALSE)
{
    num <- length(configurations)

    data <- res

    if (onlyTrivial)
    {
        data <- filter(data, trivial == FALSE)
    }

    ordered = list()
    for (c in configurations)
    {
        ordered[[c]] = sort(data[[paste(c, 'cputime', sep='.')]][data[[paste(c, 'solved', sep='.')]]])
    }

    plot(c(0, nrow(data)), c(0.001, timeout), log='y', xlab=if (onlyTrivial) 'Solved non-trivial formulas' else "Solved formulas", ylab='CPU time (s)', frame.plot=TRUE, type='n', yaxt="n")
    axis(2, at = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
         labels = c(expression(paste("10"^"-3")),
                    expression(paste("10"^"-2")),
                    expression(paste("10"^"-1")),
                    "1",
                    "10",
                    expression(paste("10"^"2")),
                    expression(paste("10"^"3"))))

    colors <- c("blue", "darkgreen", "red", "black", "purple")
    ltys <- c(5,6,4,1,2)
    for (i in seq_along(configurations))
    {
        c <- configurations[i]
        lines(1:length(ordered[[c]]), ordered[[c]], type='s', col=colors[i], lty=ltys[i])
    }

    legend("topleft",
           lty=ltys,
           lwd=rep(2, each=num),
           col=colors,
           legend=labels)
}

quantilePlot()

pdf(file="figures/all_quantile.pdf",width=8,height=4); tryCatch({
quantilePlot(FALSE)
},error=function(e){plot(x=-1:1, y=-1:1, type='n', xlab='', ylab='', axes=FALSE); text(x=0, y=0, labels=e$message, col='red'); paste('ERROR', e$message, sep=' : ')}); dev.off()

nrow(filter(res, trivial))

quantilePlot(TRUE)

pdf(file="figures/all_trivial_quantile.pdf",width=8,height=4); tryCatch({
quantilePlot(TRUE)
},error=function(e){plot(x=-1:1, y=-1:1, type='n', xlab='', ylab='', axes=FALSE); text(x=0, y=0, labels=e$message, col='red'); paste('ERROR', e$message, sep=' : ')}); dev.off()

plotUnsolved <- function(cs)
{
    cs <- rev(cs)
    unsolved = data.frame(family=character(),
                          configuration=character(),
                          stringsAsFactors=TRUE)
    for (c in cs)
    {
        cUnsolved <- res[["family"]][!res[[paste(c, 'solved', sep='.')]]]
        cUnsolved <- data.frame(
            family = cUnsolved,
            configuration = labels[c])
        unsolved <- rbind(unsolved, cUnsolved)
    }

    print(levels(unsolved$family))
    unsolved$family<- factor(unsolved$family, levels=sort(levels(unsolved$family)))
    print(levels(unsolved$family))

    chart.data <- unsolved %>%
                    group_by(family, configuration) %>%
                    summarize(freq = n()) %>%
                    arrange(desc(family)) %>%
                    group_by(configuration) %>%
                    mutate(pos = cumsum(freq) - (0.5 * freq))

    ggplot(data = chart.data, aes(x = configuration, y = freq, fill = family)) +
        geom_bar(stat="identity") +
        coord_flip() +
        geom_text(data=chart.data, aes(x = configuration, y = pos, label = freq), size=3) +
        labs(y = "Number of unsolved benchmarks (less is better)", x = NULL, fill = "Benchmark family") +
        scale_fill_brewer(palette = "Set2")
}

plotUnsolved(c('boolector', 'cvc4', 'q3b', 'z3'))

pdf(file="figures/unsolved.pdf",width=8,height=3); tryCatch({
plotUnsolved(c('boolector', 'cvc4', 'z3', 'q3b'))
},error=function(e){plot(x=-1:1, y=-1:1, type='n', xlab='', ylab='', axes=FALSE); text(x=0, y=0, labels=e$message, col='red'); paste('ERROR', e$message, sep=' : ')}); dev.off()

allSolved <- res %>% filter(boolector.solved & cvc4.solved & q3b.solved & z3.solved)
data <- bind_rows(allSolved %>% transmute(solver = "boolector", walltime = boolector.walltime),
                  allSolved %>% transmute(solver = "cvc4", walltime = cvc4.walltime),
                  allSolved %>% transmute(solver = "q3b", walltime = q3b.walltime),
                  allSolved %>% transmute(solver = "z3", walltime = z3.walltime))

ggplot(data, aes(x = solver, y = walltime, color=solver), log10="y") +
  geom_boxplot() +
  scale_y_log10()

allSolved <- res %>% filter(boolector.solved & cvc4.solved & q3b.solved & z3.solved)
data <- bind_rows(allSolved %>% transmute(solver = "boolector", walltime = boolector.walltime),
                  allSolved %>% transmute(solver = "cvc4", walltime = cvc4.walltime),
                  allSolved %>% transmute(solver = "q3b", walltime = q3b.walltime),
                  allSolved %>% transmute(solver = "z3", walltime = z3.walltime))

ggplot(data, aes(x = solver, y = walltime, color=solver), log10="y") +
  geom_boxplot() +
  scale_y_log10()

pdf(file="figures/boxplot.pdf",width=8,height=5); tryCatch({
allSolved <- res %>% filter(boolector.solved & cvc4.solved & q3b.solved & z3.solved)
data <- bind_rows(allSolved %>% transmute(solver = "boolector", walltime = boolector.walltime),
                  allSolved %>% transmute(solver = "cvc4", walltime = cvc4.walltime),
                  allSolved %>% transmute(solver = "q3b", walltime = q3b.walltime),
                  allSolved %>% transmute(solver = "z3", walltime = z3.walltime))

ggplot(data, aes(x = solver, y = walltime, color=solver), log10="y") +
  geom_boxplot() +
  scale_y_log10()
},error=function(e){plot(x=-1:1, y=-1:1, type='n', xlab='', ylab='', axes=FALSE); text(x=0, y=0, labels=e$message, col='red'); paste('ERROR', e$message, sep=' : ')}); dev.off()
