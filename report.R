## Prepare plots and tables for report

## Before: sofia20_proportions.csv (bootstrap/data), results.rds (model),
##         current_status.csv, stock_timeseries.csv (output)
## After:  bbmsy.png, status_by_year.png, status_sofia.png, status_sraplus.png,
##         stock_biomass_index.pdf, stock_posterior.pdf,
##         stock_timeseries.pdf (report)

library(TAF)
taf.library(SOFIA)
suppressMessages(library(egg))  # ggarrange
library(ggplot2)  # geom_hline, geom_line, ggplot, ggsave, ggtitle
library(sraplus)  # plot_prior_posterior, plot_sraplus

mkdir("report")

stocks <- readRDS("model/results.rds")
levels <- c("Underfished", "Fully fished", "Overfished")

## Plot biomass index
pdf("report/stock_biomass_index.pdf")
for(i in seq_len(nrow(stocks)))
{
  x <- stocks$driors[[i]]$index_years
  y <- stocks$driors[[i]]$index
  plot(x, y, ylim=lim(y), main=stocks$stock[i], xlab="", ylab="Biomass index",
       type="l")
}
dev.off()

## Barplots of stock status
taf.png("status_sraplus")
current_status <- read.taf("output/current_status.csv")
current_status$status <- ordered(current_status$status, levels=levels)
barplot(prop.table(table(current_status$status)), col=c(3,7,2), ylim=0:1,
        xlab="Category", ylab="Proportion")
dev.off()
taf.png("status_sofia")
results_sofia <- read.taf("bootstrap/data/sofia20_proportions.csv")
results_sofia$Category <- ordered(results_sofia$Category, levels=levels)
barplot(Proportion~Category, results_sofia, col=c(3,7,2), ylim=0:1)
dev.off()

## Plot posteriors and time series for each stock
pdf("report/stock_posterior.pdf")
for(i in seq_len(nrow(stocks)))
{
  p <- plot_prior_posterior(stocks$sraplus_fit[[i]], stocks$driors[[i]])
  suppressWarnings(print(p + ggtitle(stocks$stock[i])))
}
dev.off()
pdf("report/stock_timeseries.pdf")
for(i in seq_len(nrow(stocks)))
  print(plot_sraplus(stocks$sraplus_fit[[i]]) + ggtitle(stocks$stock[i]))
dev.off()

## Plot time series for each stock
stock.timeseries <- read.taf("output/stock_timeseries.csv")
taf.png("status_by_year")
p1 <- plotCat(stock.timeseries, method="effEdepP", cats=3, type="count")
p2 <- plotCat(stock.timeseries, method="effEdepP", cats=3, type="stock")
ggarrange(p1, p2, ncol=1)
dev.off()

## Overlay B/Bmsy time series of all stocks in a single plot
ggplot(stock.timeseries, aes(x=year, y=bbmsy, colour=stock, group=stock)) +
  geom_line(show.legend=TRUE) +
  geom_hline(yintercept=0.8, linetype="dashed", color="red", linewidth=2) +
  geom_hline(yintercept=1.2, linetype="dashed", color="green", linewidth=2)
ggsave("report/bbmsy.png", width=12, height=6)
