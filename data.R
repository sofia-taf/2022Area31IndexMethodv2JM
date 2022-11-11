## Preprocess data, write TAF data tables

## Before: catch.csv, index.csv, priors.csv (bootstrap/data)
## After:  catch_by_stock.png, catch_index.csv, catch_relative.png,
##         catch_total.png, driors.pdf, input.rds (data)

library(TAF)
taf.library(SOFIA)
suppressMessages(library(dplyr))  # filter, group_by, mutate, summarise, ungroup
library(ggplot2)  # aes, geom_line, geom_point, ggplot, ggsave, ggtitle
library(sraplus)  # plot_driors
library(tidyr)    # nest, pivot_longer

mkdir("data")

## Read catch data, convert to tibble (long format)
catch <- read.taf("bootstrap/data/catch.csv")
catch$Total <- NULL  # not used, not a stock
catch <- pivot_longer(catch, !Year, "stock", values_to="capture")
names(catch) <- tolower(names(catch))

## Plot catch
catch %>%
  group_by(year) %>%
  summarise(total_capture=sum(capture)) %>%
  ggplot(aes(year, total_capture)) +
  geom_line()
ggsave("data/catch_total.png", width=12, height=6)

## Plot catch by stock
catch %>%
  ggplot(aes(year, capture, color=stock)) +
  geom_line(show.legend=FALSE) +
  geom_point()
ggsave("data/catch_by_stock.png", width=12, height=6)

## Select stocks with min 10 years of non-zero catches...
viable_stocks <- catch %>%
  group_by(stock) %>%
  summarise(n_pos_catch=sum(capture > 0.1)) %>%
  filter(n_pos_catch > 10)

## ...and discard zero-catch years at the beginning or end of series
catch <- catch %>%
  filter(stock %in% viable_stocks$stock) %>%
  group_by(stock) %>%
  filter(year >= min(year[capture > 0.1]),
         year <= max(year[capture > 0.1]))

## Plot relative catch
catch %>%
  group_by(stock) %>%
  mutate(capture = capture / max(capture)) %>%
  ggplot(aes(year, capture, group=stock)) +
  geom_point()
ggsave("data/catch_relative.png", width=12, height=6)

## Read index data, combine catch and index data
index <- read.taf("bootstrap/data/index.csv")
index <- pivot_longer(index, !Year, "stock", values_to="index")
names(index) <- tolower(names(index))
catch_index <- addIndex(catch, index, same.index=FALSE)

## Create nested tibble with 'data' column (catch and index)
stocks <- catch_index %>%
  group_by(stock) %>%
  nest() %>%
  ungroup()

## Read priors data, add as driors to stocks object
priors <- read.taf("bootstrap/data/priors.csv")

## Custom function to add driors
customDriors <- function(stocks, priors, same.priors)
{
  driors <- list()
  for (i in seq_len(nrow(stocks))) {
    p <- if (same.priors)
           match("All", priors$stock) else match(stocks$stock[i], priors$stock)
    driors[[i]] <- format_driors(
      taxa = stocks$stock[i],
      catch = stocks$data[[i]]$capture,
      years = stocks$data[[i]]$year,
      index = na.omit(stocks$data[[i]])$index,
      index_years = na.omit(stocks$data[[i]])$year,
      # Prior section
      shape_prior = 2,
      b_ref_type = "k",
      initial_state = priors$initial_state[p],
      initial_state_cv = priors$initial_state_cv[p],
      # terminal_state = priors$terminal_state[p],
      # terminal_state_cv = priors$terminal_state_cv[p],
      growth_rate_prior = NA,
      growth_rate_prior_cv = 0.2)
  }
  stocks$driors <- driors
  stocks
}

stocks <- customDriors(stocks, priors, same.priors=TRUE)

## Plot driors
pdf("data/driors.pdf")
for(i in seq_len(nrow(stocks)))
{
  suppressWarnings(print(plot_driors(stocks$driors[[i]]) +
                         ggtitle(stocks$stock[i])))
}
dev.off()

## Export stocks and catch_index
saveRDS(stocks, "data/input.rds")
write.taf(catch_index, dir="data")
