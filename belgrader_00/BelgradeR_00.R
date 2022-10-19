
### --- BelgradeR Meetup #00
### --- Startit center, Belgrade
### --- 19. October 2022.

### --- SETUP
library(tidyverse)
library(ggrepel)
data_dir <- paste0(getwd(), "/_data/")
analytics_dir <- paste0(getwd(), "/_analytics/")
img_dir <- paste0(getwd(), "/_img/")

### --- DATA
# - scrape Available CRAN Packages By Date of Publication
# - URL: https://cran.r-project.org/web/packages/available_packages_by_date.html
url_source <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"
source_page <- xml2::read_html(url_source)
tables <- rvest::html_table(source_page, fill = TRUE)
r_packages <- tables[[1]]
rm(list = c("tables", "source_page"))

### --- ANALYSIS

### --- CRAN History

# - popular packages
selected_packages <- c("dplyr",
                       "tidyr",
                       "rvest",
                       "xml2",
                       "tidyverse",
                       "tidymodels",
                       "tidytext",
                       "data.table", 
                       "ggplot2", 
                       "caret",
                       "mlr3",
                       "plotly", 
                       "rbokeh",
                       "htmlwidgets",
                       "rmarkdown",
                       "shiny",
                       "stringr", 
                       "purrr",
                       "readr")
# - select packages
wpckg <- which(r_packages$Package %in% selected_packages)
r_packages$Package[wpckg]
length(wpckg) == length(selected_packages)
selected_packages_frame <- r_packages[wpckg, ]
# - store r_packages
write.csv(r_packages,
          paste0(data_dir, "r_packages_", Sys.Date(), ".csv"))
# - aggregate per Date
r_packages_agg <- r_packages %>% 
  dplyr::select(Date) %>% 
  dplyr::group_by(Date) %>% 
  dplyr::summarise(Count = n()) %>% 
  dplyr::left_join(
    dplyr::select(selected_packages_frame,
                  Date, Package),
    by = "Date") %>%
  dplyr::mutate(Cumulative = cumsum(Count))
# - add labels
r_packages_agg$Label <- ifelse(is.na(r_packages_agg$Package), 
                                     "",
                                     paste0(r_packages_agg$Package, 
                                            " (", 
                                            r_packages_agg$Cumulative,
                                            ")"))
# - store r_packages_agg
write.csv(r_packages_agg,
          paste0(analytics_dir, "r_packages_agg.csv"))

# - ggplot
ggplot(r_packages_agg, aes(x = Date,
                           y = Cumulative,
                           label = Label)) +
  geom_path(size = .25, group = 1, color = "darkblue") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  geom_label_repel(size = 2.5, 
                  max.overlaps = 1000) +
  ggtitle("History of CRAN") + 
  ylab("Cumulative Package Count") + 
  theme(axis.text.x = element_text(angle = 90, size = 6)) + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5, size = 15)) + 
  theme(plot.background =  element_rect(fill = "white")) + 
  theme(axis.ticks = element_blank())
# - scale this ^^ ggplot
r_packages_agg_recent <- r_packages_agg %>% 
  dplyr::filter(grepl("2021|2022", Date))
ggplot(r_packages_agg_recent, aes(x = Date,
                           y = Cumulative,
                           label = Label)) +
  geom_path(size = .25, group = 1, color = "darkblue") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  geom_label_repel(size = 2.5, 
                   max.overlaps = 1000) +
  ggtitle("History of CRAN (recent)") + 
  ylab("Cumulative Package Count") + 
  theme(axis.text.x = element_text(angle = 90, size = 6)) + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5, size = 15)) + 
  theme(plot.background =  element_rect(fill = "white")) + 
  theme(axis.ticks = element_blank())

### --- Topics

# - Generalized Linear Models
num_pckgs <- sum(grepl("GLM|GLM|Generalized Linear Model",
                       r_packages$Title,
                       ignore.case = TRUE))
print(num_pckgs)
wpckgs <- which(grepl("GLM|GLM|Generalized Linear Model",
                      r_packages$Title,
                      ignore.case = TRUE))
print(paste(sort(r_packages$Package[wpckgs]), 
            collapse = ", "))
selected_packages <- r_packages$Package[wpckgs]
selected_packages_frame <- r_packages[wpckgs, ]

# - function to visualize topics
visualize_pckgs <- function(cran_history, 
                            selected, 
                            title,
                            color) {
  
  # - aggregate per Date
  r_packages_agg <- cran_history %>% 
    dplyr::select(Date) %>% 
    dplyr::group_by(Date) %>% 
    dplyr::summarise(Count = n()) %>% 
    dplyr::left_join(
      dplyr::select(selected,
                    Date, Package),
      by = "Date") %>%
    dplyr::mutate(Cumulative = cumsum(Count))
  # - add labels
  r_packages_agg$Label <- ifelse(is.na(r_packages_agg$Package), 
                                 "",
                                 paste0(r_packages_agg$Package, 
                                        " (", 
                                        r_packages_agg$Cumulative,
                                        ")"))
  # - add color
  r_packages_agg$Color <- ifelse(r_packages_agg$Label == "", 
                                 "white", color)
  # - ggplot
  return(
    ggplot(r_packages_agg, aes(x = Date,
                               y = Cumulative)) +
      geom_point(color = r_packages_agg$Color, size = 1) +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
      ggtitle(title) +
      ylab("Cumulative Package Count") +
      theme(axis.text.x = element_text(angle = 90, size = 6)) +
      theme(panel.border = element_blank()) +
      theme(plot.title = element_text(hjust = .5, size = 12)) +
      theme(plot.background =  element_rect(fill = "white")) +
      theme(axis.ticks = element_blank())
  )
}


# - Text and NLP
query <- "text|natural language|NLP"
num_pckgs <- sum(grepl(query, 
                       r_packages$Title, 
                       ignore.case = TRUE))
print(num_pckgs)
wpckgs <- which(grepl(query,
                      r_packages$Title,
                      ignore.case = TRUE))
print(paste(sort(r_packages$Package[wpckgs]), 
            collapse = ", "))
selected_packages <- r_packages$Package[wpckgs]
selected_packages_frame <- r_packages[wpckgs, ]
chart <- visualize_pckgs(cran_history = r_packages,
                         selected = selected_packages_frame,
                         title = "CRAN History: Text and NLP", 
                         color = "darkred")
print(chart)

# - Neural Networks and Deep Learning
query <- "neural network|deep learning"
num_pckgs <- sum(grepl(query, 
                       r_packages$Title, 
                       ignore.case = TRUE))
print(num_pckgs)
wpckgs <- which(grepl(query,
                      r_packages$Title,
                      ignore.case = TRUE))
print(paste(sort(r_packages$Package[wpckgs]), 
            collapse = ", "))
selected_packages <- r_packages$Package[wpckgs]
selected_packages_frame <- r_packages[wpckgs, ]
chart <- visualize_pckgs(cran_history = r_packages,
                         selected = selected_packages_frame,
                         title = "CRAN History: NNs and Deep Learning",
                         color = "violet")
print(chart)

# - Graphics and Visualization
query <- "visual|graphics"
num_pckgs <- sum(grepl(query, 
                       r_packages$Title, 
                       ignore.case = TRUE))
print(num_pckgs)
wpckgs <- which(grepl(query,
                      r_packages$Title,
                      ignore.case = TRUE))
print(paste(sort(r_packages$Package[wpckgs]), 
            collapse = ", "))
selected_packages <- r_packages$Package[wpckgs]
selected_packages_frame <- r_packages[wpckgs, ]
chart <- visualize_pckgs(cran_history = r_packages,
                         selected = selected_packages_frame,
                         title = "CRAN History: Graphics and Visualization",
                         color = "darkorange")
print(chart)

# - Clustering
query <- "cluster"
num_pckgs <- sum(grepl(query, 
                       r_packages$Title, 
                       ignore.case = TRUE))
print(num_pckgs)
wpckgs <- which(grepl(query,
                      r_packages$Title,
                      ignore.case = TRUE))
print(paste(sort(r_packages$Package[wpckgs]), 
            collapse = ", "))
selected_packages <- r_packages$Package[wpckgs]
selected_packages_frame <- r_packages[wpckgs, ]
chart <- visualize_pckgs(cran_history = r_packages,
                         selected = selected_packages_frame,
                         title = "CRAN History: Clustering", 
                         color = "darkblue")
print(chart)

# - Time Series
query <- "time-series|time series"
num_pckgs <- sum(grepl(query, 
                       r_packages$Title, 
                       ignore.case = TRUE))
print(num_pckgs)
wpckgs <- which(grepl(query,
                      r_packages$Title,
                      ignore.case = TRUE))
print(paste(sort(r_packages$Package[wpckgs]), 
            collapse = ", "))
selected_packages <- r_packages$Package[wpckgs]
selected_packages_frame <- r_packages[wpckgs, ]
chart <- visualize_pckgs(cran_history = r_packages,
                         selected = selected_packages_frame,
                         title = "CRAN History: Time Series", 
                         color = "darkgreen")
print(chart)

# - Bayesian
query <- "bayes"
num_pckgs <- sum(grepl(query, 
                       r_packages$Title, 
                       ignore.case = TRUE))
print(num_pckgs)
wpckgs <- which(grepl(query,
                      r_packages$Title,
                      ignore.case = TRUE))
print(paste(sort(r_packages$Package[wpckgs]), 
            collapse = ", "))
selected_packages <- r_packages$Package[wpckgs]
selected_packages_frame <- r_packages[wpckgs, ]
chart <- visualize_pckgs(cran_history = r_packages,
                         selected = selected_packages_frame,
                         title = "CRAN History: Bayes", 
                         color = "darkorange")
print(chart)

# - Deployment, Production
query <- "deploy|production"
num_pckgs <- sum(grepl(query, 
                       r_packages$Title, 
                       ignore.case = TRUE))
print(num_pckgs)
wpckgs <- which(grepl(query,
                      r_packages$Title,
                      ignore.case = TRUE))
print(paste(sort(r_packages$Package[wpckgs]), 
            collapse = ", "))
selected_packages <- r_packages$Package[wpckgs]
selected_packages_frame <- r_packages[wpckgs, ]
chart <- visualize_pckgs(cran_history = r_packages,
                         selected = selected_packages_frame,
                         title = "CRAN History: Deployment & Production", 
                         color = "darkred")
print(chart)

### --- Exponential Growth
start_date <- min(r_packages_agg$Date)
end_date <- max(r_packages_agg$Date)
cran_dates <- seq.Date(as.Date(start_date),
                       as.Date(end_date),
                       by = "days")
exp_reg_frame <- data.frame(Date = as.character(cran_dates), 
                            stringsAsFactors = FALSE)
exp_reg_frame <- exp_reg_frame %>% 
  dplyr::left_join(r_packages_agg, by = "Date")
for (i in 2:dim(exp_reg_frame)[1]) {
  if (is.na(exp_reg_frame$Cumulative[i])) {
    exp_reg_frame$Cumulative[i] <- exp_reg_frame$Cumulative[i-1]
  }
}
# - ggplot
ggplot(exp_reg_frame, aes(x = Date,
                          y = Cumulative)) +
  geom_path(size = .25, group = 1, color = "darkblue") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  ggtitle("REAL History of CRAN") + 
  theme(axis.text.x = element_text(angle = 90, size = 6)) + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5, size = 15)) + 
  theme(plot.background =  element_rect(fill = "white")) + 
  theme(axis.ticks = element_blank())
# - exponential regression
exp_reg_frame$index <- 1:dim(exp_reg_frame)[1]
exp_reg_model <- lm(log(Cumulative) ~ index,
                    data = exp_reg_frame)
summary(exp_reg_model)
a <- exp(coefficients(exp_reg_model)[1])
b <- exp(coefficients(exp_reg_model)[2])
exp_reg_frame$exp_reg_prediction <- a*b^exp_reg_frame$index
exp_reg_chart_frame <- exp_reg_frame %>% 
  dplyr::select(index, Cumulative, exp_reg_prediction) %>% 
  tidyr::pivot_longer(cols = c("Cumulative", "exp_reg_prediction"),
                      names_to = "variable",
                      values_to = "value")
ggplot(exp_reg_chart_frame, aes(x = index,
                          y = value,
                          color = variable)) +
  geom_path(size = .75) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  ggtitle("EXPONENTIAL History of CRAN? (Nah...)") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, size = 6)) + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5, size = 15)) + 
  theme(plot.background =  element_rect(fill = "white")) + 
  theme(axis.ticks = element_blank())

