data <- read.csv("./data/clear_amazon.csv")

data = data[,!(names(data) %in% c("date"))]
data['decade'] <- (data['year'] %/% 10) * 10

data.region <- with(data,
                    aggregate(x = number,
                              by = list(state, month),
                              FUN = mean
                              )
                    )

data.decade <- with(data,
                    aggregate(x = number,
                              by = list(state, decade),
                              FUN = sum
                    )
)



# ANÁLISE FATORIAL

model <- aov(number ~ year,
             data=data)

replications(number ~ month*state,
    data=data)

library(multcomp)
mcp.manuf <- glht(model, linfct = mcp(month = "Tukey"))
plot(confint(mcp.manuf),
     cex.axis   = 1.2,
     cex        = 2)

shapiro.test(model$residuals[1:5000])

interfac <- with(data,
                 interaction(month, state))
which.max(tapply(data$number, interfac, mean))
interfac <- relevel(interfac, ref = "Agosto.Pará")

model2 <- aov(number ~ interfac,
              data = data)
mcp.inter <- glht(model2,
                  linfct = mcp(interfac = "Dunnett"))

par(mar = c(5,12,4,2))
plot(confint(mcp.inter),
     cex.axis   = 1.2,
     cex        = 2)

shapiro.test(model2$residuals[1:5000])

#####################################################

library(dplyr)

group_by(data, month) %>%
    summarise(
        count = n(),
        mean = mean(number, na.rm = TRUE),
        sd = sd(number, na.rm = TRUE),
        median = median(number, na.rm = TRUE),
        IQR = IQR(number, na.rm = TRUE)
    )

data.month <- with(data,
                    aggregate(x = number,
                              by = list(month, year),
                              FUN = sum
                    )
)

m <- kruskal.test(number ~ month,
                  data=data)

pairwise.wilcox.test(data$number, data$month,
                     p.adjust.method = "BH")


library("ggpubr")
ggboxplot(data, x = "month", y = "number",
          color = "month",
          ylab = "Ocorrencias", xlab = "Mes")

ggline(data, x = "month", y = "number",
       add = c("mean_se", "jitter"),
       ylab = "Ocorrencias", xlab = "Mes")

############################################################

library(dplyr)

group_by(data, year) %>%
    summarise(
        count = n(),
        mean = mean(number, na.rm = TRUE),
        sd = sd(number, na.rm = TRUE),
        median = median(number, na.rm = TRUE),
        IQR = IQR(number, na.rm = TRUE)
    )


m.year <- kruskal.test(number ~ year,
                  data=data)

pairwise.wilcox.test(data$number, data$year,
                     p.adjust.method = "BH")

pairwise.wilcox.test(data$number, data$year,
                     p.adjust.method = "BH",
                     alternative="less")

pairwise.wilcox.test(data$number, data$year,
                     p.adjust.method = "BH",
                     alternative="greater")

library("ggpubr")
ggboxplot(data, x = "year", y = "number",
          color = "year",
          ylab = "Ocorrencias", xlab = "Abo")

ggline(data, x = "year", y = "number",
       add = c("mean_se", "jitter"),
       ylab = "Ocorrencias", xlab = "Ano")
