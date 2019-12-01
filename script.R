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

model <- aov(number ~ month*state,
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

