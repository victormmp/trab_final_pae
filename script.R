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
                              FUN = mean
                    )
)
