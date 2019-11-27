import csv
import pandas as pd

with open("amazon.csv") as file:
    reader = csv.reader(file)

    data = pd.read_csv(file)
    print(data.head(5))
