import matplotlib.pyplot as plt
import csv


class Plotter:
    def __init__(self, filename):
        self.filename = filename

    def plot(self, x, y, title):
        xs = []
        ys = []
        with open(self.filename, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                xs.append(float(row[x]))
                ys.append(float(row[y]))

        plt.plot(xs, ys, 'r-o')
        plt.title(title)
        plt.xlabel(x)
        plt.ylabel(y)
        plt.show()
