from collections import defaultdict

import matplotlib.pyplot as plt
import csv


class Plotter:
    def __init__(self, filename):
        self.filename = filename
        self.colors = ['b', 'g', 'r', 'c', 'm', 'y', 'k']

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

    def plot_n_lines(self, group_by, title, x, y, labels):
        xs = defaultdict(list)
        ys = defaultdict(list)
        with open(self.filename, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                xs[row[group_by]].append(float(row[x]))
                ys[row[group_by]].append(float(row[y]))

        for i, group in enumerate(xs.keys()):
            plt.plot(xs[group], ys[group], label=labels[i], color=self.colors[i])

        plt.title(title)
        plt.xlabel(x)
        plt.ylabel(y)
        plt.legend()
        plt.show()
