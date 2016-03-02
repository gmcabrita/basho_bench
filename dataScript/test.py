#!/usr/bin/env python3

import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt

if __name__ == "__main__":
    data = [1, 2, 3, 4, 5]

    gs1 = gridspec.GridSpec(3, 3)
    gs1.update(wspace=0.025, hspace=0.05)

    fig = plt.figure()
    fig.suptitle("Title for whole figure", fontsize=16)
    ax = plt.subplot(gs1[0])
    ax.set_title("Title for first plot")
    ax.plot(data)

    ax = plt.subplot(gs1[1])
    ax.set_title("Title for second plot")
    ax.axes.yaxis.set_ticklabels([])
    ax.plot(data)

    ax = plt.subplot(gs1[2])
    ax.set_title("Title for second plot")
    ax.axes.yaxis.set_ticklabels([])
    ax.plot(data)

    ax = plt.subplot("334")
    ax.axes.xaxis.set_ticklabels([])
    ax.plot(data)

    ax = plt.subplot("335")
    ax.axes.xaxis.set_ticklabels([])
    ax.plot(data)

    ax = plt.subplot("336")
    ax.axes.xaxis.set_ticklabels([])
    ax.plot(data)

    ax = plt.subplot("337")
    ax.axes.xaxis.set_ticklabels([])
    ax.plot(data)

    ax = plt.subplot("338")
    ax.plot(data)

    ax = plt.subplot("339")
    ax.plot(data)

    plt.show()
