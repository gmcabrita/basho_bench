#!/usr/bin/env python3

import matplotlib.pyplot as plt
import matplotlib.ticker as ticker

## the following two functions override the default behavior or twiny()
def make_patch_spines_invisible(ax):
    ax.set_frame_on(True)
    ax.patch.set_visible(False)
    for sp in ax.spines.values():
        sp.set_visible(False)

def make_spine_invisible(ax, direction):
    if direction in ["right", "left"]:
        ax.yaxis.set_ticks_position(direction)
        ax.yaxis.set_label_position(direction)
    elif direction in ["top", "bottom"]:
        ax.xaxis.set_ticks_position(direction)
        ax.xaxis.set_label_position(direction)
    else:
        raise ValueError("Unknown Direction : %s" % (direction,))

    ax.spines[direction].set_visible(True)

data = (('A',0.01),('A',0.02),('B',0.10),('B',0.20)) # fake data

fig = plt.figure(1)
sb = fig.add_subplot(111)
sb.xaxis.set_major_locator(ticker.FixedLocator([0,1,2,3]))

sb.plot([i[1] for i in data],"*",markersize=10)
sb.set_xlabel("dose")

plt.subplots_adjust(bottom=0.17) # make room on bottom

par2 = sb.twiny() # create a second axes
par2.spines["bottom"].set_position(("axes", -.1)) # move it down

## override the default behavior for a twiny axis
make_patch_spines_invisible(par2) 
make_spine_invisible(par2, "bottom")
par2.set_xlabel("treatment")

par2.plot([i[1] for i in data],"*",markersize=10) #redraw to put twiny on same scale
par2.xaxis.set_major_locator(ticker.FixedLocator([0,1,2,3]))
par2.xaxis.set_ticklabels([i[0] for i in data])

plt.savefig('test.png')
