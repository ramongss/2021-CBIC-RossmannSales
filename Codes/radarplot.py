# Libraries
import matplotlib.pyplot as plt
import pandas as pd
from math import pi
plt.style.use('science')

# Set data
df = pd.DataFrame({
'group': ['(a)', '(b)', '(c)'],
'A': [495.5726, 1116.350, 1230.139],
'B': [495.7883, 1061.467, 1181.240],
'C': [891.6400, 1164.107, 1291.679],
'D': [1221.2699, 1434.346, 1514.695],
'E': [1224.9977, 1509.619, 1523.884],
'F': [1281.8825, 1426.602, 1553.102]
})

# ------- PART 1: Define a function that do a plot for one line of the dataset!

def make_spider(row, title, color):

  # number of variable
  categories=list(df)[1:]
  N = len(categories)
  
  # What will be the angle of each axis in the plot? (we divide the plot / number of variable)
  angles = [n / float(N) * 2 * pi for n in range(N)]
  angles += angles[:1]
  
  # Initialise the spider plot
  ax = plt.subplot(1,3,row+1, polar=True)
  
  # If you want the first axis to be on top:
  ax.set_theta_offset(pi / 2)
  ax.set_theta_direction(-1)
  
  # Draw one axe per variable + add labels labels yet
  plt.xticks(angles[:-1], categories, size=15)
  
  # Draw ylabels
  ax.set_rlabel_position(0)
  plt.yticks(color="grey", size=12)
  plt.ylim(400,1700)
  
  # Ind1
  values=df.loc[row].drop('group').values.flatten().tolist()
  values += values[:1]
  ax.plot(angles, values, color=color, linewidth=2, linestyle='solid')
  ax.fill(angles, values, color=color, alpha=0.4)
  
  # Add a title
  plt.title(title, size=15, y=1.2)

# ------- PART 2: Apply to all individuals
# initialize the figure
my_dpi=96
plt.figure(figsize=(1000/my_dpi, 1000/my_dpi), dpi=my_dpi)

# Create a color palette:
# my_palette = plt.cm.get_cmap("Set1", len(df.index))
my_palette = plt.cm.get_cmap("Set1", 9)

# Loop to plot
for row in range(0, len(df.index)):
  make_spider(row=row, title=df['group'][row], color=my_palette(row))

plt.savefig('radarplot.pdf',dpi=1200, bbox_inches='tight', pad_inches=0)
