#import matplotlib.pyplot as plt

import numpy as np
import pylab as pl
from scipy import interpolate, signal
import matplotlib.font_manager as fm
import xlrd
from datetime import date, timedelta
import calendar
import XKCDify as xkcd

def get_hour(s):
    l = s.split(':')
    return (float(l[0]) * 60.0 + float(l[1]))/60.0


ax = pl.axes()

city_daylight = xlrd.open_workbook("../data/city_daylight.xls")
#SOURCE: http://aa.usno.navy.mil/data/docs/Dur_OneYear.php

city_weather = xlrd.open_workbook("../data/city_weather.xls")
#SOURCE: http://www.usclimatedata.com/climate/denver/colorado/united-states/usco0501

sh = city_daylight.sheet_by_name("Aggregate")
print sh.name, sh.nrows, sh.ncols

sh_high = city_weather.sheet_by_name("High")
sh_low = city_weather.sheet_by_name("Low")
sh_rain = city_weather.sheet_by_name("Rain")

#print "Cell D30 is", sh.cell_value(rowx=29, colx=3)
#for rx in range(sh.nrows):
#    print sh.row(rx)


#base_dt = date(2016, 1, 1)
#print base_dt

min_hour = 8

x = [(sh.cell_value(i, 0) - 42370.0) for i in range(sh.nrows) if i !=0]
evanston = [get_hour(sh.cell_value(i, 1))-min_hour for i in range(sh.nrows) if i !=0]
salt_lake = [get_hour(sh.cell_value(i, 2))-min_hour for i in range(sh.nrows) if i !=0]
seattle = [get_hour(sh.cell_value(i, 3))-min_hour for i in range(sh.nrows) if i !=0]
santa_cruz = [get_hour(sh.cell_value(i, 4))-min_hour for i in range(sh.nrows) if i !=0]
denver = [get_hour(sh.cell_value(i, 5))-min_hour for i in range(sh.nrows) if i !=0]
boulder = [get_hour(sh.cell_value(i, 6))-min_hour for i in range(sh.nrows) if i !=0]
portland = [get_hour(sh.cell_value(i, 7))-min_hour for i in range(sh.nrows) if i !=0]

#print x
#print evanston
ax.plot(x, evanston, 'r', lw=1, label='Evanston')
ax.plot(x, salt_lake, 'b', lw=1, label='Salt Lake')
ax.plot(x, seattle, 'g', lw=1, label='Seattle')
#ax.plot(x, santa_cruz, 'c', lw=1, label='Santa Cruz')
ax.plot(x, denver, 'm', lw=1, label='Denver')
ax.plot(x, boulder, 'k', lw=1, label='Boulder')
ax.plot(x, portland, 'c', lw=1, label='Portland')

ax.set_title('Total Daylight Hours')
ax.set_xlabel('Date')
ax.set_ylabel('Hours')

ax.legend(loc='upper right')

ax.set_xlim(0, 365)
ax.set_ylim(0, 8)

#XKCDify the axes -- this operates in-place
xkcd.XKCDify(ax, xaxis_loc=0.0, yaxis_loc=0.0,
        xaxis_arrow='+-', yaxis_arrow='+-',
        expand_axes=True, mag=0.8, mline_width=0.7, bg_width=0.7)

pl.savefig('daylight_comparison.png') 
pl.clf()

#### High ####
ax_high = pl.axes()

min_temp = 0


x_high = [list(calendar.month_abbr).index(sh_high.cell_value(i, 0)) for i in range(sh_high.nrows) if i !=0]
evanston_high = [sh_high.cell_value(i, 1)-min_temp for i in range(sh_high.nrows) if i !=0]
seattle_high = [sh_high.cell_value(i, 2)-min_temp for i in range(sh_high.nrows) if i !=0]
salt_lake_high = [sh_high.cell_value(i, 3)-min_temp for i in range(sh_high.nrows) if i !=0]
boulder_high = [sh_high.cell_value(i, 4)-min_temp for i in range(sh_high.nrows) if i !=0]
denver_high = [sh_high.cell_value(i, 5)-min_temp for i in range(sh_high.nrows) if i !=0]
santa_cruz_high = [sh_high.cell_value(i, 6)-min_temp for i in range(sh_high.nrows) if i !=0]
portland_high = [sh_high.cell_value(i, 7)-min_temp for i in range(sh_high.nrows) if i !=0]

#print x
#print evanston
ax_high.plot(x_high, evanston_high, 'r', lw=1, label='Evanston')
ax_high.plot(x_high, salt_lake_high, 'b', lw=1, label='Salt Lake')
ax_high.plot(x_high, seattle_high, 'g', lw=1, label='Seattle')
#ax_high.plot(x_high, santa_cruz_high, 'c', lw=1, label='Santa Cruz')
ax_high.plot(x_high, denver_high, 'm', lw=1, label='Denver')
ax_high.plot(x_high, boulder_high, 'k', lw=1, label='Boulder')
ax_high.plot(x_high, portland_high, 'c', lw=1, label='Portland')

ax_high.set_title('Monthly Average High Temperature')
ax_high.set_xlabel('Date')
ax_high.set_ylabel('Temperature')

ax_high.legend(loc='upper right')

ax_high.set_xlim(0, 12)
ax_high.set_ylim(0, 100)

#XKCDify the axes -- this operates in-place
xkcd.XKCDify(ax_high, xaxis_loc=0.0, yaxis_loc=0.0,
        xaxis_arrow='+-', yaxis_arrow='+-',
        expand_axes=True, mag=0.8, mline_width=0.7, bg_width=0.7)

pl.savefig('high_comparison.png') 
pl.clf()

#### Low ####
ax_low = pl.axes()

min_temp = 0


x_low = [list(calendar.month_abbr).index(sh_low.cell_value(i, 0)) for i in range(sh_low.nrows) if i !=0]
evanston_low = [sh_low.cell_value(i, 1)-min_temp for i in range(sh_low.nrows) if i !=0]
seattle_low = [sh_low.cell_value(i, 2)-min_temp for i in range(sh_low.nrows) if i !=0]
salt_lake_low = [sh_low.cell_value(i, 3)-min_temp for i in range(sh_low.nrows) if i !=0]
boulder_low = [sh_low.cell_value(i, 4)-min_temp for i in range(sh_low.nrows) if i !=0]
denver_low = [sh_low.cell_value(i, 5)-min_temp for i in range(sh_low.nrows) if i !=0]
santa_cruz_low = [sh_low.cell_value(i, 6)-min_temp for i in range(sh_low.nrows) if i !=0]
portland_low = [sh_low.cell_value(i, 7)-min_temp for i in range(sh_low.nrows) if i !=0]

#print x
#print evanston
ax_low.plot(x_low, evanston_low, 'r', lw=1, label='Evanston')
ax_low.plot(x_low, salt_lake_low, 'b', lw=1, label='Salt Lake')
ax_low.plot(x_low, seattle_low, 'g', lw=1, label='Seattle')
#ax_low.plot(x_low, santa_cruz_low, 'c', lw=1, label='Santa Cruz')
ax_low.plot(x_low, denver_low, 'm', lw=1, label='Denver')
ax_low.plot(x_low, boulder_low, 'k', lw=1, label='Boulder')
ax_low.plot(x_low, portland_low, 'c', lw=1, label='Portland')

ax_low.set_title('Monthly Average Low Temperature')
ax_low.set_xlabel('Date')
ax_low.set_ylabel('Temperature')

ax_low.legend(loc='upper right')

ax_low.set_xlim(0, 12)
ax_low.set_ylim(0, 100)

#XKCDify the axes -- this operates in-place
xkcd.XKCDify(ax_low, xaxis_loc=0.0, yaxis_loc=0.0,
        xaxis_arrow='+-', yaxis_arrow='+-',
        expand_axes=True, mag=0.8, mline_width=0.7, bg_width=0.7)

pl.savefig('low_comparison.png') 
pl.clf()



#### Rain ####
ax_rain = pl.axes()

min_rain = 0


x_rain = [list(calendar.month_abbr).index(sh_rain.cell_value(i, 0)) for i in range(sh_rain.nrows) if i !=0]
evanston_rain = [sh_rain.cell_value(i, 1)-min_rain for i in range(sh_rain.nrows) if i !=0]
seattle_rain = [sh_rain.cell_value(i, 2)-min_temp for i in range(sh_rain.nrows) if i !=0]
salt_lake_rain = [sh_rain.cell_value(i, 3)-min_temp for i in range(sh_rain.nrows) if i !=0]
boulder_rain = [sh_rain.cell_value(i, 4)-min_temp for i in range(sh_rain.nrows) if i !=0]
denver_rain = [sh_rain.cell_value(i, 5)-min_temp for i in range(sh_rain.nrows) if i !=0]
santa_cruz_rain = [sh_rain.cell_value(i, 6)-min_temp for i in range(sh_rain.nrows) if i !=0]
portland_rain = [sh_rain.cell_value(i, 7)-min_temp for i in range(sh_rain.nrows) if i !=0]

#print x
#print evanston
ax_rain.plot(x_rain, evanston_rain, 'r', lw=1, label='Evanston')
ax_rain.plot(x_rain, salt_lake_rain, 'b', lw=1, label='Salt Lake')
ax_rain.plot(x_rain, seattle_rain, 'g', lw=1, label='Seattle')
#ax_rain.plot(x_rain, santa_cruz_rain, 'c', lw=1, label='Santa Cruz')
ax_rain.plot(x_rain, denver_rain, 'm', lw=1, label='Denver')
ax_rain.plot(x_rain, boulder_rain, 'k', lw=1, label='Boulder')
ax_rain.plot(x_rain, portland_rain, 'c', lw=1, label='Portland')

ax_rain.set_title('Monthly Average Rainfall')
ax_rain.set_xlabel('Date')
ax_rain.set_ylabel('Rainfall')

ax_rain.legend(loc='upper right')

ax_rain.set_xlim(0, 12)
ax_rain.set_ylim(0, 6)

#XKCDify the axes -- this operates in-place
xkcd.XKCDify(ax_rain, xaxis_loc=0.0, yaxis_loc=0.0,
        xaxis_arrow='+-', yaxis_arrow='+-',
        expand_axes=True, mag=0.8, mline_width=0.7, bg_width=0.7)

pl.savefig('rain_comparison.png') 
pl.clf()

#Crime Rates
#Housing Prices

#Annual Temperature
#Annual Rainfall
#Annual days of sunshine
#Public School quality
#hours of sunlight per day
#energy markets
#phd postdoc positions
#biomedical concentration
#number of people named Ben
#number of people named Elise
#number of people named Walker
#percent of migration vs born there 
#happiness statistics
#distance from san jose vs. distance from Logan
#travel time from san jose vs. travel time from Logan
#proximity to nearest in-n-out
#number of google results for "board game shop ______"
#google trends most searched for terms
#first google image returned for "____ resident"
#city debt level
#diveroce rates
#leading cause of death
#"Green" rating
#age

