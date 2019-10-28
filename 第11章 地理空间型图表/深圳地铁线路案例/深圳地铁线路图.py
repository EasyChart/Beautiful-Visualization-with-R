# -*- coding: utf-8 -*-
"""
Created on Fri Nov  9 17:11:20 2018

@author: Jie Zhang
"""

import pandas as pd
import numpy as np
from plotnine import *
from plotnine.data import *

file = open('F:/1.R语言数据可视化之美/第9章 地理空间/深圳地铁/ShenzhenSubway_Station.csv')
mydata_station=pd.read_csv(file)

file = open('F:/1.R语言数据可视化之美/第9章 地理空间/深圳地铁/ShenzhenSubway_Path.csv')
mydata_Path=pd.read_csv(file)

mydata_Path['Subway_Num']=pd.Categorical(mydata_Path['Subway_Num'])
mydata_station['Subway_Num']=pd.Categorical(mydata_station['Subway_Num'])

base_plot=(ggplot()+
  geom_path (mydata_Path,aes(x='x',y='y',group='Subway_Num',colour='Subway_Num'), size=1)+
  geom_point(mydata_station,aes(x='x',y='y',group='Subway_Num',colour='Subway_Num'),shape='o',size=3,fill="white")+
  xlab("long")+
  ylab("lat"))

print(base_plot)


#--------------------------------------------------------------------------------
Price_max=np.max(mydata_station['Unit_Price']) # 89503.92558
Price_min=np.min(mydata_station['Unit_Price'])

mydata_station['Unit_Price2']=pd.cut(mydata_station['Unit_Price'],
              bins=[0,30000,40000,50000,60000,70000,80000,90000],
              labels=[" <=30000","30000~40000","40000~50000","50000~60000","60000~70000","70000~80000","80000~90000"])

base_plot=(ggplot()+
  geom_path (mydata_Path,aes(x='x',y='y',group='Subway_Num',colour='Subway_Num'), size=1)+
  geom_point(mydata_station,aes(x='x',y='y',group='Subway_Num2',size='Unit_Price2',fill='Unit_Price2'),shape='o')+
  #guides(fill = guide_legend((title="二手房均价(平方米)")),
  #       size = guide_legend((title="二手房均价(平方米)")))+
  theme_void()+
  theme(
    figure_size = (11, 11),
    dpi = 58
))
print(base_plot)
