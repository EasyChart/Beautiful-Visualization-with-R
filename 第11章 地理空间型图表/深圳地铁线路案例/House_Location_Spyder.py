# -*- coding: utf-8 -*-
"""
Created on Tue Feb 27 15:33:54 2018

@author: Jie Zhang
"""

from urllib.parse import quote  
from urllib import request  
import json  
import csv
import time
import re

amap_web_key = 'bc76d5878a6a96afb43beab66126ecc7'#'3e65b5abc153f1aac68ec95b3a468e76'   #
poi_search_url = "http://restapi.amap.com/v3/place/text"  
poi_boundary_url = "https://ditu.amap.com/detail/get/detail"  
import pandas as pd

#根据城市名称和分类关键字获取poi数据  
def getpois(cityname, keywords):      
    try:
        result = getpoi_page(cityname, keywords, 1)  
        result = json.loads(result)  # 将字符串转换为json  
        pois = result['pois']
        return pois[0] 
    except:
        return '未获取经纬度'
  
#将返回的poi数据装入集合返回  
def hand(poilist, result):  
    #result = json.loads(result)  # 将字符串转换为json  
    pois = result['pois']  
    for i in range(len(pois)) :  
        poilist.append(pois[i])  
        
#单页获取pois  
def getpoi_page(cityname, keywords, page):  
    req_url = poi_search_url + "?key=" + amap_web_key + '&extensions=all&keywords=' + quote(keywords) + '&city=' + quote(cityname) + '&citylimit=true' + '&offset=25' + '&page=' + str(page) + '&output=json'  
    data = ''  
    with request.urlopen(req_url) as f:  
        data = f.read()  
        data = data.decode('utf-8')  
    return data 


#获取城市分类数据  

cityname = "深圳"  
Housename = "港逸豪庭"  
pois = getpois(cityname,Housename)  
 
##
total=[]    
house_info={}             #新建一个字典用于存放单个房屋的信息
house_info['gaode_addressinfo']='gaode_addressinfo'
house_info['lianjia_addressinfo']='lianjia_addressinfo'
house_info['unit_price']='unit_price'
house_info['latitude']='latitude'
house_info['longitude']='longitude'
total.append(house_info)
i=0
with open('深圳二手房数据.csv') as f:
    f_csv = csv.DictReader(f)
    for row in f_csv:
        i=i+1
        if (i<2):
            pois = getpois(cityname, row['addressinfo']) 
            if (pois=='未获取经纬度'):
                continue
            house_info={}             #新建一个字典用于存放单个房屋的信息
            house_info['gaode_addressinfo']=pois['name']
            house_info['lianjia_addressinfo']=row['addressinfo']
            house_info['unit_price']=re.findall("\d+",row['unit_price'])[0]
            house_info['latitude']=pois['location'].split(",")[0]
            house_info['longitude']=pois['location'].split(",")[1]
            total.append(house_info) 
            if (i%10==0):
                time.sleep(1)
            if (i%100==0):
                print(i)
                my_df = pd.DataFrame(total)
                #my_df.to_csv('深圳二手房地址.csv', index=False, header=False)
        


