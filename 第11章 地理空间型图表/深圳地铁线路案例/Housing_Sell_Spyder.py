# -*- coding: utf-8 -*-
"""
Created on Tue Feb 27 14:39:56 2018

@author: Jie Zhang
"""
#https://www.jianshu.com/p/ecf3a88d9ae1 
#https://zhuanlan.zhihu.com/p/28059182

import urllib.request
from lxml import etree
import pandas as pd
#from openpyxl import Workbook

#获取并解码网页
def get_htmltext(url):
    html=urllib.request.urlopen(url)
    htmltext=html.read().decode('utf-8')
    return htmltext

def get_data(htmltext,total):
    html=etree.HTML(htmltext)                        #使用lxml库将获取的网页内容解析
    houses=html.xpath('//ul[@class="sellListContent"]/li')        #将包含所有房屋的代码块提取出来，每个房屋的信息都是在<li class="clear">这个标签下面
    #/html/body/div[4]/div[1]/ul/li[1] 
    #body > div.content > div.leftContent > ul > li:nth-child(1)
    for house in houses:          #迭代，将每个房屋各方面的信息提取出来
        house_info={}             #新建一个字典用于存放单个房屋的信息
        try:         #抽取各个方面信息#
            house_info['title']=house.xpath('.//div[1]/div[1]/a/text()')[0]
            house_info['addressinfo']=house.xpath('.//div[1]/div[2]/div/a/text()')[0]
            #/html/body/div[4]/div[1]/ul/li[1]/div[1]/div[2]/div/a
            house_info['sizeinfo']=house.xpath('.//div[1]/div[2]/div/text()')[0]
            #/html/body/div[4]/div[1]/ul/li[1]/div[1]/div[2]/div/text()
            #house_info['flood']=house.xpath('./div[@class="info clear"]/div[@class="flood"]/div/text()')[0]
            
            house_info['followinfo']=house.xpath('.//div[1]/div[3]/div/text()')[0]
            #/html/body/div[4]/div[1]/ul/li[1]/div[1]/div[3]/div
            house_info['total_price']=house.xpath('.//div[1]/div[6]/div[1]/span/text()')[0]
            #/html/body/div[4]/div[1]/ul/li[1]/div[1]/div[6]/div[1]/span
            house_info['unit_price']=house.xpath('.//div[1]/div[6]/div[2]/span/text()')[0]
            #/html/body/div[4]/div[1]/ul/li[1]/div[1]/div[6]/div[2]/span
            total.append(house_info)   #将单个房屋的信息放入总列表中
        except:     #try，避免某方面信息缺失导致报错
            continue
    print('正在爬取第%d页'%i)

#下面这块代码是用来将解析后的网页内容导出保存，方便查找信息所在的路径
#string=etree.tostring(html, encoding='utf-8', pretty_print=True, method='html')
#handle=open(r'E:\test1\text.txt','w',encoding='utf-8')
#handle.write(string.decode('utf-8'))

#运行起来
total=[]
house_info={}             #新建一个字典用于存放单个房屋的信息
house_info['title']='title'
house_info['addressinfo']='addressinfo'
house_info['sizeinfo']='sizeinfo'
#house_info['flood']='flood'
house_info['followinfo']='followinfo'
house_info['total_price']='total_price'
house_info['unit_price']='unit_price'
total.append(house_info)
for i in range(0,1):
    url='https://sz.lianjia.com/ershoufang/pg%d/'%i   #https://cd.lianjia.com/ershoufang/pg%d/
    htmltext=get_htmltext(url)
    get_data(htmltext, total)

#将结果放进空的DataFrame里面，方便使用pandas库对内容汇总处理。
#df=pd.DataFrame(total)

my_df = pd.DataFrame(total)
#my_df.to_csv('深圳二手房3.csv', index=False, header=False)