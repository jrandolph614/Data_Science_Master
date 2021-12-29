#!/usr/bin/env python
# coding: utf-8

# In[1]:


from keys import bkey, secret
import asyncio
from binance import AsyncClient, BinanceSocketManager
import pandas as pd 
import datetime as dt 
from binance.client import Client
from sqlalchemy import create_engine


# In[2]:


engine = create_engine('sqlite:///CryptoDB.db')
client = Client(bkey,secret,tld='us')


# In[3]:


symbols = pd.read_sql('select name from sqlite_master where type="table"',engine).name.to_list()


# In[4]:


def qry(symbol,lookback:int):
    now = dt.datetime.now() + dt.timedelta(hours=5) #binance time
    before  = now - dt.timedelta(minutes=lookback)
    qry_str = f"""select * from '{symbol}' where time >= '{before}'"""
    return pd.read_sql(qry_str,engine)


# In[5]:


rets = []
for symbol in symbols:
    prices = qry(symbol,3).Price
    cumret = (prices.pct_change() +1).prod()-1
    rets.append(cumret)


# In[6]:


top_coin = symbols[rets.index(max(rets))]
'''print(top_coin)
top_coin = 'SOLUSDT''''


# In[7]:


investment_amt = 300 


# In[8]:


info = client.get_symbol_info(symbol=top_coin)
print(info)


# In[9]:


lotsize = float([i for i in info['filters'] if  i['filterType']=='LOT_SIZE'][0]['minQty'])


# In[10]:


prize = float(client.get_symbol_ticker(symbol=top_coin)['price'])


# In[11]:


buy_quantity = round(investment_amt/prize,len(str(lotsize).split('.')[1])) 


# In[12]:


free_usd = [i for i in client.get_account()['balances'] if i['asset']=='USDT'][0]['free']


# In[14]:


if float(free_usd) > investment_amt:
    order = client.create_order(symbol=top_coin, side ='BUY', type='MARKET',quantity=buy_quantity)
    print(order)
    
else:
    print('order has not been executed. You are already invested')
    quit()
    


# In[13]:


buyprice = float(order['fills'][0]['price'])


# In[14]:


def createframe(msg):
    df = pd.DataFrame([msg['data']])
    df = df.loc[:,['s','E','p']]
    df.columns = ['symbol','Time','Price']
    df.Price = df.Price.astype(float)
    df.Time = pd.to_datetime(df.Time,unit='ms')
    return df


# In[ ]:


async def main(coin):
    bm = BinanceSocketManager(client)
    ts = bm.trade_socket(coin)
    async with ts as tscm:
        while True:
            res = await tscm.recv()
            if res:
                frame = createframe(res)
                if frame['price'][0] < buyprice * 0.97 or frame.Price[0] > 1.005 * buyprice:
                    order = client.create_order(
                    symbol = coin,
                    side='SELL',
                    type='MARKET',
                    quantity = buy_quantity)
    await client.close_connection()
if __name__ == '__main__':
    loop = asyncio.get_event_loop()
    loop.run_until_complete(main(top_coin))

