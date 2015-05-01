import oandapy
import datetime
import message

oanda = oandapy.API(environment="practice", access_token="90c9fea896069cec944d4b03404a4725-3665345b9d5b4cc4cea8711349587903")

# response = oanda.get_prices(instruments="XAU_USD")
# prices = response.get("prices")
# asking_price = prices[0].get("ask")

# print prices;

account_id = 767708

# order = oanda.create_order(account_id, instrument="XAU_USD", units=1, side="buy", type="market")
# trade = oanda.modify_trade(account_id, 954385749, stopLoss=20)



# trades = oanda.get_trades(account_id)
# if 'trades' in trades:
#     for trade in trades['trades']:
#         print trade
#         # print oanda.modify_trade(account_id, trade['id'], trailingStop=20)
#     print 

# prices = oanda.get_prices(instruments='XAU_USD')
# xau_price = prices['prices'][0]
# print xau_price

# expiry_time = (datetime.datetime.utcnow() + datetime.timedelta(0,300)).isoformat('T')
# upper_order = oanda.create_order(account_id, instrument="XAU_USD", units=1, side="buy", type="stop", price=xau_price['ask']+1, expiry=expiry_time)
# lower_order = oanda.create_order(account_id, instrument="XAU_USD", units=1, side="sell", type="stop", price=xau_price['bid']-1, expiry=expiry_time)

# print upper_order
# print lower_order


# trade = oanda.modify_trade(account_id, 954384683, stopLoss=20)
# print trade

message.send('hello world')

print "ok"