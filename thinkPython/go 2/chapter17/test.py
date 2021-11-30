seconds = 5000
hour = seconds//3600
seconds = seconds%3600
minute = seconds//60
seconds = seconds%60
#print(hour,minute,seconds)

#print(type({}))
if type({}) == list:
    print('yes')