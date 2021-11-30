import copy
class Time(object):
   """Represents the time of day.
attributes: hour, minute, second
"""
time = Time()
time.hour = 11
time.minute = 59
time.second = 30
def print_time(time):
    print('%s:%s:%s'%(time.hour,time.minute,time.second))
#print_time()

t1 = Time()
t1.hour = 11
t1.minute = 59
t1.second = 30

t2 = Time()
t2.hour = 11
t2.minute = 59
t2.second = 20

def is_after(t1,t2):
    t1.total = t1.second + (60*t1.minute) + (t1.hour*60*60)
    t2.total = t2.second + (60*t2.minute) + (t2.hour*60*60)
    return(t1.total>t2.total)
#print(is_after(t1,t2))
def increment(time, seconds):
    time.second += seconds
    if time.second <60 and time.minute <60:
        return
    if time.second >= 60:
        time.second -=60
        time.minute +=1
        increment(time,0)
    if time.minute >= 60:
        time.minute -=60
        time.hour +=1
        increment(time,0)
t3 = Time()
t3.hour = 11
t3.minute = 30
t3.second = 30
#increment(t3,800)
#print_time(t3)
def increment2(time, seconds):
    t4 = copy.deepcopy(time)
    t4.second += seconds
    if t4.second >= 60:
        while t4.second >=60:
            if t4.second >= 60:
                t4.second -=60
                t4.minute +=1
    if t4.minute >=60:
        while t4.second >=60:
            if t4.minute >= 60:
                t4.minute -=60
                t4.hour +=1
    return t4
#print_time(t3)
#print_time(increment2(t3,45))
#print_time(t3)

def time_to_int(time):
    minutes = time.hour * 60 + time.minute
    seconds = minutes * 60 + time.second
    return seconds
def int_to_time(seconds):
    time = Time()
    minutes, time.second = divmod(seconds, 60)
    time.hour, time.minute = divmod(minutes, 60)
    return time
def increment3(time,seconds):
    temp = copy.deepcopy(time)
    temp = time_to_int(time)
    temp += seconds
    temp  = int_to_time(temp)
    return temp 

#print_time(increment3(t3,45))
def lap_pace(time,distance):
    sec = time_to_int(time)
    avg = sec//int(distance)
    return int_to_time(avg)
race = Time()
race.minute = 24  
race.hour = 0 
race.second = 0
#print_time(lap_pace(race,6))
import datetime
#print(datetime.date.today())
def weeknum():
 cur = datetime.date.today()
 print( datetime.date.weekday(cur))
#weeknum()

#datetime.datetime.fromisoformat()
print(datetime.datetime(2019,4,13))
#yyy,m,dd
def birthday(bdate):
    bargs = bdate.split(',')
    bargs = [int(i) for i in bargs]
    d = datetime.datetime(bargs[0],bargs[1],bargs[2])
    dif = datetime.datetime.now() - d
    print(dif.days//365)
birthday('1995,6,14')

