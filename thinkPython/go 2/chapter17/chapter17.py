def int_to_time(seconds):
    hour = seconds//3600 
    seconds = seconds%3600
    minute = seconds//60
    seconds = seconds%60
    out = Time(hour,minute,seconds)
class Time:
    def __init__(self,hour=0,minute=0,second=0):
        self.hour =hour
        self.minute = minute
        self.second = second
    def __str__(self):
        print('%.2d:%.2d:%.2d'% (self.hour,self.minute,self.second))
    def __add__(self,other):
        if isinstance(other,Time):
            return self.add_time()
        else:
            return self.increment()
    def __radd__(self,other):
        return self.__add__(other)
    def increment(self,seconds):
        seconds+= self.time_to_int()
        return int_to_time(seconds)
    def is_after(self,other):
        return self.time_to_int() > other.time_to_int()
    def add_time(self,other):
        seconds = self.time_to_int() + other.time_to_int()
        return int_to_time(seconds)
    def time_to_int(self):
        out = 0
        out +=((self.hour*3600)+ (self.minute*60) + self.second)
        return out


class Kangaroo:
    def __init__(self,pouch=[]):
        self.pouch_contents = pouch
    def put_in_pouch(self, object):
        self.pouch_contents.append(object)

    