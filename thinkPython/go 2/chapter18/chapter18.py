import random
class Card:
    """represents a palying card"""

    def __init__(self,suit=0,rank=2):
        self.suit= suit
        self.rank = rank
    suit_names = ['Clubs', 'Diamonds', 'Hearts', 'Spades']
    rank_names = [None, 'Ace', '2', '3', '4', '5', '6', '7',
                '8', '9', '10', 'Jack', 'Queen', 'King']
    def __str__(self) -> str:
        return '%s of %s' % (Card.rank_names[self.rank],Card.suit_names[self.suit])
    def __lt__(self,other):
        t1 = self.suit, self.rank
        t2 = self.suit, self.rank
        return t1 < t2
class Deck:
    def __init__(self):
        self.cards = []
        for suit in range(4):
            for rank in range(1,14):
                card = Card(suit,rank)
                self.cards.append(card)
    def __str__(self):
        res = []
        for card in self.cards:
            res.append(str(card))
        return '\n'.join(res)
    def pop_card(self):
        return self.cards.pop()
    def add_card(self,card):
        self.cards.append(card)
    def shuffle(self):
        random.shuffle(self.cards)
    def sort(self):
        sorted = True
        while sorted:
            sorted = False
            for i in range(len(self.cards)-1):
                if self.cards[i] > self.cards[i+1]:
                    self.cards[i], self.cards[i+1] = self.cards[i+1], self.cards[i]
                    sorted = True
    def move_cards(self,hand,num):
        for i in range(num):
            hand.add_cards(self.pop_card())
class Hand:
    """represent a hand of playing cards"""
    def __init__(self,label=''):
        self.cards = []
        self.label = label
queen_of_diamonds = Card(1,12)
print(queen_of_diamonds)
