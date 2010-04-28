class LiteralExpr:
    def __init__(self, bits):
        self.bits = bits

class BoundExpr:
    def __init__(self, index):
        self.index = index

class ConcatExpr:
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

class CallExpr:
    def __init__(self, fn, args):
        self.fn = fn
        self.args = args
