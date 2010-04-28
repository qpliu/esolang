import expr

class UndefinedFunctionError(BaseException):
    pass

class NilVal:
    def value():
        return None

    def next():
        assert False

class LiteralVal:
    def __init__(self, bits, index):
        self._bits = bits
        self._index = index
        self._next = None

    def value(self):
        if self._index >= len(self._bits):
            return None
        else:
            return self._bits[self._index]

    def next(self):
        if self._next == None:
            assert self._index < len(self._bits)
            self._next = LiteralVal(self._bits, self._index + 1)
        return self._next

class FileVal:
    def __init__(self, file, index = 7, byte = 0):
        self._file = file
        self._index = index
        self._byte = byte
        self._next = None

    def value(self):
        if self._byte == None:
            assert self._index == 7
            bytes = self._file.read(1)
            if len(bytes) == 0:
                self._byte = -1
                self._file.close()
            else:
                self._byte = bytes[0]
        if self._byte < 0:
            return None
        return (self._byte & (1 << self._index)) != 0

    def next(self):
        if self._next == None:
            self.value()
            assert self.value() != None
            if self._index > 0:
                self._next = FileVal(self._file, self._index - 1, self._byte)
            else:
                self._next = FileVal(self._file, 7, None)
        return self._next

class ConcatVal:
    def __init__(self, head, tail):
        self._head = head
        self._tail = tail
        self._value = None
        self._next = None

    def value(self):
        if self._head != None:
            bit = self._head.value()
            if bit != None:
                self._value = bit
                self._next = ConcatVal(self._head.next(), self._tail)
            else:
                self._value = self._tail.value()
                if self._value != None:
                    self._next = self._tail.next()
            self._head = None
            self._tail = None
        return self._value

    def next(self):
        if self._head != None:
            self.value()
            assert self._value != None
        return self._next

class ExprVal:
    def __init__(self, fns, expr, bindings):
        self._fns = fns
        self._expr = expr
        self._bindings = bindings
        self._value = None
        self._next = None

    def value(self):
        if self._expr != None:
            if self._expr.__class__ is expr.LiteralExpr:
                self._value, self._next = self._eval_literal()
            elif self._expr.__class__ is expr.BoundExpr:
                self._value, self._next = self._eval_bound()
            elif self._expr.__class__ is expr.ConcatExpr:
                self._value, self._next = self._eval_concat()
            elif self._expr.__class__ is expr.CallExpr:
                self._value, self._next = self._eval_call()
            else:
                assert False
            self._expr = None
        return self._value

    def next(self):
        if self._expr != None:
            self.value()
            assert self._value != None
        return self._next

    def _eval_literal(self):
        if self._expr.bits == []:
            return None, None
        else:
            return self._expr.bits[0], LiteralVal(self._expr.bits, 1)

    def _eval_bound(self):
        val = self._bindings[self._expr.index]
        bit = val.value()
        return bit, val.next() if bit != None else None

    def _eval_concat(self):
        head = ExprVal(self._fns, self._expr.head, self._bindings)
        tail = ExprVal(self._fns, self._expr.tail, self._bindings)
        bit = head.value()
        if bit != None:
            return bit, ConcatVal(head.next(), tail)
        else:
            bit = tail.value()
            return bit, tail.next() if bit != None else None

    def _eval_call(self):
        val = apply(self._fns, self._expr.fn,
                    [ExprVal(self._fns, arg, self._bindings)
                     for arg in self._expr.args])
        bit = val.value()
        return bit, val.next() if bit != None else None

def apply(fns, fn, args):
    def match(pattern, val):
        for bit in pattern.match:
            b = val.value()
            if b == None or b != bit:
                return False
            val = val.next()
        if pattern.is_literal():
            return val.value() == None
        elif pattern.is_wild():
            return True
        else:
            return val

    for defn in fns[fn]:
        bindings = []
        for pattern, arg in zip(defn.patterns, args):
            binding = match(pattern, arg)
            if binding == False:
                break
            elif binding == True:
                continue
            else:
                bindings.append(binding)
        else:
            return ExprVal(fns, defn.expr, bindings)
    raise UndefinedFunctionError
