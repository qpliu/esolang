import expr
import tokenizer

class ParseError(BaseException):
    pass

class Pattern:
    def __init__(self, match, binding, index):
        self.match = match
        self.binding = binding
        self.index = index
    def is_wild(self):
        return self.binding == '.'
    def is_literal(self):
        return self.binding == '_'

class Defn:
    def __init__(self, tokens):
        self.name = tokens.__next__()
        if self.name == '.' or self.name == '=' or self.name == '_' \
           or self.name == '0' or self.name == '1':
            raise ParseError
        self.patterns = self._parse_patterns(tokens)
        self.body = self._collect_body(tokens)

    def _parse_patterns(self, tokens):
        patterns = []
        match = []
        index = 0
        while True:
            token = tokens.__next__()
            if token == '=':
                if match != []:
                    patterns.append(Pattern(match, ".", None))
                return patterns
            elif token == '0':
                match.append(False)
            elif token == '1':
                match.append(True)
            elif token == '.' or token == '_':
                patterns.append(Pattern(match, token, None))
                match = []
            else:
                patterns.append(Pattern(match, token, index))
                match = []
                index = index + 1

    def _collect_body(self, tokens):
        body = []
        while True:
            token = tokens.__next__()
            if token == '.':
                return body
            body.append(token)

    def _arity(self, fns, name):
        return len(fns[name][0].patterns) if name in fns else None

    def _binding(self, patterns, name):
        for pattern in patterns:
            if name == pattern.binding:
                return pattern.index
        return None

    def parse_body(self, fns):
        if len(self.body) == 0:
            self.expr = expr.LiteralExpr([])
        else:
            self.expr = self._parse_exprs(self.body, 0, self.patterns, fns)
        del self.body

    def _parse_exprs(self, tokens, index, patterns, fns):
        exp, index = self._parse_expr(tokens, index, patterns, fns)
        if index >= len(tokens):
            return exp
        return expr.ConcatExpr(exp, self._parse_exprs(tokens, index, patterns, fns))

    def _parse_expr(self, tokens, index, patterns, fns):
        token = tokens[index]
        if token == '0' or token == '1' or token == '_':
            return self._parse_literal(tokens, index)
        binding = self._binding(patterns, token)
        if binding != None:
            return expr.BoundExpr(binding), index + 1
        arity = self._arity(fns, token)
        if arity == None:
            raise ParseError
        args = []
        index = index + 1
        while len(args) < arity:
            if index >= len(tokens):
                raise ParseError
            arg, index = self._parse_expr(tokens, index, patterns, fns)
            args.append(arg)
        return expr.CallExpr(token, args), index

    def _parse_literal(self, tokens, index):
        bits = []
        while True:
            if index >= len(tokens):
                return expr.LiteralExpr(bits), index
            elif tokens[index] == '0':
                bits.append(False)
            elif tokens[index] == '1':
                bits.append(True)
            elif tokens[index] == '_':
                return expr.LiteralExpr(bits), index + 1
            else:
                return expr.LiteralExpr(bits), index
            index = index + 1

def collect_defns(fns, tokens):
    def defns_list():
        while True:
            yield Defn(tokens)
    for defn in defns_list():
        if defn.name in fns:
            if len(fns[defn.name][0].patterns) != len(defn.patterns):
                raise ParseError
            fns[defn.name].append(defn)
        else:
            fns[defn.name] = [defn]

def parse_file(fns, file):
    def chars():
        for line in file:
            for char in line:
                yield char
    collect_defns(fns, tokenizer.tokens(chars()))

def parse_bodies(fns):
    for name, defns in fns.items():
        for defn in defns:
            defn.parse_body(fns)
