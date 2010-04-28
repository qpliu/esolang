def tokens(chars):
    lookahead = None
    while True:
        if lookahead == None:
            char = chars.__next__()
        else:
            char = lookahead
            lookahead = None
        if char == '0' or char == '1' or char == '.' or char == '_':
            yield char
        elif char == '=':
            try:
                lookahead = chars.__next__()
            except StopIteration:
                yield '='
                raise StopIteration
            if lookahead != '=':
                yield char
            else:
                lookahead = None
                while chars.__next__() != '\n':
                    pass
        elif char != ' ' and char != '\t' and char != '\r' and char != '\n':
            symbol = char
            while True:
                try:
                    char = chars.__next__()
                except StopIteration:
                    yield symbol
                    raise StopIteration
                if (char == '0' or char == '1' or char == '.' or char == '='
                    or char == '_' or char == ' ' or char == '\t'
                    or char == '\r' or char == '\n'):
                    yield symbol
                    lookahead = char
                    break
                else:
                    symbol = symbol + char
