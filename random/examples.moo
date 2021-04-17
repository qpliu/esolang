zero a = a <= a

one a = (a <= a) <= a

not a b = (a <= zero) <= b

(->) a b c = (a <= b) <= c

(||) a b c = (not a -> b) <= c

(&&) a b c = a <= (b <= c)

(==) a b c = ((a <= b) && (b <= a)) <= c

(/=) a b c = c == not (a == b)

(<) a b c = c == ((a <= b) && (a /= b))

inc a b = a < b

dec a b = inc b == a

(+) a b c =
  ((b == zero) -> (c == a)) &&
  ((b /= zero) -> (c == inc (a + dec b)))

(-) a b c = (c + b) == a

(*) a b c =
  ((b == zero) -> (c == zero)) &&
  ((b /= zero) -> (c == (a + (a * dec b))))

(/) a b c = (c * b) == a

0 a = a <= a

1 a = a

2 a = not (a <= 1)

(++) a b = not (b <= a)

(plus) a b c = (not ((a == 0) && (b <= c))) <= (inc (a == 0) <= (1 <= ((dec a plus inc b) <= c)))