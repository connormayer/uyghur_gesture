def remove_duplicates(my_list):
    # Helper function to remove duplicates from a list
    return list(set(my_list))

# You can see, in PFSA.py, how to write functions for working with 
# probabilistic FSAs such as gfsa1 above, i.e. backward_prob and val_prob.
# That's a good start, but it's no use for gfsa2... to work with gfsa2 
# we might write a separate function val_bool that does conjunctions and disjunctions. 
# And if we want to do things with a cost FSA, we might write a separate function 
# val_cost, which adds up costs in some places and then picks the smallest 
# cost in some other places.
# But no! That would be painfully repetitive. There's a better way ...

# Now, to introduce the idea of a semiring.
# To be a Semiring you need to have two two-place functions called `gconj' and 
# `gdisj', and two elements called `gtrue' and `gfalse'.'' 
# These four things are named for ``generalized conjunction'', 
# ``generalized disjunction'', ``generalized true'' and 
# ``generalized false''; see the handout.

class Semiring():
    # These are what need to be specified for specific semiring types
    gtrue = None
    gfalse = None

    def gconj(self, x, y):
        raise Exception("gconj not implemented")

    def gdisj(self, x, y):
        raise Exception("gdisj not implemented")

    # Now we can write functions that will be well-defined for 
    # any semiring type.
    def big_gconj(self, values):
        # Performs gconj across all values in a list
        if not values:
            return self.gtrue

        first = values[0]
        rest = values[1:]
        return self.gconj(first, self.big_gconj(rest))

    def big_gdisj(self, values):
        # Performs gdisj across all values in alist
        if not values:
            return self.gfalse

        first = values[0]
        rest = values[1:]
        return self.gdisj(first, self.big_gdisj(rest))

    def distrib_lhs(self, x, y, z):
        # Calculates the lefthand side of the distributive equation
        # Should always equal distrib_rhs
        return self.gconj(x, self.gdisj(y, z))

    def distrib_rhs(self, x, y, z):
        # Calculates the righthand side of the distributive equation
        # Should always equal distrib_lhs
        return self.gdisj(self.gconj(x, y), self.gconj(x, z))

    def dotprod(self, x, y):
        # Calculates the 'semiring dotproduct'
        if not x or not y:
            return self.gfalse

        first_x = x[0]
        rest_x = x[1:]
        first_y = y[0]
        rest_y = y[1:]
        return self.gdisj(
            self.gconj(first_x, first_y), 
            self.dotprod(rest_x, rest_y)
        )

    def expn(self, x, num):
        # Applies gconj to x num times.
        if num <= 0:
            return self.gtrue

        return self.gconj(x, self.expn(x, num - 1)) 

# Here's a generic automaton type that takes a semiring
# and uses it to perform operations on its values
class GenericAutomaton():
    def __init__(self, starts, ends, deltas, semiring):
        self.starts = starts
        self.ends = ends
        self.deltas = deltas
        self.sr = semiring

    def all_states(self):
        # Returns all states in the FSA
        return remove_duplicates(
            [q for (q, _) in self.starts] + 
            [q for (q, _) in self.ends] + 
            [q for (q1, _, _, q2) in self.deltas for q in (q1, q2)]
        )

    def init(self, q):
        return self.sr.big_gdisj([v for (q1, v) in self.starts if q == q1])

    def fin(self, q):
        return self.sr.big_gdisj([v for (q1, v) in self.ends if q == q1])

    def tr(self, q1, x, q2):
        return self.sr.big_gdisj(
            [p for (q3, y, p, q4) in self.deltas if (q3, y, q4) == (q1, x, q2)]
        )

    def backward(self, sequence, q):
        if not sequence:
            return self.fin(q)
            
        first = sequence[0]
        rest = sequence[1:]
        return self.sr.big_gdisj(
            [self.sr.gconj(self.tr(q, first, q1), self.backward(rest, q1)) 
             for q1 in self.all_states() if self.tr(q, first, q1) != self.sr.gfalse]
        )

    def backward2(self, sequence, q):
        # A version of backward using for loops instead of list comprehensions
        if sequence == '':
            return self.fin(q)

        first = sequence[0]
        rest = sequence[1:]

        subvals = []
        for q_1 in self.all_states():
            transition = self.tr(q, first, q_1)

            if transition != self.sr.gfalse:
                subval = self.sr.gconj(transition, 
                                    self.backward2(rest, q_1))
                subvals.append(subval)

        return self.sr.big_gdisj(subvals)

    def val_b(self, sequence):
        return self.sr.big_gdisj(
            [self.sr.gconj(self.init(q), self.backward(sequence, q)) 
             for q in self.all_states() if self.init(q) != self.sr.gfalse]
        )

    def val_b2(self, sequence):
        # A version of val_b using for loops instead of list comprehensions
        subvals = []
        for q_0 in self.all_states():
            init_val = self.init(q_0)
            if init_val != self.sr.gfalse:
                subvals.append(
                    self.sr.gconj(
                        self.init(q_0),
                        self.backward2(sequence, q_0)
                    )
                )
        return self.sr.big_gdisj(subvals)

# Here are some sample semirings and automata that use them
# This says, ``bool is a semiring type. When some code 
# based on semirings uses `gconj x y', what that means for 
# the type bool is `x && y'; when some semiring code uses 
# `gdisj x y', what that means for bool is `x || y'; etc.''

class BoolSemiring(Semiring):
    gtrue = True
    gfalse = False

    def gconj(self, x, y):
        return x and y

    def gdisj(self, x, y):
        return x or y

# Similarly for the Double type. 
# When some code based on semirings uses `gconj x y', what 
# that means for the type Double is `x * y'; etc.

class DoubleSemiring(Semiring):
    gtrue = 1.0
    gfalse = 0.0

    def gconj(self, x, y):
        return x * y

    def gdisj(self, x, y):
        return x + y

# Corresponds to the PFSA used in examples in class
gfsa1 = GenericAutomaton(
    starts = [("Edge", 1.0)],
    ends = [("Edge", 0.5)],
    deltas = [
        ("Edge", 'a', 0.015, "Edge"),       ("Internal", 'a', 0.042, "Edge"),
        ("Edge", 'i', 0.015, "Edge"),       ("Internal", 'e', 0.056, "Edge"),
                                            ("Internal", 'i', 0.014, "Edge"),
                                            ("Internal", 'n', 0.098, "Edge"),
                                            ("Internal", 't', 0.084, "Edge"),
                                            ("Internal", 's', 0.154, "Edge"), 
        ("Edge", 'a', 0.103, "Internal"),   ("Internal", 'a', 0.085, "Internal"),
        ("Edge", 'e', 0.029, "Internal"),   ("Internal", 'e', 0.149, "Internal"),
        ("Edge", 'i', 0.088, "Internal"),   ("Internal", 'i', 0.149, "Internal"),
        ("Edge", 'n', 0.029, "Internal"),   ("Internal", 'n', 0.085, "Internal"),
        ("Edge", 't', 0.103, "Internal"),   ("Internal", 't', 0.021, "Internal"),
        ("Edge", 's', 0.118, "Internal"),   ("Internal", 's', 0.064, "Internal")
    ],
    semiring = DoubleSemiring()
)

# Corresponds to the FSA over the alphabet {C,V} that requires either two Cs or
# two Vs (or both), now expressed as a generic automaton that has booleans as
# its values
gfsa2 = GenericAutomaton(
    starts = [(40, True)],
    ends = [(43, True)],
    deltas = [
        (40, 'C', True, 40),      (41, 'C', True, 43),
        (40, 'V', True, 40),      (42, 'V', True, 43),
        (40, 'C', True, 41),      (43, 'C', True, 43),
        (40, 'V', True, 42),      (43, 'V', True, 43)
    ],
    semiring = BoolSemiring()
)

# print(gfsa2.val_b2('CVCVCV'))
# print(gfsa2.val_b('CVCVCV'))
# print(gfsa2.val_b2('CVCCVCV'))
# print(gfsa2.val_b('CVCCVCV'))

# print(gfsa1.val_b2('eta'))
# print(gfsa1.val_b('eta'))
# print(gfsa1.val_b2('ena'))
# print(gfsa1.val_b('ena'))