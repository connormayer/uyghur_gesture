from Semiring import (
    Semiring, remove_duplicates, BoolSemiring, DoubleSemiring
)
from math import inf

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

    def val_b(self, sequence):
        return self.sr.big_gdisj(
            [self.sr.gconj(self.init(q), self.backward(sequence, q)) 
             for q in self.all_states() if self.init(q) != self.sr.gfalse]
        )

    def forward(self, sequence, q):
        if not sequence:
            return self.init(q)
        last = sequence[-1]
        rest = sequence[:-1]
        return self.sr.big_gdisj(
            [self.sr.gconj(self.forward(rest, q1), self.tr(q1, last, q))
            for q1 in self.all_states() if self.tr(q1, last, q) != self.sr.gfalse]
        )

    def val_f(self, sequence):
        return self.sr.big_gdisj(
            [self.sr.gconj(self.forward(sequence, q), self.fin(q))
            for q in self.all_states() if self.fin(q) != self.sr.gfalse]
        )

class SetOfStringsSemiring(Semiring):
    gtrue = ['']
    gfalse = []

    def gconj(self, x, y):
        return [s1 + s2 for s1 in x for s2 in y]

    def gdisj(self, x, y):
        return x + y

def get_syllabifier():

    vowels = ['a', 'e', 'i', 'o', 'u', 'ü', 'ö', 'é']
    consonants = [
        'p', 'b', 't', 'd', 'k', 'g', 'q', 'G', 'C', 'j', 'x', 
        'r', 'z', 'Z', 's', 'S', 'f', 'N', 'l', 'm', 'n', 'h', "'",
        'y', 'w'
    ]
    sim_onsets = ['q', 'G', 'C', 'j', 'x', 'r', 'z', 'Z', 's', 'S', 'N', 'l', 'm', 'n', 'h', "'", 'y', 'w']
    comp_onsets = ['p', 'b', 'k', 'g', 'f'] 
    comp_onsets_t = ['t', 'd']

    # Add your definition of the FST here
    deltas = ([('start', v, [v], 'vowel') for v in vowels] +
              [('start', c, [c], 'onset_sim') for c in sim_onsets] +
              [('start', c, [c], 'onset_comp') for c in comp_onsets] +
              [('start', c, [c], 'onset_comp_t') for c in comp_onsets_t] +

              [('onset_sim', v, [v], 'vowel') for v in vowels] +
              [('onset_comp', v, [v], 'vowel') for v in vowels] +
              [('onset_comp', c, [c], 'onset_sim') for c in ['l', 'r']] +
              [('onset_comp_t', v, [v], 'vowel') for v in vowels] +
              [('onset_comp_t', c, [c], 'onset_sim') for c in ['r']] +

              [('vowel', c, ['.{}'.format(c)], 'onset_sim') for c in sim_onsets] +
              [('vowel', c, ['.{}'.format(c)], 'onset_comp') for c in comp_onsets] +
              [('vowel', c, ['.{}'.format(c)], 'onset_comp') for c in comp_onsets_t] +
              
              [('vowel', c, [c], 'coda1') for c in consonants] +
              [('vowel', v, ['.{}'.format(v)], 'vowel') for v in vowels] +
              [('vowel', '-', ['.'], 'start')] +

              [('coda1', c, [c], 'coda2') for c in consonants] +
              [('coda1', c, ['.{}'.format(c)], 'onset_sim') for c in sim_onsets] +
              [('coda1', c, ['.{}'.format(c)], 'onset_comp') for c in comp_onsets] +
               [('coda1', c, ['.{}'.format(c)], 'onset_comp_t') for c in comp_onsets_t] +
              [('coda1', '-', ['.'], 'start')] +
              [('coda1', "'", ['.'], 'start')] +

              [('coda2', c, ['.{}'.format(c)], 'onset_sim') for c in sim_onsets] +
              [('coda2', c, ['.{}'.format(c)], 'onset_comp') for c in comp_onsets] +
              [('coda2', c, ['.{}'.format(c)], 'onset_comp_t') for c in comp_onsets_t] +
              [('coda2', '-', ['.'], 'start')])

    uyghur_syllabifier = GenericAutomaton(
        starts = [('start', ['']), ('onset', []), ('vowel', []), ('coda1', []), ('coda2', [])],
        ends = [('start', []), ('onset', []), ('vowel', ['']), ('coda1', ['']), ('coda2', [''])],
        deltas = deltas,
        semiring = SetOfStringsSemiring()
    )

    return uyghur_syllabifier
