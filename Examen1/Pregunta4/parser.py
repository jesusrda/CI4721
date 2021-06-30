from typing import List

from utils import *

class Parser:

    # Storing initial symbol
    must_be_init_sym = None
    init_sym = None

    # Storing precedences
    precedences = dict()

    # Storing rules
    rules = dict()
    non_terminals = set()
    
    # Storing operators
    n_operators = 1
    opr_index = {"$": 0}
    str_val = {0: "$"}

    # Storing equivalence classes
    eq_class = []
    n_classes = 0

    # Parser state
    built = 0
    n_oprs = 0
    f_prec = dict()

    def _f(self, index: str):
        return self.dist[self.eq_class[index]]
    
    def _g(self, index: str):
        return self.dist[self.eq_class[index + self.n_operators]]

    def add_rule(self, symbol: str, symbols: List[str]):

        if len(symbols) == 0:
            if self.must_be_init_sym is not None and self.must_be_init_sym != symbol:
                raise Exception(f"no se puede insertar más de una lambda producción.")
            else:
                self.must_be_init = symbol

        self.non_terminals.add(symbol)

        for sym in symbols:
            if not is_non_terminal(sym) and sym not in self.opr_index:
                self.opr_index[sym] = self.n_operators
                self.ser_val[self.n_operators] = sym
                self.n_operators += 1
        
        rule = "$".join(symbols)
        self.rules[rule] = symbol


    
    def mark_init(self, symbol: str):

        if symbol not in self.non_terminals:
            raise Exception(f"no existen reglas definidas para {symbol}.")
        
        self.init_sym = symbol

    def insert_precedence(self, op1: str, prec: str, op2: str):
         
        if op1 not in self.opr_index or op2 not in self.opr_index:
            raise Exception(f"operador indefinido")

        id1 = self.opr_index[op1]
        id2 = self.opr_index[op2]
        self.precedences[(id1, id2)] = prec

    def build(self):
        
        if self.init_sym is None:
            raise Exception(f"no se puede construir un parser sin especificar el símbolo inicial.")

        pre_graph = [[] for i in range(2 * self.n_operators)]
        for ((a, b), p) in self.precedences.items():
            if p == "=":
                pre_graph[a].append(b + self.n_operators)
                pre_graph[b + self.n_operators].append(a)

        error = self.build_classes(pre_graph)

        if error:
            raise Exception(f"f y g no pueden ser construidas: = no es una relación de equivalencia")

        graph = [[] for i in range(self.n_classes)]
        for ((a, b), p) in self.precedences.items():
            if p == ">":
                c1 = self.eq_class[a]
                c2 = self.eq_class[b + self.n_operators]
                graph[c1].append(c2)
            elif p == "<":
                c1 = self.eq_class[a + self.n_operators]
                c2 = self.eq_class[b]
                graph[c2].append(c1)
        
        error = self.calculate_distances(graph)

        if error:
            raise Exception(f"f y g no pueden ser construidas: se encontró un ciclo.")

        print("Analizador sintático construido.")

        print("Valores para f:")
        for i in range(self.n_operators):
            print(self.str_val[i] + ": " + str(self._f(i)))

        print("Valores para g:")
        for i in range(self.n_operators):
            print(self.str_val[i] + ": " + str(self._g(i))



        





        
