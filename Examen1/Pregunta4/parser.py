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

    # built state
    built = False

    def _f(self, index: str):
        return self.dist[self.cc[index]]
    
    def _g(self, index: str):
        return self.dist[self.cc[index + self.n_operators]]

    def add_rule(self, symbol: str, symbols: List[str]):

        if self.built:
            raise Exception(f"no se puede insertar más producciones: analizador construido")

        if len(symbols) == 0:
            if self.must_be_init_sym is not None and self.must_be_init_sym != symbol:
                raise Exception(f"no se puede insertar más de una lambda producción.")
            else:
                self.must_be_init = symbol

        self.non_terminals.add(symbol)

        for sym in symbols:
            if not is_non_terminal(sym) and sym not in self.opr_index:
                self.opr_index[sym] = self.n_operators
                self.str_val[self.n_operators] = sym
                self.n_operators += 1
        
        rule = "$".join(symbols)
        self.rules[rule] = symbol


    
    def mark_init(self, symbol: str):

        if self.built:
            raise Exception(f"no se puede cambiar el súmbolo inicial: analizador construido")

        if symbol not in self.non_terminals:
            raise Exception(f"no existen reglas definidas para {symbol}.")
        
        self.init_sym = symbol

    def insert_precedence(self, op1: str, prec: str, op2: str):

        if self.built:
            raise Exception(f"no se puede insertar precedencias: analizador construido")
         
        if op1 not in self.opr_index or op2 not in self.opr_index:
            raise Exception(f"operador indefinido")

        id1 = self.opr_index[op1]
        id2 = self.opr_index[op2]
        self.precedences[(id1, id2)] = prec

    def build_cc_rec(self, u: int, cur_cc: int, graph: List[List[str]]):
        self.cc[u] = cur_cc
        self.len_cc[cur_cc] += 1
        for v in graph[u]:
            if self.cc[v] == -1:
                self.build_cc_rec(v, cur_cc, graph)

    def build_cc(self):

        pre_graph = [[] for i in range(2 * self.n_operators)]
        for ((a, b), p) in self.precedences.items():
            if p == "=":
                pre_graph[a].append(b + self.n_operators)
                pre_graph[b + self.n_operators].append(a)

        self.cc = [-1 for i in range(2*self.n_operators)]
        self.len_cc = []
        self.n_cc = 0
        for u in range(2 * self.n_operators):
            if self.cc[u] == -1:
                self.len_cc.append(0)
                self.build_cc_rec(u, self.n_cc, pre_graph)
                self.n_cc += 1

        for u in range(2 * self.n_operators):
            c = self.cc[u]
            if len(pre_graph[u]) != self.len_cc[c] - 1:
                return True

        return False

    def calculate_dist_rec(self, u:int, graph:List) -> bool:

        result = False
        self.dist[u] = 0
        self.vis[u] = 0
        for v in graph[u]:
            if self.vis[v] == 0:
                return True
            elif self.vis[v] == -1:
                result = self.calculate_dist_rec(v, graph)
                if result:
                    break
            self.dist[u] = max(1+self.dist[v], self.dist[u])
        self.vis[u] = 1
        return result

    def calculate_distances(self):

        graph = [[] for i in range(self.n_cc)]
        self.vis = [-1 for i in range(self.n_cc)]
        self.dist = [-1 for i in range(self.n_cc)]
        for ((a, b), p) in self.precedences.items():
            if p == ">":
                c1 = self.cc[a]
                c2 = self.cc[b + self.n_operators]
                graph[c1].append(c2)
            elif p == "<":
                c1 = self.cc[a + self.n_operators]
                c2 = self.cc[b]
                graph[c2].append(c1)
        
        error = False

        for u in range(self.n_cc):
            if self.dist[u] == -1:
                error = self.calculate_dist_rec(u, graph)

                if error:
                    break

        return error


    def build(self):

        if self.built:
            raise Exception(f"el analizador ya ha sido construido previamente")
        
        if self.init_sym is None:
            raise Exception(f"no se puede construir un parser sin especificar el símbolo inicial.")

        error = self.build_cc()

        if error:
            raise Exception(f"f y g no pueden ser construidas: = no es una relación de equivalencia")

        error = self.calculate_distances()

        if error:
            raise Exception(f"f y g no pueden ser construidas: se encontró un ciclo.")

        print("Analizador sintático construido.")

        print("Valores para f:")
        for i in range(self.n_operators):
            print(self.str_val[i] + ": " + str(self._f(i)))

        print("Valores para g:")
        for i in range(self.n_operators):
            print(self.str_val[i] + ": " + str(self._g(i)))
        
        built = True

    
    def pasrse(self, symbols: List[str]):

        if not self.built:
            raise Exception(f"el analizador no ha sido construido.")
        
        self.stack = [0]
        self.parsed = []

        cur = 0
        while cur < len(symbols):

            p = self.stack[-1]

            e = symbols[cur]
            eidx = self.opr_index[e]

            if p == 0 and e == "$":
                break

            if self._f(p) <= self._g(eidx):
                self.stack.append(eidx)
                cur += 1

            else:
                x = self.stack[-1]
                self.stack.pop()
                while self._f(self.stack[-1]) >= self._g(x):
                    x = self.stack[-1]
                    self.stack.pop()
                
                rule = []
                while self.opr_index.get(self.parsed[-1], -1) != self.stack[-1]:
                    rule.append(self.parsed[-1])
                    self.parsed.pop()
                rule = self.rules["$".join(reversed(rule)]
                print(f"Reduce {rule}")

