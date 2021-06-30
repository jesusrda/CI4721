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
        """ f function for parsing """
        return self.dist[self.cc[index]]
    
    def _g(self, index: str):
        """ g function for parsing """
        return self.dist[self.cc[index + self.n_operators]]

    def add_rule(self, symbol: str, symbols: List[str]):
        """ Function to insert a production in the parser  """
        
        if self.built:
            raise Exception(f"no se puede insertar más producciones: analizador construido")

        if len(symbols) == 0:
            # There is already a lambda production, we can not insert more than one
            if self.must_be_init_sym is not None and self.must_be_init_sym != symbol:
                raise Exception(f"no se puede insertar más de una lambda producción.")
            else:
                self.must_be_init = symbol

        # Storing initial symbol of the rule
        self.non_terminals.add(symbol)

        # Adding operators to our state
        for sym in symbols:
            if not is_non_terminal(sym) and sym not in self.opr_index:
                print(sym)
                self.opr_index[sym] = self.n_operators
                self.str_val[self.n_operators] = sym
                self.n_operators += 1
        
        # Storing rule for parsing
        rule = "$".join(symbols)
        self.rules[rule] = symbol

    def mark_init(self, symbol: str):
        """ Function to set the initial symbol of the grammar """

        if self.built:
            raise Exception(f"no se puede cambiar el súmbolo inicial: analizador construido")

        if symbol not in self.non_terminals:
            raise Exception(f"no existen reglas definidas para {symbol}.")

        if self.must_be_init_sym is not None and self.must_be_init_sym != symbol:
            raise Exception(f"{self.must_be_init_sym} debe ser el símbolo inicial puesto a que posee una lambda producción.")
        
        self.init_sym = symbol

    def insert_precedence(self, op1: str, prec: str, op2: str):
        """ Function to insert a precedence rule in our parser """

        if self.built:
            raise Exception(f"no se puede insertar precedencias: analizador construido")
         
        if op1 not in self.opr_index or op2 not in self.opr_index:
            raise Exception(f"operador indefinido")

        id1 = self.opr_index[op1]
        id2 = self.opr_index[op2]
        self.precedences[(id1, id2)] = prec

    def find(self, u:int):
        """ find function of union-set algorithm to calculate equivalence classes """
        if u != self.cc[u]:
            self.cc[u] = self.find(self.cc[u])
        
        return self.cc[u]

    def union(self, u: int, v: int):
        """ union function of union-set algorithm to calculate equivalence classes """
        if (u != v):
            if (self.size[u] < self.size[v]):
                u, v = v, u
            self.cc[v] = u
            self.size[u] += self.size[v]

    def build_cc(self):
        """ Function to check if = is an equivalence relation by building connected components """
        
        # Union find algorithm to find connected components
        self.size = [1 for i in range(2*self.n_operators)]
        self.cc = [u for u in range(2*self.n_operators)]
        deg = [0 for u in range(2*self.n_operators)]
        for ((a, b), p) in self.precedences.items():
            if p == "=":
                u = self.find(a)
                v = self.find(b + self.n_operators)
                if u != v:
                    self.union(u,v)
                deg[a] += 1
                deg[b + self.n_operators] += 1

        # Checking if each connected component is a complete graph
        for u in range(2 * self.n_operators):
            c = self.find(u)
            if deg[u] != self.size[c] - 1:
                return True

        for u in range(2* self.n_operators):
            self.cc[u] = self.find(u)

        return False

    def calculate_dist_rec(self, u:int, graph:List, vis: List) -> bool:
        """ dfs search to perform calculation of distances on the precedence graph """
        result = False
        vis[u] = 0
        for v in graph[u]:
            if vis[v] == 0:
                return True
            elif vis[v] == -1:
                result = self.calculate_dist_rec(v, graph, vis)
                if result:
                    break
            self.dist[u] = max(1+self.dist[v], self.dist[u])
        vis[u] = 1
        return result

    def calculate_distances(self):
        """ function to calculate maximum distances in precedence graph """

        # Building precedence graph
        graph = [[] for i in range(2*self.n_operators)]
        vis = [-1 for i in range(2*self.n_operators)]
        self.dist = [0 for i in range(2*self.n_operators)]
        for ((a, b), p) in self.precedences.items():
            c1 = self.cc[a]
            c2 = self.cc[b + self.n_operators]
            if p == ">":    
                graph[c1].append(c2)
            elif p == "<":
                graph[c2].append(c1)
        
        error = False

        for u in range(2*self.n_operators):
            u = self.cc[u]
            if vis[u] == -1:
                error = self.calculate_dist_rec(u, graph, vis)
                if error:
                    break

        return error


    def build(self):
        """ function to build the parser functions """
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
        
        self.built = True

    def print_parsed(self, parsed: List):
        """ function to print state of the parsed stack """
        print("Pila: ", end="")
        for symbol in parsed:
            print(symbol, end=" ")
        print("")

    def print_stack(self, stack: List):
        """ function to print state of the algorithm stack """
        print("Entrada procesada: ", end="")
        for idx in stack:
            print(self.str_val[idx], end=" ")
        print("")

    def print_remaining(self, symbols: List[str]):
        """ function to print state of the remaining string to process """
        print("Entrada por procesar: ", end="")
        for sym in symbols:
            print(sym, end=" ")
        print("")

    def print_action(self, tp: int, init: str, rule: List):
        """ function to print action to take """
        print("Acción: ", end="")
        if tp == 0:
            print(f"reducir {init} -> ", end="")
            for sym in rule:
                print(f"{sym} ", end="")
            print("")
        elif tp == 1:
            print("leer", end= "")
        elif tp == 2:
            print("rechazar. No se encontró ninguna acción a tomar", end="")
        else:
            print("aceptar", end="")
        print("\n")
    
    def parse(self, symbols: List[str]):
        """ function to parse a given input """
        if not self.built:
            raise Exception(f"el analizador no ha sido construido.")

        if any(self.opr_index.get(sym, -1) == -1 for sym in symbols):
            raise Exception(f"símbolo no reconocido entre los símbolos presentes en la regla")

        symbols.append("$")
        stack = [0]
        parsed = []
        cur = 0

        while cur < len(symbols):
            
            self.print_parsed(parsed)
            self.print_stack(stack)
            self.print_remaining(symbols[cur:])

            p = stack[-1]
            e = symbols[cur]
            eidx = self.opr_index[e]            

            if p == 0 and e == "$":
                self.print_action(3, "", [])
                break

            if (p, eidx) not in self.precedences:
                raise Exception(f"{self.str_val[p]} y {e} no son comparables.")

            if self._f(p) <= self._g(eidx):
                stack.append(eidx)
                parsed.append(e)
                cur += 1
                self.print_action(1, "", [])

            else:

                x = stack.pop()
                while self._f(stack[-1]) >= self._g(x):
                    x = stack.pop()
                    
                rule = []
                while len(parsed) > 0 and self.opr_index.get(parsed[-1], -1) != stack[-1]:
                    rule.append(parsed.pop())
                
                rule = list(reversed(rule))
                init = self.rules.get("$".join(rule), "")
                if init == "":
                    self.print_action(2, "", [])
                    break

                self.print_action(0, init, rule)
                parsed.append(init)

