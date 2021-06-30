

from parser import Parser

from typing import List

from utils import *

def add_rule(symbol: str, rule: List[str], parser: Parser):
          
    if not is_non_terminal(symbol):
        print(f"error: {symbol} no es un símbolo terminal.")
        return

    if not all(valid_token(x) for x in rule):
        print(f"error: token inválido.")
        return
    
    if not is_op_grammar(rule):
        print(f"error: regla inválida para una gramática de operadores.")
        return

    try: 
        parser.add_rule(symbol, rule)
        print(f"Regla agregada a la gramática.")
    except Exception as ex:
        print("error: " + str(ex) + " Skipped")

def mark_init(symbol: str, parser: Parser):
          
    if not is_non_terminal(symbol):
        print(f"error: {symbol} no es un nodo no-terminal.")
        return

    try: 
        parser.mark_init(symbol)
        print(f"'{symbol}'' es ahora el símbolo inicial de la gramática.")
    except Exception as ex:
        print("error: " + str(ex))

def insert_precedence(op1: str, prec: str, op2: str, parser: Parser):

    if not is_prec(prec):
        print(f"error: {prec} no es un operador de precedencia válido")
        return

    if not valid_token(op1) or not valid_token(op2) or is_non_terminal(op1) or is_non_terminal(op2):
        print(f"error: operador inválido.")
        return

    try: 
        parser.insert_precedence(op1, prec, op2)
        print("Regla de precedencia insertada apropiadamente.")
    except Exception as ex:
        print("error: " + str(ex))        

def run():

    parser = Parser()

    while True:

        inpt = input().strip().split()

        if len(inpt) == 0:
            print("error: instrucción vacía.")
            continue
    
        instr = inpt[0]

        if instr == "RULE":

            if len(inpt) < 2:
                print("error: instrucción inválida. Argumentos insuficientes.")
                continue
            
            add_rule(inpt[1], inpt[2:], parser)

        elif instr == "INIT":
            
            if len(inpt) < 2:
                print("error: instrucción inválida. Argumentos insuficientes.")
                continue
            
            mark_init(inpt[1], parser)

        elif instr == "PREC":

            if len(inpt) < 4:
                print("error: instrucción inválida. Argumentos insuficientes.")
                continue

            insert_precedence(*inpt[1:4], parser)

        elif instr == "BUILD":
            
           # try:
            parser.build()
           # except Exception as ex:
            #    print("error: " + str(ex))
            #    continue

        elif instr == "PARSE":
            
            if parser is None:
                print("error: ningún analizador sintáctico ha sido construido")
                continue

            try:
                parser.parse(inpt[1:])
            except Exception as ex:
                print("error: " + str(ex))
                continue


        elif instr == "EXIT":
            break

        else:
            print("error: instrucción inválida")


if __name__ == "__main__":
    run()