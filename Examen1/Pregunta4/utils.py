import re

from typing import List

tkn_rgx = re.compile("[ -~]+")
upper_rgx = re.compile("[A-Z]")

def is_non_terminal(symbol: str) -> bool:
    return isinstance(symbol, str) and len(symbol) == 1 and symbol.isupper()

def valid_token(symbol: str) -> bool:
    return ((re.fullmatch(tkn_rgx, symbol) is not None) and (is_non_terminal(symbol) or (re.search(upper_rgx, symbol) is None)))

def is_op_grammar(rule: List[str]) -> bool:
    
    for a, b in zip([*rule, ""], ["", *rule]):
        if a != "" and b != "":
            if is_non_terminal(a) and is_non_terminal(b):
                return False
    return True

def is_prec(prec: str):
    return prec == "<" or prec == ">" or prec == "="