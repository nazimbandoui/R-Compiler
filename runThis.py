import ply
import ply.lex as lex
import ply.yacc as yacc
import sys
import re


tokens = [
    "LPAREN","RPAREN",
    "LACCO","RACCO",
    "LBRACKET","RBRACKET",
    "COMMA",
    "COLON",
    "EQUAL",
    "ADD","SUB","MUL","DIV","MOD",
    "SUP","INF","SUPEQUAL","INFEQUAL","EEQUAL","NOTEQUAL",
    "numeric_value","integer_value","character_value",
    "IF","ELSE",
    "FOR","IN",
    "IDF",
    "NEWLINE",
    "OR","AND",
    "WHILE",
    "NUMERIC","INTEGER","CHARACTER","LOGICAL",
    "TRUE","FALSE",
    "INC","DEC",
    "COMMENT",
]

reserved = {
    "IF": "IF",
    "ELSE": "ELSE",
    "FOR": "FOR",
    "IN": "IN",
    "OR": "OR",
    "AND": "AND",
    "WHILE": "WHILE",
    "NUMERIC": "NUMERIC",
    "INTEGER": "INTEGER",
    "CHARACTER": "CHARACTER",
    "LOGICAL": "LOGICAL",
    "TRUE": "TRUE",
    "FALSE": "FALSE"

}


t_ignore  = " \t\r"

t_LPAREN = r"\("
t_RPAREN = r"\)"
t_LACCO = r"\{"
t_RACCO = r"\}"
t_LBRACKET = r"\["
t_RBRACKET = r"\]"
t_COMMA=r"\,"
t_COLON=r"\:"
t_ADD = r"\+"
t_SUB = r"\-"
t_MUL = r"\*"
t_DIV = r"\/"
t_MOD = r"\%"
t_EQUAL = r"\="
t_INF = r"\<"
t_SUP = r"\>"

def t_TRUE(t):
    r"TRUE"
    t.value = "TRUE"
    return t

def t_COMMENT(t):
    r"\#.*"
    return t

def t_FALSE(t):
    r"FALSE"
    t.value = "FALSE"
    return t

def t_IDF(t):
    r"[a-zA-Z_][a-zA-Z_0-9]*"
    t.type = reserved.get(t.value, "IDF")
    if t.type == "IDF":
        if t.value[0].islower() or len(t.value) > 10:
            print("Lexical error : Invalid identifier line:{} col:{}".format(t.lineno, find_column(t.lexpos)))
            t.lexer.skip(1)
            return
    return t

def t_INC(t):
    r"\+="
    t.value = "+="
    return t

def t_DEC(t):
    r"-="
    t.value = "-="
    return t

def t_NOTEQUAL(t):
    r"!="
    t.value = "!="
    return t

def t_EEQUAL(t):
    r"=="
    t.value = "=="
    return t

def t_SUPEQUAL(t):
    r">="
    t.value = ">="
    return t

def t_INFEQUAL(t):
    r"<="
    t.value = "<="
    return t

def t_character_value(t):
    r"\'.\'"
    t.value = t.value[1]
    return t

def t_numeric_value(t):
    r"(\d+\.\d+|\(\-\d+\.\d+\))"
    t.value = t.value.replace("(", "")
    t.value = t.value.replace(")", "")
    t.value = float(t.value)
    return t

def t_integer_value(t):
    r"(\d+|\(\-\d+\))"
    t.value = t.value.replace("(", "")
    t.value = t.value.replace(")", "")
    t.value = int(t.value)
    if t.value < -32768 or t.value > 32767:
       print("Integer out of range line:{} col:{}".format(t.lineno, find_column(t.lexpos)))
       t.lexer.skip(1)
       return
    return t

def t_NEWLINE(t):
    r"\n+"
    t.lexer.lineno += len(t.value)
    t.value = ""
    return t

def t_error(t):
    print("Lexical Error at line:{} col:{}".format(t.lineno, find_column(t.lexpos)))
    t.lexer.skip(1)

codeSourceText = None

if len(sys.argv) > 1:
    try:
        textSourceFilse = open(sys.argv[1], "r")
        codeSourceText = textSourceFilse.read()
    except FileNotFoundError as e:
        print("Invalid source file")
        exit(0)

lexer = lex.lex()
lexer.input(codeSourceText)

quadruple = []
numQuad = 0
ADRStack = []
cpt = 0

precedence = (
    ('left','OR'),
     ('left','AND'),
     ('left', 'ADD', 'SUB'),
     ('left', 'MUL', 'DIV','MOD')
 )

class Symb(object):
    def __init__(self, name, type=None,category=None,size=1,initialized=False):
        self.name = name
        self.type = type
        self.category = category
        self.size=size
        self.initialized = initialized
    
    def __repr__(self):
        return self.name+"   ||"+self.type+"   ||"+self.category+"   ||"+str(self.size)+"   ||"+str(self.initialized)


class TableSymb(object):
    def __init__(self):
        self._symbols = {}

    def insert(self, symbol):
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        symbol = self._symbols.get(name)
        return symbol

SymbolsTable = TableSymb()

def p_starter(p):
    '''
    STARTER : INSTRUCTION_BLOCK
            | NEWLINE INSTRUCTION_BLOCK
    '''

def p_bloc_inst(p):
    '''
    INSTRUCTION_BLOCK : INSTRUCTION_BLOCK INST NEWLINE
              | INST NEWLINE
              | IF_CLAUSE
              | WHILE_INSTRUCTION
              | FOR_INSTRUCTION
              | INSTRUCTION_BLOCK IF_CLAUSE
              | INSTRUCTION_BLOCK WHILE_INSTRUCTION
              | INSTRUCTION_BLOCK FOR_INSTRUCTION
    '''

def p_inst(p):
    '''
    INST : DECLARATION
         | AFFECT
         | AFFECT_COND
         | INCR
         | DECR
         | COMMENT
    '''
def p_incr(p):
    '''
    INCR : IDF INC integer_value
    '''
    if(SymbolsTable.lookup(p[1])):
        if(SymbolsTable.lookup(p[1]).type == "Character"):
            print("Syntax Error : Incompatible types. Line "+str(p.lineno(1))+" column "+str(find_column(p.lexpos(1)))+"\n")
        else:
            quadruple.append(["+",p[1],p[3],p[1]])
    else:
        print("Syntax Error : Undeclared variable " + str(p[1]) + " at line " + str(p.lineno(1))+" col "+str(find_column(p.lexpos(1))))

def p_decr(p):
    '''
    DECR : IDF DEC integer_value
    '''
    if(SymbolsTable.lookup(p[1])):
        if(SymbolsTable.lookup(p[1]).type == "Character"):
            print("Syntax Error : Incompatible types. Line "+str(p.lineno(1))+" column "+str(find_column(p.lexpos(1)))+"\n")
        else :
            quadruple.append(["-",p[1],p[3],p[1]])
    else:
        print("Syntax Error : Undeclared variable " + str(p[1]) + " at line " + str(p.lineno(1))+" col "+str(find_column(p.lexpos(1))))

def p_affect(p):
    '''
    AFFECT : IDF EQUAL EXPR
    '''
    global numQuad
    quadruple.append([p[2],p[3],None,p[1]])
    p[0] = p[1]
    if(not SymbolsTable.lookup(p[1])):
        symbol = Symb(p[1],"DYNAMIC","Variable")
        SymbolsTable.insert(symbol)
        SymbolsTable.lookup(p[1]).initialized = True
    else:
        SymbolsTable.lookup(p[1]).initialized = True

def p_affect_char(p):
    '''
    AFFECT : IDF EQUAL character_value
    '''
    global numQuad
    quadruple.append([p[2],p[3],None,p[1]])
    p[0] = p[1]
    if(not SymbolsTable.lookup(p[1])):
        symbol = Symb(p[1],"Character","Variable")
        SymbolsTable.insert(symbol)
        SymbolsTable.lookup(p[1]).initialized = True
    else:
        SymbolsTable.lookup(p[1]).initialized = True

def p_declaration(p):
    '''
    DECLARATION : TYPE AFFECT
                | DECLARATION_MULTIPLE
    '''
    if(len(p)>2):
        if(not SymbolsTable.lookup(p[2]) or SymbolsTable.lookup(p[2]).type == "DYNAMIC" ):
            symbol = Symb(p[2],p[1],"Variable")
            symbol.initialized = True
            SymbolsTable.insert(symbol)

def p_declaration_multiple(p):
    '''
    DECLARATION_MULTIPLE : TYPE IDF
    '''
    p[0] = p[1]
    if(not SymbolsTable.lookup(p[2]) or SymbolsTable.lookup(p[2]).type == "DYNAMIC"):
        symbol = Symb(p[2],p[0],"Variable")
        SymbolsTable.insert(symbol)

def p_declaration_multiple_multiple(p):
    '''
    DECLARATION_MULTIPLE : DECLARATION_MULTIPLE COMMA IDF
    '''
    p[0] = p[1]
    if(not SymbolsTable.lookup(p[3]) or not SymbolsTable.lookup(p[3]).type == "DYNAMIC"):
        symbol = Symb(p[3],p[0],"Variable")
        SymbolsTable.insert(symbol)

def p_TABLE_dECLARATION(p):
    '''
    DECLARATION : ARRAY_DECLARATION
    '''
    p[0] = p[1]

def p_tab_declaration(p):
    '''
    ARRAY_DECLARATION : TYPE IDF LBRACKET integer_value RBRACKET
    '''
    p[0] = p[2]
    if(not SymbolsTable.lookup(p[2])):
        symbol = Symb(p[2],p[1],"Variable",p[4])
        SymbolsTable.insert(symbol)

def p_type(p):
    '''
    TYPE : INTEGER
         | NUMERIC
         | CHARACTER
         | LOGICAL
    '''
    p[0] = p[1]

def p_EXPR(p):
    '''
    EXPR : ARITHMETICAL_EXPRESSION
         | COMPARE_EXPRESSION
         | LOGICAL_EXPRESSION
    '''
    p[0] = p[1]

def p_ARITHMETICAL_EXPRESSION(p):
    '''
    ARITHMETICAL_EXPRESSION : ARITHMETICAL_EXPRESSION ADD ARITHMETICAL_EXPRESSION
                      | ARITHMETICAL_EXPRESSION SUB ARITHMETICAL_EXPRESSION
                      | ARITHMETICAL_EXPRESSION MUL ARITHMETICAL_EXPRESSION
                      | ARITHMETICAL_EXPRESSION DIV ARITHMETICAL_EXPRESSION
                      | ARITHMETICAL_EXPRESSION MOD ARITHMETICAL_EXPRESSION
                      | VALUE
    '''
    global cpt
    global numQuad
    if(len(p)>2):
        cpt+=1
        p[0] = "TEMPORARY_" + str(cpt)
        quadruple.append([p[2],p[1],p[3],p[0]])
    else:
        p[0] = p[1]

def p_ARITHMETICAL_EXPRESSION_IDF(p):
    '''
    ARITHMETICAL_EXPRESSION : IDF
                      | IDF LBRACKET ARITHMETICAL_EXPRESSION RBRACKET
    '''
    if(SymbolsTable.lookup(p[1])):
        if(SymbolsTable.lookup(p[1]).initialized):
            p[0]=p[1]
        else :
            print("Syntax error : " + str(p[1]) +" Unassigned variable at line " + str(p.lineno(1))+" col "+str(find_column(p.lexpos(1))))
    else:
        print("Syntax error : Undeclared variable " + str(p[1]) + " at line " + str(p.lineno(1))+" col "+str(find_column(p.lexpos(1))))
    if len(p) > 2:
        p[0] = p[1] + "[" + str(p[3]) + "]"

def p_ARITHMETICAL_EXPRESSION_PAR(p):
    '''
    ARITHMETICAL_EXPRESSION : LPAREN ARITHMETICAL_EXPRESSION RPAREN
    '''
    p[0] = p[2]

def p_COMPARE_EXPRESSION(p):
    '''
    COMPARE_EXPRESSION : ARITHMETICAL_EXPRESSION COMPARISON_OPERATOR ARITHMETICAL_EXPRESSION
    '''
    global numQuad
    global cpt
    cpt += 1
    p[0] = "TEMPORARY_" + str(cpt)
    quadruple.append([p[2],p[1],p[3],p[0]])
def p_COMPARE_EXPRESSION_par(p):
    '''
    COMPARE_EXPRESSION : LPAREN COMPARE_EXPRESSION RPAREN
    '''
    p[0] = p[2]

def p_LOGICAL_EXPRESSION(p):
    '''
    LOGICAL_EXPRESSION : EXPR AND EXPR
    | EXPR OR EXPR
    '''
    global cpt
    if(len(p)>2):
        cpt+=1
        p[0] = "TEMPORARY_" + str(cpt)
        quadruple.append([p[2],p[1],p[3],p[0]])
    else:
        p[0]=p[1]

def p_LOGICAL_EXPRESSION2(p):
    '''
    LOGICAL_EXPRESSION : LOGICAL_VALUE
    '''
    p[0] = p[1]
def p_LOGICAL_EXPRESSION_par(p):
    '''
    LOGICAL_EXPRESSION : LPAREN LOGICAL_EXPRESSION RPAREN
    '''
    p[0] = p[2]

def p_COMPARISON_OPERATOR(p):
    '''
    COMPARISON_OPERATOR : SUP
                  | INF
                  | SUPEQUAL
                  | INFEQUAL
                  | EEQUAL
                  | NOTEQUAL
    '''
    p[0] = p[1]
def p_logical_value(p):
    '''
    LOGICAL_VALUE : TRUE
                  | FALSE
    '''
    p[0] = p[1]

def p_value_integer(p):
    '''
    VALUE : integer_value
    '''
    p[0] = p[1]


def p_value_float(p):
    '''
    VALUE : numeric_value
    '''
    p[0] = p[1]

def p_AFFECT_COND(p):
    '''
    AFFECT_COND : AFF_COND_EXP EXPR RPAREN
    '''
    quadruple.append(['=', p[2], None, p[1]])
    adresseBR = ADRStack.pop()
    quadruple[adresseBR[0]][1] = len(quadruple)
def p_AFF_COND_EXP(p):
    '''
    AFF_COND_EXP : AFF_COND_DEB EXPR COMMA
    '''
    adresseBZ = ADRStack.pop()
    quadruple.append(['=', p[2], None, p[1]])
    ADRStack.append([len(quadruple), 'AFFCONDbr'])
    quadruple.append(['BR', None, None, None])
    quadruple[adresseBZ[0]][1] = len(quadruple)

    p[0] = p[1]

def p_AFF_COND_DEB(p):
    '''
    AFF_COND_DEB : IDF EQUAL LPAREN EXPR COMMA
    '''
    ADRStack.append([len(quadruple), 'AFFCONDtest'])
    quadruple.append(['BZ', None, p[4], None])
    p[0] = p[1]


def p_IF_CLAUSE(p):
    '''
    IF_CLAUSE : IF_INSTRUCTION NEWLINE
              | IF_ELSE_BLOC INSTRUCTION_BLOCK RACCO NEWLINE

    '''
    while(True):
        adresse = ADRStack.pop()
        quadruple[adresse[0]][1] = len(quadruple)
        if adresse[1] is not None:
            break
def p_IF_INSTRUCTION(p):
    '''
    IF_INSTRUCTION : IF_CONDITION NEWLINE LACCO NEWLINE INSTRUCTION_BLOCK RACCO
            | ELSE_IF_CONDITION LACCO NEWLINE INSTRUCTION_BLOCK RACCO
    '''

def p_IF_CONDITION(p):
    '''
    IF_CONDITION : IF LPAREN EXPR RPAREN

    '''
    ADRStack.append([len(quadruple), "startIF"])
    quadruple.append(['BZ', None,  p[3], None])

def p_IF_ELSE_BLOCK(p):
    '''
    IF_ELSE_BLOC : IF_INSTRUCTION NEWLINE ELSE NEWLINE LACCO NEWLINE
    '''
    adresse = ADRStack.pop()
    if adresse[1] == "startIF":
        hint = "firstElse"
    else: hint = None

    ADRStack.append([len(quadruple), hint])
    quadruple.append(['BR', None, None, None])
    quadruple[adresse[0]][1] = len(quadruple)

def p_ELSE_IF(p):
    '''
    ELSE_IF : IF_INSTRUCTION NEWLINE ELSE IF
    '''
    adresse = ADRStack.pop()

    if adresse[1] == "startIF":
        hint = "firstElse"
    else: hint = None

    ADRStack.append([len(quadruple), hint])
    quadruple.append(['BR', None, None, None])
    quadruple[adresse[0]][1] = len(quadruple)

def p_ELSE_IF_CONDITION(p):
    '''
    ELSE_IF_CONDITION : ELSE_IF LPAREN EXPR RPAREN NEWLINE
    '''
    ADRStack.append([len(quadruple), None])
    quadruple.append(['BZ', None,  p[3], None])

def p_WHILE_INSTRUCTION(p):
    '''
    WHILE_INSTRUCTION : WHILE_COND LACCO NEWLINE INSTRUCTION_BLOCK RACCO NEWLINE
    '''
    adresseTest = ADRStack.pop()
    adresseDebutCond = ADRStack.pop()
    quadruple.append(['BR', adresseDebutCond[0],  None, None])
    quadruple[adresseTest[0]][1] = len(quadruple)
def p_while_cond(p):
    '''
    WHILE_COND : WHILE_START LPAREN EXPR RPAREN NEWLINE
    '''
    ADRStack.append([len(quadruple), "WHILEcond"])
    quadruple.append(['BZ', None,  p[3], None])

def p_while_start(p):
    '''
    WHILE_START : WHILE
    '''
    ADRStack.append([len(quadruple), "WHILEstart"])

def p_for_block(p):
    '''
     FOR_INSTRUCTION : FOR_CONDITION LACCO NEWLINE INSTRUCTION_BLOCK RACCO NEWLINE
    '''
    adresseBGE = ADRStack.pop()
    adresseTest = ADRStack.pop()
    quadruple.append(['+', p[1], 1, p[1]])
    quadruple.append(['BR', adresseTest[0], None, None])
    quadruple[adresseBGE[0]][1] = len(quadruple)
def p_for_condition(p):
    '''
    FOR_CONDITION : FOR_START for_int RPAREN NEWLINE
    '''
    ADRStack.append([len(quadruple), "FORbge"])
    quadruple.append(['BGE', None, p[1], p[2]])
    p[0] = p[1]
def p_for_start(p):
    '''
    FOR_START : FOR LPAREN IDF IN for_int COLON
    '''
    quadruple.append(['=', p[5], None, p[3]])
    ADRStack.append([len(quadruple), "FORtest"])
    p[0] = p[3]


def p_for_int(p):
    '''
    for_int : IDF
            | integer_value
    '''
    p[0]=p[1]

def p_error(p):
    if p:
        print("Syntax error at token "+p.type+" Line : "+str(p.lineno)+" Col : "+str(find_column(p.lexpos)))
        parser.errok()
    else:
        print("Syntax error at EOF")

def find_column(lexpos):
    '''
    find column , used to spot error's column
    '''
    global codeSourceText
    line_start = codeSourceText.rfind('\n', 0, lexpos) + 1
    return (lexpos - line_start) + 1

parser = yacc.yacc()
codeSourceText += "\n"
parser.parse(codeSourceText)



def generateAssembleyCode(quadruple):
    '''
    for each quadruple : transalte a quadruple into an assembley line code
    '''
    toPrint = []
    toPrint.append("Section .DATA\n")
    for name, value in SymbolsTable._symbols.items():
        toPrint.append(name+" "+declarationTypeVar(value.size))
    toPrint.append("\nSection .START\n")

    sectionStart = []
    labels = []
    i = 0
    label_counter = 1
    for quad in quadruple:
        if quad[0] == "=":
            if not variableType(quad[1]) and type(quad[1]) is str:
                sectionStart.append([i, "MOV {}, '{}'".format(variableType(quad[3]), quad[1])])
            else:
                sectionStart.append([i, "MOV {}, {}".format(variableType(quad[3]), variableType(quad[1]))])
        elif quad[0] in ["+", "-", "/", "*"]:
            if quad[1] == quad[3]:
                sectionStart.append([i, "MOV RAX, {}".format(variableType(quad[3]))])
                if quad[2] == 1:
                    if quad[0] == "-":
                        sectionStart.append([i, "DEC RAX"])
                    elif quad[0] == "+":
                        sectionStart.append([i, "INC RAX"])
                else:
                    sectionStart.append([i, "{} RAX, {}".format(operationToToken(quad[0]), variableType(quad[2]))])
                sectionStart.append([i, "MOV {}, RAX".format(variableType(quad[3]))])
            else:
                sectionStart.append([i, "MOV {}, {}".format(variableType(quad[3]), quad[1])])
                sectionStart.append([i, "{} {}, {}".format(operationToToken(quad[0]), variableType(quad[3]), variableType(quad[2]))])
        elif quad[0] in ["!=", "==", ">=", "<=", "<", ">"]:
            sectionStart.append([i, "MOV {}, {}".format(variableType(quad[3]), variableType(quad[1]))])
            sectionStart.append([i, "SUB {}, {}".format(variableType(quad[3]), variableType(quad[2]))])
            sectionStart.append([i, "CMP {}, 0".format(variableType(quad[3]))])
            sectionStart.append([i, "{} .labelLocation_{}".format(comparToJump(quad[0]), label_counter)])
            sectionStart.append([i, "MOV {}, 0".format(variableType(quad[3]))])
            sectionStart.append([i, "JMP .labelLocation_{}".format(label_counter+1)])
            sectionStart.append([i, ".labelLocation_{}".format(label_counter)])
            sectionStart.append([i, "MOV {}, 1".format(variableType(quad[3]))])
            sectionStart.append([i, ".labelLocation_{}".format(label_counter+1)])
            label_counter += 2
        elif quad[0] in ["AND", "OR"]:
            sectionStart.append([i, "MOV {}, {}".format(variableType(quad[3]), variableType(quad[1]))])
            sectionStart.append([i, "{} {}, {}".format(quad[0].lower(), variableType(quad[3]), variableType(quad[2]))])
        elif quad[0] == "BZ":
            sectionStart.append([i, "CMP {}, 0".format(variableType(quad[2]))])
            sectionStart.append([i, "{} .labelLocation_{}".format(jumpQuad(quad[0]), label_counter)])
            labels.append([quad[1], label_counter])
            label_counter += 1
        elif quad[0] == "BGE":
            sectionStart.append([i, "CMP {}, {}".format(variableType(quad[2]), variableType(quad[3]))])
            sectionStart.append([i, "{} .labelLocation_{}".format(jumpQuad(quad[0]), label_counter)])
            labels.append([quad[1], label_counter])
            label_counter += 1
        elif quad[0] == "BR":
            sectionStart.append([i, "JMP .labelLocation_{}".format(label_counter)])
            labels.append([quad[1], label_counter])
            label_counter += 1
        i += 1
    sectionStart.append([sectionStart[-1][0]+1, "END"])

    for inst in sectionStart:
        to_delete = []
        for k, label in enumerate(labels):
            if label[0] == inst[0]:
                toPrint.append(".labelLocation_{}".format(label[1]))
                to_delete.append(k)
        labels = [j for i, j in enumerate(labels) if i not in to_delete]

        if inst[1] != "END":
            toPrint.append(inst[1])

    return toPrint


def variableType(var):
    '''
    takes a variable as an input and returns its type to use in a register
    '''
    if not isinstance(var, str):
        return var
    if var == "TRUE":
        return "1"
    if var == "FALSE":
        return "0"
    if "TEMPORARY_" in var:
        return "r"+(var.replace("TEMPORARY_", ""))
    if var[-1] == "]":
        m = re.search(r"\[(.*)\]", var)
        t = m.group(1)
        m = re.search(r"(.*)\[.*\]", var)
        var = m.group(1)+"+"+t
    return "["+var+"]"

def declarationTypeVar(size):
    '''
    Used to declare a variable and it's type in ASSM
    '''
    return "DB" if size==1 else ".space {}".format(size)

def comparToJump(comparator):
    '''
    takes a sign as input and converts it into an adequat jump
    '''
    dictio = {"!=":"JNZ", "==":"JZ", ">=":"JGE", "<=":"JLE", ">":"JG", "<":"JL"}
    return dictio[comparator]
def operationToToken(oper):
    '''
    takes an operation sign as input and returns the adequat token
    '''
    dictio = {"+":"ADD", "-":"SUB", "*":"MUL", "/":"DIV","%":"MOD"}
    return dictio[oper]


def jumpQuad(jump):
    if jump == "BZ":
        return "JZ"
    elif jump == "BGE":
        return "JGE"





quadruple.append(["END", "", "", ""])


print("===QUADRUPLETS :===")
for quad in quadruple:
    print(quad)
print("================================")
print("================================\n\n")


jj=0
try :
    for a in generateAssembleyCode(quadruple):
        print(a)
        jj+=1
except :
    pass

print("================================")
print("================================\n\n")
print("===Table des symbols :===\n\n")
print("NAME   ||TYPE   ||CATEGORY   ||SIZE   ||INITIALIZED")
for symbol in SymbolsTable._symbols.values():
    print(symbol)