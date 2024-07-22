%token LEFT_PAREN "(" RIGHT_PAREN ")"
%token LEFT_CURLY "{" RIGHT_CURLY "}"
%token LEFT_SQUARE "[" RIGHT_SQUARE "]"

%token SEMICOLON ";" COMMA ","
%token PLUS "+" MINUS "-" SLASH "/" STAR "*"
%token EQUALS "="
%token LAMBDA "λ" ARROW "->"
%token BAR "|"
%token DOT "."

%token LET MATCH IN

%token <string> HOLE
%token <string> IDENTIFIER
%token <string> SYMBOL
%token <Q.t> NUMBER

%token EOF

%nonassoc below_SEMICOLON
%nonassoc SEMICOLON

%left "+" "-" "/" "*" "="

%{
    open! ContainersLabels

    open Ast_type
%}

%type <Ast_type.expr> base single expression

%start <Ast_type.expr> main

%%

main: e = expression; EOF { e }

expression:
  | seq = sequence { let pos, seq = seq in pos, Seq seq }

sequence:
  | e = single
    %prec below_SEMICOLON
    { ($startpos, $endpos), [e] }
  | e = single; SEMICOLON; s = sequence
    { let _, s = s in ($startpos, $endpos), e :: s }

single:
  | LET; ","?; b = separated_nonempty_list(",", binding); IN; e = expression
    { ($startpos, $endpos), Let { bindings = b; body = e } }
  | "λ"; p = pattern+; "->"; e = expression
    { ($startpos, $endpos), Lambda { name = None; params = p; body = e } }
  | MATCH; e = expression; IN; "{"; "|"?; c = separated_nonempty_list("|", match_clause); "}"
    { ($startpos, $endpos), Match (e, c) }
  | "λ"; MATCH; "{"; "|"?; c = separated_nonempty_list("|", match_clause); "}"
    { let id = Id.of_string "_" in
      ($startpos, $endpos),
      Lambda {
          name = None;
          params = [Bind id]; 
          body = ($startpos, $endpos), Match ((($startpos, $endpos), Var id), c)
        }
    }
  | l = single; o = operator; r = single
    { ($startpos, $endpos), Appl (o, [l; r]) }
  | b = base; rest = base+
    { ($startpos, $endpos), (Appl (b, rest)) }
  | b = base
    { b }

binding:
  | p = pattern; "="; e = expression
	{ p, e }
  | i = identifier; p = pattern+; "="; e = expression
    { Bind i, (($startpos, $endpos), Lambda { name = Some i; params = p; body = e }) }

match_clause:
  | p = pattern; "->"; e = expression
    { p, e }

pattern:
  | i = identifier
    { Bind i }
  | h = HOLE
    { Hole h }
  | l = literal(pattern)
    { Literal l }

identifier:
  | i = IDENTIFIER { Id.of_string i }

%inline literal(elem):
  | "["; l = separated_list(",", elem); "]"
    { Literal.List l }
  | "{"; l = separated_list(",", dict_element(elem)); "}"
    { Literal.Dict l }
  | n = NUMBER
    { Literal.Number n }
  | s = SYMBOL
    { Literal.Symbol (Id.of_string s) }
  | "("; ")"
    { Literal.Unit }

dict_element(elem):
  | i = identifier; "="; e = elem
    { `Bare (i, e) }
  | "."; "["; k = elem; "]"; "="; v = elem
    { `Computed (k, v) }
  | i = identifier;
    { `Single i }

%inline operator:
  | "+" { ($startpos, $endpos), Var (Id.of_string "+") }
  | "-" { ($startpos, $endpos), Var (Id.of_string "-") }
  | "/" { ($startpos, $endpos), Var (Id.of_string "/") }
  | "*" { ($startpos, $endpos), Var (Id.of_string "*") }
  | "=" { ($startpos, $endpos), Var (Id.of_string "=") }

base:
  | "("; e = expression; ")"
    { let _, e = e in ($startpos, $endpos), e }
  | b = base; "."; p = path
    { ($startpos, $endpos), (Get (b, p)) }
  | i = identifier
    { ($startpos, $endpos), Var i }
  | l = literal(expression)
    { ($startpos, $endpos), Literal l }

path:
  | i = identifier
    { ($startpos, $endpos), Literal (Literal.Symbol i) }
  | "["; e = expression; "]"
    { e }
