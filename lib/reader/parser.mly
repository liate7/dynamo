%token LEFT_PAREN "(" RIGHT_PAREN ")"
%token LEFT_CURLY "{" RIGHT_CURLY "}"
%token LEFT_SQUARE "[" RIGHT_SQUARE "]"

%token SEMICOLON ";" COMMA ","
%token PLUS "+" MINUS "-" SLASH "/" STAR "*"
%token EQUALS "="
%token LAMBDA "λ" ARROW "->"
%token BAR "|"
%token DOT "."
%token BANG "!"

%token LET MATCH IN
%token RAISED

%token <string> HOLE
%token <string> IDENTIFIER
%token <string> SYMBOL
%token <Q.t> NUMBER

%token EOF

%nonassoc below_SEMICOLON
%nonassoc SEMICOLON

%left "+" "-" "/" "*" "=" "!"

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
    { $loc, [e] }
  | e = single; SEMICOLON; s = sequence
    { let _, s = s in $loc, e :: s }

single:
  | LET; ","?; b = separated_nonempty_list(",", binding); IN; e = expression
    { $loc, Let { bindings = b; body = e } }
  | "λ"; p = pattern+; "->"; e = expression
    { $loc, Lambda { name = None; params = p; body = e } }
  | MATCH; e = expression; IN; "{"; "|"?; c = separated_nonempty_list("|", match_clause); "}"
    { $loc, Match (e, c) }
  | "λ"; MATCH; "{"; "|"?; c = separated_nonempty_list("|", match_clause); "}"
    { let id = Id.of_string "_" in
      $loc,
      Lambda {
          name = None;
          params = [$loc($2), Bind id]; 
          body = $loc, Match (($loc, Var id), c)
        }
    }
  | l = single; o = operator; r = single
    { $loc, Appl (o, [l; r]) }
  | b = base; rest = base+
    { $loc, (Appl (b, rest)) }
  | b = base
    { b }

binding:
  | p = pattern; "="; e = expression
	{ p, e }
  | i = identifier; p = pattern+; "="; e = expression
    { (($startpos(i), $endpos(p)), Bind i),
	  ($loc, Lambda { name = Some i; params = p; body = e }) }

match_clause:
  | p = pattern; "->"; e = expression
    { `Value p, e }
  | RAISED; p = pattern; "->"; e = expression
    { `Catch p, e }

pattern:
  | i = identifier
    { $loc, Bind i }
  | h = HOLE
    { $loc, Hole h }
  | l = literal(pattern)
    { $loc, Literal l }
  | tag = pattern; "!"; value = pattern
    { $loc, Exception (tag, value) }

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
    { `Single ($loc, i) }

%inline operator:
  | "+" { $loc, Var (Id.of_string "+") }
  | "-" { $loc, Var (Id.of_string "-") }
  | "/" { $loc, Var (Id.of_string "/") }
  | "*" { $loc, Var (Id.of_string "*") }
  | "=" { $loc, Var (Id.of_string "=") }
  | "!" { $loc, Var (Id.of_string "!") }

base:
  | "("; e = expression; ")"
    { let _, e = e in $loc, e }
  | b = base; "."; p = path
    { $loc, (Get (b, p)) }
  | i = identifier
    { $loc, Var i }
  | l = literal(expression)
    { $loc, Literal l }

path:
  | i = identifier
    { $loc, Literal (Literal.Symbol i) }
  | "["; e = expression; "]"
    { e }
