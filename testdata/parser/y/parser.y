// This source file is a modification of [0].
//
// [0]: https://github.com/golang/go/blob/release-branch.go1.5/src/cmd/compile/internal/gc/go.y

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

%token
	ILLEGAL

	IDENT
	INT
	FLOAT
	IMAG
	CHAR
	STRING

	ADD_ASSIGN	"+="
	AND_ASSIGN	"&="
	AND_NOT		"&^"
	AND_NOT_ASSIGN	"&^="
	ARROW		"<-"
	DEC		"--"
	DEFINE		":="
	ELLIPSIS	"..."
	EQL		"=="
	GEQ		">="
	INC		"++"
	LAND		"&&"
	LEQ		"<="
	LOR		"||"
	MUL_ASSIGN	"*="
	NEQ		"!="
	OR_ASSIGN	"|="
	QUO_ASSIGN	"/="
	REM_ASSIGN	"%="
	SHL		"<<"
	SHL_ASSIGN	"<<="
	SHR		">>"
	SHR_ASSIGN	">>="
	SUB_ASSIGN	"-="
	XOR_ASSIGN	"^="
	 
	BREAK		"break"
	CASE		"case"
	CHAN		"chan"
	CONST		"const"
	CONTINUE	"continue"
	DEFAULT		"default"
	DEFER		"defer"
	ELSE		"else"
	FALLTHROUGH	"fallthrough"
	FOR		"for"
	FUNC		"func"
	GO		"go"
	GOTO		"goto"
	IF		"if"
	IMPORT		"import"
	INTERFACE	"interface"
	MAP		"map"
	PACKAGE		"package"
	RANGE		"range"
	RETURN		"return"
	SELECT		"select"
	STRUCT		"struct"
	SWITCH		"switch"
	TYPE		"type"
	VAR		"var"

	BODY 		"{"

%left	ARROW

%left	LOR
%left	LAND
%left	EQL NEQ LEQ GEQ '<' '>'
%left	'+' '-' '|' '^'
%left	'*' '/' '%' '&' SHL SHR AND_NOT

%left	_NotPackage
%left	PACKAGE

%left	_NotParen
%left	'('

%left	')'
%left	_PreferToRightParen

%%

file:
	package imports xdcl_list

package:
	"package" IDENT ';'

imports:
|	imports import ';'

import:
	"import" import_here
|	"import" '(' import_stmt_list osemi ')'
|	"import" '(' ')'

import_stmt_list:
	import_here
|	import_stmt_list ';' import_here

import_here:
	STRING
|	IDENT STRING
|	'.' STRING

xdcl:
	common_dcl
|	"func" fndcl fnbody

common_dcl:
	"var" vardcl
|	"var" '(' vardcl_list osemi ')'
|	"var" '(' ')'
|	"const" constdcl
|	"const" '(' constdcl osemi ')'
|	"const" '(' constdcl ';' constdcl_list osemi ')'
|	"const" '(' ')'
|	"type" typedcl
|	"type" '(' typedcl_list osemi ')'
|	"type" '(' ')'

vardcl:
	ident_list ntype
|	ident_list ntype '=' expr_list
|	ident_list '=' expr_list

constdcl:
	ident_list ntype '=' expr_list
|	ident_list '=' expr_list

constdcl1:
	constdcl
|	ident_list ntype
|	ident_list

typedcl:
	IDENT ntype

simple_stmt:
	expr
|	expr asop expr
|	expr_list '=' expr_list
|	expr_list ":=" expr_list
|	expr "++"
|	expr "--"

case:
	"case" expr_or_type_list ':'
|	"case" expr_or_type_list '=' expr ':'
|	"case" expr_or_type_list ":=" expr ':'
|	"default" ':'

compound_stmt:
	'{' stmt_list '}'

caseblock:
	case stmt_list // "missing statement after label"

caseblock_list:
|	caseblock_list caseblock

loop_body:
	BODY stmt_list '}'

range_stmt:
	expr_list '=' "range" expr
|	expr_list ":=" "range" expr
|	"range" expr

for_header:
	osimple_stmt ';' osimple_stmt ';' osimple_stmt
|	osimple_stmt
|	range_stmt

for_stmt:
	"for" for_header loop_body

if_header:
	osimple_stmt
|	osimple_stmt ';' osimple_stmt

if_stmt:
	"if" if_header loop_body elseif_list else

elseif:
	"else" "if" if_header loop_body

elseif_list:
|	elseif_list elseif

else:
|	"else" compound_stmt

switch_stmt:
	"switch" if_header BODY caseblock_list '}'

select_stmt:
	"select" BODY caseblock_list '}'

expr:
	uexpr
|	expr "||" expr
|	expr "&&" expr
|	expr "==" expr
|	expr "!=" expr
|	expr '<' expr
|	expr "<=" expr
|	expr ">=" expr
|	expr '>' expr
|	expr '+' expr
|	expr '-' expr
|	expr '|' expr
|	expr '^' expr
|	expr '*' expr
|	expr '/' expr
|	expr '%' expr
|	expr '&' expr
|	expr "&^" expr
|	expr "<<" expr
|	expr ">>" expr
|	expr "<-" expr

uexpr:
	pexpr
|	'*' uexpr
|	'&' uexpr
|	'+' uexpr
|	'-' uexpr
|	'!' uexpr
|	'^' uexpr
|	"<-" uexpr

pseudocall:
	pexpr '(' ')'
|	pexpr '(' expr_or_type_list ocomma ')'
|	pexpr '(' expr_or_type_list "..." ocomma ')'

pexpr_no_paren:
	literal
|	IDENT %prec _NotParen
|	pexpr '.' IDENT
|	pexpr '.' '(' expr_or_type ')'
|	pexpr '.' '(' "type" ')'
|	pexpr '[' expr ']'
|	pexpr '[' oexpr ':' oexpr ']'
|	pexpr '[' oexpr ':' oexpr ':' oexpr ']'
|	pseudocall
|	convtype '(' expr ocomma ')'
|	othertype lbrace braced_keyval_list '}'
|	pexpr_no_paren '{' braced_keyval_list '}'
|	fnliteral

keyval:
	complitexpr ':' complitexpr

complitexpr:
	expr
|	'{' braced_keyval_list '}'

pexpr:
	pexpr_no_paren
|	'(' expr_or_type ')'

expr_or_type:
	expr
|	non_expr_type %prec _PreferToRightParen

lbrace:
	BODY
|	'{'

oident:
|	IDENT

dotdotdot:
	"..." ntype

ntype:
	recvchantype
|	fntype
|	othertype
|	ptrtype
|	dotname
|	'(' ntype ')'

non_expr_type:
	recvchantype
|	fntype
|	othertype
|	'*' non_expr_type

non_recvchantype:
	fntype
|	othertype
|	ptrtype
|	dotname
|	'(' ntype ')'

convtype:
	fntype
|	othertype

fnret_type:
	recvchantype
|	fntype
|	othertype
|	ptrtype
|	dotname

dotname:
	IDENT %prec _NotParen
|	IDENT '.' IDENT

othertype:
	'[' oexpr ']' ntype
|	'[' "..." ']' ntype
|	"chan" non_recvchantype
|	"chan" "<-" ntype
|	"map" '[' ntype ']' ntype
|	structtype
|	interfacetype

ptrtype:
	'*' ntype

recvchantype:
	"<-" "chan" ntype

structtype:
	"struct" lbrace structdcl_list osemi '}'
|	"struct" lbrace '}'

interfacetype:
	"interface" lbrace interfacedcl_list osemi '}'
|	"interface" lbrace '}'

fndcl:
	IDENT '(' oarg_type_list_ocomma ')' fnres
|	'(' oarg_type_list_ocomma ')' IDENT '(' oarg_type_list_ocomma ')' fnres

fntype:
	"func" '(' oarg_type_list_ocomma ')' fnres

fnbody:
|	'{' stmt_list '}'

fnres:
	%prec _NotParen
|	fnret_type
|	'(' oarg_type_list_ocomma ')'

fnliteral:
	fntype lbrace stmt_list '}'

xdcl_list:
|	xdcl_list xdcl ';'

vardcl_list:
	vardcl
|	vardcl_list ';' vardcl

constdcl_list:
	constdcl1
|	constdcl_list ';' constdcl1

typedcl_list:
	typedcl
|	typedcl_list ';' typedcl

structdcl_list:
	structdcl
|	structdcl_list ';' structdcl

interfacedcl_list:
	interfacedcl
|	interfacedcl_list ';' interfacedcl

structdcl:
	ident_list ntype oliteral
|	packname oliteral
|	'*' packname oliteral

packname:
	IDENT
|	IDENT '.' IDENT

interfacedcl:
	IDENT '(' oarg_type_list_ocomma ')' fnres
|	packname

arg_type:
	ntype
|	IDENT ntype
|	IDENT dotdotdot
|	dotdotdot

arg_type_list:
	arg_type
|	arg_type_list ',' arg_type

oarg_type_list_ocomma:
|	arg_type_list ocomma

stmt:
|	compound_stmt
|	common_dcl
|	non_dcl_stmt

non_dcl_stmt:
	simple_stmt
|	for_stmt
|	switch_stmt
|	select_stmt
|	if_stmt
|	IDENT ':' stmt
|	"fallthrough"
|	"break" oident
|	"continue" oident
|	"go" pseudocall
|	"defer" pseudocall
|	"goto" IDENT
|	"return" oexpr_list

stmt_list:
	stmt
|	stmt_list ';' stmt

ident_list:
	IDENT
|	ident_list ',' IDENT

expr_list:
	expr
|	expr_list ',' expr

expr_or_type_list:
	expr_or_type
|	expr_or_type_list ',' expr_or_type

keyval_list:
	keyval
|	complitexpr
|	keyval_list ',' keyval
|	keyval_list ',' complitexpr

braced_keyval_list:
|	keyval_list ocomma

osemi:
|	';'

ocomma:
|	','

oexpr:
|	expr

oexpr_list:
|	expr_list

osimple_stmt:
|	simple_stmt

oliteral:
|	literal

literal:
	INT
|	FLOAT
|	IMAG
|	CHAR
|	STRING

asop:
	"+="
|	"&^="
|	"&="
|	"/="
|	"<<="
|	"%="
|	"*="
|	"|="
|	">>="
|	"-="
|	"^="
