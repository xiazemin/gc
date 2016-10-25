// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"go/token"
)

type parser struct {
	c             token.Token
	errHandler    func(ofs int, msg string, args ...interface{})
	l             *lexer
	loophack      bool
	loophackStack []bool
	ofs           int
	syntaxError   func()
}

func newParser(l *lexer) *parser {
	return &parser{
		l: l,
	}
}

func (p *parser) init(l *lexer) {
	p.l = l
	p.loophack = false
	p.loophackStack = p.loophackStack[:0]
}

func (p *parser) err(ofs int, msg string, args ...interface{}) {
	if p.errHandler != nil {
		p.errHandler(ofs, msg, args...)
		return
	}

	p.l.err(ofs, msg, args...)
}

func (p *parser) n() token.Token {
	switch p.ofs, p.c = p.l.scan(); p.c {
	case token.FOR, token.IF, token.SELECT, token.SWITCH:
		p.loophack = true
	case token.LPAREN, token.LBRACK:
		if p.loophack || len(p.loophackStack) != 0 {
			p.loophackStack = append(p.loophackStack, p.loophack)
			p.loophack = false
		}
	case token.RPAREN, token.RBRACK:
		if n := len(p.loophackStack); n != 0 {
			p.loophack = p.loophackStack[n-1]
			p.loophackStack = p.loophackStack[:n-1]
		}
	case token.LBRACE:
		if p.loophack {
			p.c = tokenBODY
			p.loophack = false
		}
	}
	return p.c
}

func (p *parser) opt(tok token.Token) bool {
	if p.c == tok {
		p.n()
		return true
	}

	return false
}

func (p *parser) must(tok token.Token) {
	if p.c != tok {
		p.syntaxError()
	}
	p.n()
}

func (p *parser) must2(toks ...token.Token) {
	for _, tok := range toks {
		p.must(tok)
	}
}

func (p *parser) not2(toks ...token.Token) bool {
	for _, tok := range toks {
		if p.c == tok {
			return false
		}
	}
	return true
}

// importSpec:
// 	'.' STRING
// |	IDENT STRING
// |	STRING
func (p *parser) importSpec() {
	if p.c == token.IDENT || p.c == token.PERIOD {
		p.n()
	}
	p.must(token.STRING)
}

// importSpecList:
// 	importSpec
// |	importSpecList ';' importSpec
func (p *parser) importSpecList() {
	for p.importSpec(); p.opt(token.SEMICOLON) && p.c != token.RPAREN; {
		p.importSpec()
	}
}

// imports:
// |	imports "import" '(' ')' ';'
// |	imports "import" '(' importSpecList semiOpt ')' ';'
// |	imports "import" importSpec ';'
func (p *parser) imports() {
	for p.opt(token.IMPORT) {
		switch {
		case p.opt(token.LPAREN):
			if !p.opt(token.RPAREN) {
				p.importSpecList()
				p.must(token.RPAREN)
			}
		default:
			p.importSpec()
		}
		p.must(token.SEMICOLON)
	}
}

// identList:
// 	IDENT
// |	identList ',' IDENT
func (p *parser) identList() {
	for p.must(token.IDENT); p.opt(token.COMMA); {
		p.must(token.IDENT)
	}
}

// compLitExpr:
// 	'{' bracedKeyValList '}'
// |	expr
func (p *parser) compLitExpr() {
	if p.opt(token.LBRACE) {
		p.bracedKeyValList()
		p.must(token.RBRACE)
		return
	}

	p.expr()
}

// keyVal:
// 	compLitExpr
// |	compLitExpr ':' compLitExpr
func (p *parser) keyVal() {
	p.compLitExpr()
	if p.opt(token.COLON) {
		p.compLitExpr()
	}
}

// keyValList:
// 	keyVal
// |	keyValList ',' keyVal
func (p *parser) keyValList() {
	for p.keyVal(); p.opt(token.COMMA) && p.c != token.RBRACE; {
		p.keyVal()
	}
}

// bracedKeyValList:
// |	keyValList commaOpt
func (p *parser) bracedKeyValList() {
	if p.c != token.RBRACE {
		p.keyValList()
	}
}

// exprOrType:
// 	expr
// |	nonExprType %prec _PreferToRightParen
func (p *parser) exprOrType() {
more:
	var fix bool
	switch p.c {
	case token.IDENT, token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING,
		token.NOT, token.AND, token.ADD, token.SUB, token.XOR, token.LPAREN:
		p.expr()
	case token.ARROW:
		switch p.n() {
		case token.CHAN:
			p.n()
			p.typ()
		default:
			p.expr()
		}
	case token.MUL:
		p.n()
		goto more
	case token.CHAN, token.INTERFACE, token.MAP, token.STRUCT, token.LBRACK:
		p.otherType()
		switch p.c {
		case token.LBRACE, token.LPAREN:
			p.primaryExpr2()
		}
	case token.FUNC:
		p.fnType()
		switch p.c {
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			p.n()
			p.stmtList()
			p.loophack = fix
			p.must(token.RBRACE)
		}
	default:
		p.syntaxError()
	}
}

// exprOrTypeList:
// 	exprOrType
// |	exprOrTypeList ',' exprOrType
func (p *parser) exprOrTypeList() {
	for p.exprOrType(); p.opt(token.COMMA) && p.not2(token.RPAREN, token.ELLIPSIS); {
		p.exprOrType()
	}
}

// exprOpt:
// |	expr
func (p *parser) exprOpt() (isExprPresent bool) {
	if p.c == token.COLON || p.c == token.RBRACK {
		return false
	}

	p.expr()
	return true
}

// primaryExpr:
// 	'(' exprOrType ')'
// |	IDENT %prec _NotParen
// |	convType '(' expr commaOpt ')'
// |	fnType lbrace stmtList '}'
// |	literal
// |	otherType lbrace bracedKeyValList '}'
// |	primaryExpr '(' ')'
// |	primaryExpr '(' exprOrTypeList "..." commaOpt ')'
// |	primaryExpr '(' exprOrTypeList commaOpt ')'
// |	primaryExpr '.' '(' "type" ')'
// |	primaryExpr '.' '(' exprOrType ')'
// |	primaryExpr '.' IDENT
// |	primaryExpr '[' expr ']'
// |	primaryExpr '[' exprOpt ':' exprOpt ':' exprOpt ']'
// |	primaryExpr '[' exprOpt ':' exprOpt ']'
// |	primaryExpr '{' bracedKeyValList '}'
func (p *parser) primaryExpr() (isLabel bool) {
	var fix bool
	switch p.c {
	case token.LPAREN:
		p.n()
		p.exprOrType()
		p.must(token.RPAREN)
	case token.IDENT:
		if p.n() == token.COLON {
			return true
		}
	case token.FUNC:
		p.fnType()
		switch p.c {
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			p.n()
			p.stmtList()
			p.loophack = fix
			p.must(token.RBRACE)
		default:
			p.syntaxError()
		}
	case token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING:
		p.n()
	case token.CHAN, token.INTERFACE, token.MAP, token.STRUCT, token.LBRACK:
		p.otherType()
		switch p.c {
		case token.LPAREN:
			p.n()
			p.expr()
			p.opt(token.COMMA)
			p.must(token.RPAREN)
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			p.n()
			p.bracedKeyValList()
			p.loophack = fix
			p.must(token.RBRACE)
		default:
			p.syntaxError()
		}
	default:
		p.syntaxError()
	}
	p.primaryExpr2()
	return false
}

func (p *parser) primaryExpr2() {
	for {
		switch p.c {
		case token.LPAREN:
			p.n()
			if p.opt(token.RPAREN) {
				break
			}

			p.exprOrTypeList()
			_ = p.opt(token.ELLIPSIS) && p.opt(token.COMMA) //TODOOK
			p.must(token.RPAREN)
		case token.PERIOD:
			p.n()
			if p.opt(token.LPAREN) {
				if !p.opt(token.TYPE) {
					p.exprOrType()
				}
				p.must(token.RPAREN)
				break
			}

			p.must(token.IDENT)
		case token.LBRACK:
			p.n()
			if !p.exprOpt() && p.c == token.RBRACK {
				p.syntaxError()
				break
			}

			if p.opt(token.COLON) {
				p.exprOpt()
				if p.opt(token.COLON) {
					p.exprOpt()
				}
			}
			p.must(token.RBRACK)
		case token.LBRACE:
			p.n()
			p.bracedKeyValList()
			p.must(token.RBRACE)
		default:
			return
		}
	}
}

// unaryExpr:
// 	"<-" unaryExpr
// |	'!' unaryExpr
// |	'&' unaryExpr
// |	'*' unaryExpr
// |	'+' unaryExpr
// |	'-' unaryExpr
// |	'^' unaryExpr
// |	primaryExpr
func (p *parser) unaryExpr() (isLabel bool) {
	isLabel = true
	for {
		switch p.c {
		case token.ARROW, token.NOT, token.AND, token.MUL, token.ADD, token.SUB,
			token.XOR:
			isLabel = false
			p.n()
		default:
			return p.primaryExpr() && isLabel
		}
	}
}

// expr:
// 	expr "!=" expr
// |	expr "&&" expr
// |	expr "&^" expr
// |	expr "<-" expr
// |	expr "<<" expr
// |	expr "<=" expr
// |	expr "==" expr
// |	expr ">=" expr
// |	expr ">>" expr
// |	expr "||" expr
// |	expr '%' expr
// |	expr '&' expr
// |	expr '*' expr
// |	expr '+' expr
// |	expr '-' expr
// |	expr '/' expr
// |	expr '<' expr
// |	expr '>' expr
// |	expr '^' expr
// |	expr '|' expr
// |	unaryExpr
func (p *parser) expr() (isLabel bool) {
	if p.unaryExpr() {
		return true
	}

	for {
		switch p.c {
		case token.NEQ, token.LAND, token.AND_NOT, token.ARROW, token.SHL,
			token.LEQ, token.EQL, token.GEQ, token.SHR, token.LOR, token.REM,
			token.AND, token.MUL, token.ADD, token.SUB, token.QUO, token.LSS,
			token.GTR, token.XOR, token.OR:
			p.n()
			p.expr()
		default:
			return false
		}
	}
}

// exprList:
// 	expr
// |	exprList ',' expr
func (p *parser) exprList() {
	for p.expr(); p.opt(token.COMMA); {
		p.expr()
	}
}

// constSpec:
// 	identList
// |	identList '=' exprList
// |	identList typ
// |	identList typ '=' exprList
func (p *parser) constSpec() {
	p.identList()
	switch p.c {
	case token.RPAREN, token.SEMICOLON:
		return
	case token.ASSIGN:
		p.n()
		p.exprList()
		return
	}

	p.typ()
	if p.opt(token.ASSIGN) {
		p.exprList()
	}
}

// constSpecList:
// 	constSpec
// |	constSpecList ';' constSpec
func (p *parser) constSpecList() {
	for p.constSpec(); p.opt(token.SEMICOLON) && p.c != token.RPAREN; {
		p.constSpec()
	}
}

// fieldDecl:
// 	'*' embededName literalOpt
// |	identList typ literalOpt
// |	embededName literalOpt
func (p *parser) fieldDecl() {
	if p.c == token.MUL {
		p.n()
		p.qualifiedIdent()
		p.opt(token.STRING)
		return
	}

	p.must(token.IDENT)
	switch p.c {
	case token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING:
		p.n()
		return
	case token.SEMICOLON, token.RBRACE:
		return
	case token.PERIOD:
		p.n()
		p.must(token.IDENT)
	case token.COMMA:
		p.n()
		p.identList()
		fallthrough
	default:
		p.typ()
	}
	switch p.c {
	case token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING:
		p.n()
	}
}

// fieldDeclList:
// 	fieldDecl
// |	fieldDeclList ';' fieldDecl
func (p *parser) fieldDeclList() {
	for p.fieldDecl(); p.opt(token.SEMICOLON) && p.c != token.RBRACE; {
		p.fieldDecl()
	}
}

// interfaceDecl:
// 	IDENT '(' paramTypeListCommaOptOpt ')' result
// |	embededName
func (p *parser) interfaceDecl() {
	p.must(token.IDENT)
	switch p.c {
	case token.LPAREN:
		p.n()
		p.paramTypeListCommaOptOpt()
		p.must(token.RPAREN)
		p.result()
	case token.PERIOD:
		p.n()
		p.must(token.IDENT)
	case token.SEMICOLON, token.RBRACE:
		// nop
	default:
		p.syntaxError()
	}
}

// interfaceDeclList:
// 	interfaceDecl
// |	interfaceDeclList ';' interfaceDecl
func (p *parser) interfaceDeclList() {
	for p.interfaceDecl(); p.opt(token.SEMICOLON) && p.c != token.RBRACE; {
		p.interfaceDecl()
	}
}

// otherType:
// 	"chan" "<-" typ
// |	"chan" '(' typ ')'
// |	"chan" qualifiedIdent
// |	"chan" fnType
// |	"chan" otherType
// |	"chan" ptrType
// |	"interface" lbrace '}'
// |	"interface" lbrace interfaceDeclList semiOpt '}'
// |	"map" '[' typ ']' typ
// |	"struct" lbrace '}'
// |	"struct" lbrace fieldDeclList semiOpt '}'
// |	'[' "..." ']' typ
// |	'[' exprOpt ']' typ
func (p *parser) otherType() {
	var fix bool
	switch p.c {
	case token.CHAN:
		switch p.n() {
		case token.ARROW:
			p.n()
			p.typ()
		case token.LPAREN:
			p.n()
			p.typ()
			p.must(token.RPAREN)
		case token.IDENT:
			p.qualifiedIdent()
		case token.FUNC:
			p.fnType()
		case token.MUL:
			p.ptrType()
		default:
			p.otherType()
		}
	case token.INTERFACE:
		switch p.n() {
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			if p.n() != token.RBRACE {
				p.interfaceDeclList()
			}
			p.loophack = fix
			p.must(token.RBRACE)
		default:
			p.syntaxError()
		}
	case token.MAP:
		p.n()
		p.must(token.LBRACK)
		p.typ()
		p.must(token.RBRACK)
		p.typ()
	case token.STRUCT:
		switch p.n() {
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			if p.n() != token.RBRACE {
				p.fieldDeclList()
			}
			p.loophack = fix
			p.must(token.RBRACE)
		default:
			p.syntaxError()
		}
	case token.LBRACK:
		p.n()
		if !p.opt(token.ELLIPSIS) {
			p.exprOpt()
		}
		p.must(token.RBRACK)
		p.typ()
	default:
		p.syntaxError()
	}
}

// qualifiedIdent:
// 	IDENT %prec _NotParen
// |	IDENT '.' IDENT
func (p *parser) qualifiedIdent() {
	p.must(token.IDENT)
	if p.opt(token.PERIOD) {
		p.must(token.IDENT)
	}
}

// ptrType:
// 	'*' typ
func (p *parser) ptrType() {
	for p.opt(token.MUL) {
	}
	p.typ()
}

// fnType:
// 	"func" '(' paramTypeListCommaOptOpt ')' result
func (p *parser) fnType() {
	p.n() // "func"
	p.must(token.LPAREN)
	p.paramTypeListCommaOptOpt()
	p.must(token.RPAREN)
	p.result()
}

// rxChanType:
// 	"<-" "chan" typ
func (p *parser) rxChanType() {
	p.n() // "<-"
	p.must(token.CHAN)
	p.typ()
}

// typ:
// 	'(' typ ')'
// |	qualifiedIdent
// |	fnType
// |	otherType
// |	ptrType
// |	rxChanType
func (p *parser) typ() {
	switch p.c {
	case token.LPAREN:
		p.n()
		p.typ()
		p.must(token.RPAREN)
	case token.IDENT:
		p.qualifiedIdent()
	case token.FUNC:
		p.fnType()
	case token.CHAN, token.INTERFACE, token.MAP, token.STRUCT, token.LBRACK:
		p.otherType()
	case token.MUL:
		p.ptrType()
	case token.ARROW:
		p.rxChanType()
	default:
		p.syntaxError()
	}
}

// typeSpec:
// 	IDENT typ
func (p *parser) typeSpec() {
	p.must(token.IDENT)
	p.typ()
}

// typeSpecList:
// 	typeSpec
// |	typeSpecList ';' typeSpec
func (p *parser) typeSpecList() {
	for p.typeSpec(); p.opt(token.SEMICOLON) && p.c != token.RPAREN; {
		p.typeSpec()
	}
}

// varSpec:
// 	identList '=' exprList
// |	identList typ
// |	identList typ '=' exprList
func (p *parser) varSpec() {
	p.identList()
	if p.opt(token.ASSIGN) {
		p.exprList()
		return
	}

	p.typ()
	if p.opt(token.ASSIGN) {
		p.exprList()
	}
}

// varSpecList:
// 	varSpec
// |	varSpecList ';' varSpec
func (p *parser) varSpecList() {
	for p.varSpec(); p.opt(token.SEMICOLON) && p.c != token.RPAREN; {
		p.varSpec()
	}
}

// commonDecl:
// 	"const" '(' ')'
// |	"const" '(' constSpec ';' constSpecList semiOpt ')'
// |	"const" '(' constSpec semiOpt ')'
// |	"const" constSpec
// |	"type" '(' ')'
// |	"type" '(' typeSpecList semiOpt ')'
// |	"type" typeSpec
// |	"var" '(' ')'
// |	"var" '(' varSpecList semiOpt ')'
// |	"var" varSpec
func (p *parser) commonDecl() {
	switch p.c {
	case token.CONST:
		p.n()
		switch {
		case p.opt(token.LPAREN):
			if !p.opt(token.RPAREN) {
				p.constSpecList()
				p.must(token.RPAREN)
			}
		default:
			p.constSpec()
		}
	case token.TYPE:
		p.n()
		switch {
		case p.opt(token.LPAREN):
			if !p.opt(token.RPAREN) {
				p.typeSpecList()
				p.must(token.RPAREN)
			}
		default:
			p.typeSpec()
		}
	case token.VAR:
		p.n()
		switch {
		case p.opt(token.LPAREN):
			if !p.opt(token.RPAREN) {
				p.varSpecList()
				p.must(token.RPAREN)
			}
		default:
			p.varSpec()
		}
	}
}

// paramType:
// 	IDENT dddType
// |	IDENT typ
// |	dddType
// |	typ
func (p *parser) paramType() {
	switch p.c {
	case token.IDENT:
		switch p.n() {
		case token.RPAREN:
			// nop
		case token.COMMA:
			// nop
		case token.PERIOD:
			p.n()
			p.must(token.IDENT)
		case token.ELLIPSIS:
			p.n()
			p.typ()
		default:
			p.typ()
		}
	case token.ELLIPSIS:
		p.n()
		p.typ()
	default:
		p.typ()
	}
}

// paramTypeList:
// 	paramType
// |	paramTypeList ',' paramType
func (p *parser) paramTypeList() {
	for p.paramType(); p.opt(token.COMMA) && p.c != token.RPAREN; {
		p.paramType()
	}
}

// paramTypeListCommaOptOpt:
// |	paramTypeList commaOpt
func (p *parser) paramTypeListCommaOptOpt() {
	if p.c != token.RPAREN {
		p.paramTypeList()
	}
}

// result:
// 	%prec _NotParen
// |	'(' paramTypeListCommaOptOpt ')'
// |	qualifiedIdent
// |	fnType
// |	otherType
// |	ptrType
// |	rxChanType
func (p *parser) result() {
	switch p.c {
	case token.LBRACE, token.RPAREN, token.SEMICOLON, token.COMMA, tokenBODY:
		// nop
	case token.LPAREN:
		p.n()
		p.paramTypeListCommaOptOpt()
		p.must(token.RPAREN)
	case token.IDENT:
		p.qualifiedIdent()
	case token.FUNC:
		p.fnType()
	case token.CHAN, token.INTERFACE, token.MAP, token.STRUCT, token.LBRACK:
		p.otherType()
	case token.MUL:
		p.ptrType()
	case token.ARROW:
		p.rxChanType()
	default:
		p.syntaxError()
	}
}

// simpleStmt:
// 	expr
// |	expr "%=" expr
// |	expr "&=" expr
// |	expr "&^=" expr
// |	expr "*=" expr
// |	expr "++"
// |	expr "+=" expr
// |	expr "--"
// |	expr "-=" expr
// |	expr "/=" expr
// |	expr "<<=" expr
// |	expr ">>=" expr
// |	expr "^=" expr
// |	expr "|=" expr
// |	exprList ":=" exprList
// |	exprList '=' exprList
func (p *parser) simpleStmt(acceptRange bool) (isLabel, isRange bool) {
	first := true
	if p.expr() {
		return true, false
	}

more:
	switch p.c {
	case token.REM_ASSIGN, token.AND_ASSIGN, token.AND_NOT_ASSIGN, token.MUL_ASSIGN,
		token.ADD_ASSIGN, token.SUB_ASSIGN, token.QUO_ASSIGN, token.SHL_ASSIGN,
		token.SHR_ASSIGN, token.XOR_ASSIGN, token.OR_ASSIGN:
		p.n()
		p.expr()
	case token.INC, token.DEC:
		p.n()
		return false, false
	case token.COMMA:
		if !first {
			p.syntaxError()
			break
		}

		first = false
		p.n()
		p.exprList()
		goto more
	case token.DEFINE, token.ASSIGN:
		p.n()
		if acceptRange && p.opt(token.RANGE) {
			isRange = true
		}
		p.exprList()
		return false, isRange
	}
	return false, false
}

// simpleStmtOpt:
// |	simpleStmt
func (p *parser) simpleStmtOpt(acceptRange bool) (isRange bool) {
	if p.c == token.SEMICOLON || p.c == tokenBODY {
		return false
	}

	_, isRange = p.simpleStmt(acceptRange)
	return isRange
}

// ifHeader:
// 	simpleStmtOpt
// |	simpleStmtOpt ';' simpleStmtOpt
func (p *parser) ifHeader() {
	p.simpleStmtOpt(false)
	if p.opt(token.SEMICOLON) {
		p.simpleStmtOpt(false)
	}
}

// loopBody:
// 	BODY stmtList '}'
func (p *parser) loopBody() {
	p.must(tokenBODY)
	p.stmtList()
	p.must(token.RBRACE)
}

// elseIfList:
// |	elseIfList "else" "if" ifHeader loopBody
func (p *parser) elseIfList() (isElse bool) {
	for p.opt(token.ELSE) {
		if p.opt(token.IF) {
			p.ifHeader()
			p.loopBody()
			continue
		}

		return true // Consumed "else", "if" does not follow.
	}
	return false
}

// compoundStmt:
// 	'{' stmtList '}'
func (p *parser) compoundStmt() {
	p.must(token.LBRACE)
	p.stmtList()
	p.must(token.RBRACE)
}

// caseBlockList:
// |	caseBlockList "case" exprOrTypeList ":=" expr ':' stmtList
// |	caseBlockList "case" exprOrTypeList ':' stmtList
// |	caseBlockList "case" exprOrTypeList '=' expr ':' stmtList
// |	caseBlockList "default" ':' stmtList
func (p *parser) caseBlockList() {
	for {
		switch p.c {
		case token.CASE:
			p.n()
			p.exprOrTypeList()
			if p.c == token.DEFINE || p.c == token.ASSIGN {
				p.n()
				p.expr()
			}
			p.must(token.COLON)
		case token.DEFAULT:
			p.n()
			p.must(token.COLON)
			p.stmtList()
		default:
			return
		}

		p.stmtList()
	}
}

// stmt:
// |	"break" identOpt
// |	"continue" identOpt
// |	"defer" primaryExpr
// |	"fallthrough"
// |	"for" "range" expr loopBody
// |	"for" exprList ":=" "range" expr loopBody
// |	"for" exprList '=' "range" expr loopBody
// |	"for" simpleStmtOpt ';' simpleStmtOpt ';' simpleStmtOpt loopBody
// |	"for" simpleStmtOpt loopBody
// |	"go" primaryExpr
// |	"goto" IDENT
// |	"if" ifHeader loopBody elseIfList
// |	"if" ifHeader loopBody elseIfList "else" compoundStmt
// |	"return"
// |	"return" exprList
// |	"select" BODY caseBlockList '}'
// |	"switch" ifHeader BODY caseBlockList '}'
// |	IDENT ':' stmt
// |	commonDecl
// |	compoundStmt
// |	simpleStmt
func (p *parser) stmt() {
more:
	switch p.c {
	case token.SEMICOLON, token.RBRACE, token.CASE, token.DEFAULT:
		// nop
	case token.BREAK, token.CONTINUE:
		p.n()
		p.opt(token.IDENT)
	case token.DEFER, token.GO:
		p.n()
		p.expr()
	case token.FALLTHROUGH:
		p.n()
	case token.FOR:
		switch p.n() {
		case token.RANGE:
			p.n()
			p.expr()
		default:
			if p.simpleStmtOpt(true) { // range
				break
			}

			if p.opt(token.SEMICOLON) {
				p.simpleStmtOpt(false)
				p.must(token.SEMICOLON)
				p.simpleStmtOpt(false)
			}
		}
		p.loopBody()
	case token.GOTO:
		p.n()
		p.must(token.IDENT)
	case token.IF:
		p.n()
		p.ifHeader()
		p.loopBody()
		if p.elseIfList() {
			p.compoundStmt()
		}
	case token.RETURN:
		p.n()
		if p.c != token.SEMICOLON {
			p.exprList()
		}
	case token.SELECT:
		p.n()
		p.must(tokenBODY)
		p.caseBlockList()
		p.must(token.RBRACE)
	case token.SWITCH:
		p.n()
		p.ifHeader()
		p.must(tokenBODY)
		p.caseBlockList()
		p.must(token.RBRACE)
	case token.CONST, token.TYPE, token.VAR:
		p.commonDecl()
	case token.LBRACE:
		p.compoundStmt()
	default:
		if isLabel, _ := p.simpleStmt(false); isLabel && p.opt(token.COLON) {
			goto more
		}
	}
}

// stmtList:
// 	stmt
// |	stmtList ';' stmt
func (p *parser) stmtList() {
	for p.stmt(); p.opt(token.SEMICOLON) && p.not2(token.RBRACE, token.CASE, token.DEFAULT); {
		p.stmt()
	}
}

// fnBody:
// |	'{' stmtList '}'
func (p *parser) fnBody() {
	if p.opt(token.LBRACE) {
		p.stmtList()
		p.must(token.RBRACE)
	}
}

// topLevelDeclList:
// |	topLevelDeclList "func" '(' paramTypeListCommaOptOpt ')' IDENT '(' paramTypeListCommaOptOpt ')' result fnBody ';'
// |	topLevelDeclList "func" IDENT '(' paramTypeListCommaOptOpt ')' result fnBody ';'
// |	topLevelDeclList commonDecl ';'
func (p *parser) topLevelDeclList() {
	for p.c != token.EOF {
		switch p.c {
		case token.FUNC:
			p.n()
			if p.opt(token.LPAREN) {
				p.paramTypeListCommaOptOpt()
				p.must(token.RPAREN)
			}
			p.must2(token.IDENT, token.LPAREN)
			p.paramTypeListCommaOptOpt()
			p.must(token.RPAREN)
			p.result()
			p.fnBody()
		case token.CONST, token.TYPE, token.VAR:
			p.commonDecl()
		default:
			p.syntaxError()
		}
		p.must(token.SEMICOLON)
	}
}

// file:
// 	"package" IDENT ';' imports topLevelDeclList
func (p *parser) file() {
	p.n()
	p.must2(token.PACKAGE, token.IDENT, token.SEMICOLON)
	p.imports()
	p.topLevelDeclList()
}
